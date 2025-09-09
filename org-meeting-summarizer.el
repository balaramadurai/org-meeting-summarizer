;;; org-meeting-summarizer.el --- Record and summarize meetings in Org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Bala

;; Author: Bala <bala@balaramadurai.net>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: org, meetings, summarization, audio
;; URL: https://github.com/yourusername/org-meeting-summarizer
;; License: MIT

;;; Commentary:

;; This package records audio meetings using ffmpeg, summarizes them using the
;; Gemini API, and inserts summaries into Org-mode subtrees. Summaries have bold
;; titles (e.g., **Summary for ...**) and do not fold the subtree.

;;; Code:

(defgroup org-meeting-summarizer nil
  "Customization group for org-meeting-summarizer."
  :group 'org)

(defcustom org-meeting-summarizer-script-path
  (expand-file-name "scripts/summarize_meetings.py" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the summarize_meetings.py script."
  :type 'string
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-api-key ""
  "Gemini API key for summarizing m4a files."
  :type 'string
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-model "gemini-2.5-flash"
  "Gemini model to use for summarization."
  :type 'string
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-retry-delay 60
  "Delay in seconds between retries after hitting Gemini API rate limit."
  :type 'integer
  :group 'org-meeting-summarizer)

(defun org-meeting-summarizer-record-audio-to-m4a (output-file duration)
  "Record audio using ffmpeg and save to OUTPUT-FILE (.m4a) for DURATION seconds.
If DURATION is nil or 0, record until manually stopped with M-x org-meeting-summarizer-stop-recording."
  (interactive "FFile to save recording (e.g., ~/Documents/0Inbox/recording.m4a): \nNDuration in seconds (0 for manual stop): ")
  (let* ((output-file-expanded (expand-file-name output-file))
         (command (if (and duration (> duration 0))
                      (format "ffmpeg -f alsa -i default -t %d -c:a aac %s" duration output-file-expanded)
                    (format "ffmpeg -f alsa -i default -c:a aac %s" output-file-expanded)))
         (process-name "audio-recording")
         (remaining duration))
    (message "Checking output path: '%s'" output-file-expanded)
    (if (file-exists-p output-file-expanded)
        (unless (y-or-n-p (format "File '%s' exists. Overwrite?" output-file-expanded))
          (error "Recording cancelled: File exists"))
      (when (not (file-writable-p (file-name-directory output-file-expanded)))
        (error "Directory '%s' is not writable" (file-name-directory output-file-expanded))))
    (let ((process (start-process-shell-command process-name nil command)))
      (if (and duration (> duration 0))
          (let ((timer (run-at-time
                        0 1
                        (lambda ()
                          (if (> remaining 0)
                              (progn
                                (message "Recording to '%s': %d seconds remaining..." output-file-expanded remaining)
                                (setq remaining (1- remaining)))
                            (message "Recording saved to '%s'" output-file-expanded)
                            (cancel-timer timer)))
                        output-file-expanded)))
            (set-process-sentinel
             process
             (lambda (proc event)
               (when (string-match-p "exited" event)
                 (cancel-timer timer)
                 (message "Recording saved to '%s'" output-file-expanded)))))
        (message "Recording to '%s'... Press C-g or M-x org-meeting-summarizer-stop-recording to stop." output-file-expanded)
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (string-match-p "exited" event)
             (message "Recording saved to '%s'" output-file-expanded)))))
      process)))

(defun org-meeting-summarizer-stop-recording ()
  "Stop the audio recording process."
  (interactive)
  (let ((process (get-process "audio-recording")))
    (if process
        (progn
          (interrupt-process process)
          (message "Recording stopped"))
      (message "No recording process found"))))

(defun org-meeting-summarizer-record-and-summarize (output-file duration &optional custom-prompt)
  "Record audio to OUTPUT-FILE (.m4a) for DURATION seconds, then summarize in Org-mode subtree.
Optionally provide CUSTOM-PROMPT."
  (interactive "FFile to save recording (e.g., ~/Documents/0Inbox/recording.m4a): \nNDuration in seconds (0 for manual stop): \nsCustom Prompt (leave empty for default): ")
  (let ((output-file-expanded (expand-file-name output-file)))
    (message "Starting recording to '%s'..." output-file-expanded)
    (let ((process (org-meeting-summarizer-record-audio-to-m4a output-file duration)))
      (if (and duration (> duration 0))
          (progn
            (message "Waiting for recording to complete...")
            (sleep-for (+ duration 1))  ; Wait for recording to finish
            (message "Verifying recorded file: '%s'" output-file-expanded)
            (if (file-exists-p output-file-expanded)
                (progn
                  (message "Summarizing recorded file '%s'..." output-file-expanded)
                  (org-meeting-summarizer-in-subtree output-file custom-prompt))
              (error "Recorded file '%s' does not exist." output-file-expanded)))
        (message "Recording started. Run M-x org-meeting-summarizer-stop-recording to stop, then M-x org-meeting-summarizer-in-subtree to summarize '%s'" output-file-expanded)))))

(defun org-meeting-summarizer (path &optional custom-prompt)
  "Summarize m4a meeting files in PATH using Gemini API.
Optionally provide CUSTOM-PROMPT. Output goes to *Meeting Summaries* buffer."
  (interactive "fPath to file or folder: \nsCustom Prompt (leave empty for default): ")
  (let ((path-expanded (expand-file-name path))
        (prompt-arg (if (and custom-prompt (not (string-empty-p custom-prompt)))
                        (format "--prompt \"%s\"" custom-prompt)
                      ""))
        (output-buffer (get-buffer-create "*Meeting Summaries*")))
    (message "Checking path: '%s'" path-expanded)
    (if (or (file-exists-p path-expanded) (file-directory-p path-expanded))
        (with-current-buffer output-buffer
          (erase-buffer)
          (org-mode)
          (shell-command
           (format "python3 %s \"%s\" --api_key \"%s\" --model \"%s\" --retry_delay %d %s"
                   org-meeting-summarizer-script-path
                   path-expanded
                   org-meeting-summarizer-api-key
                   org-meeting-summarizer-model
                   org-meeting-summarizer-retry-delay
                   prompt-arg)
           output-buffer)
          (goto-char (point-min))
          (switch-to-buffer output-buffer))
      (error "Path '%s' does not exist." path-expanded))))

(defun org-meeting-summarizer-in-subtree (path &optional custom-prompt)
  "Summarize m4a meeting files in PATH and insert summary into current Org-mode subtree.
Optionally provide CUSTOM-PROMPT."
  (interactive "fPath to file or folder: \nsCustom Prompt (leave empty for default): ")
  (unless (derived-mode-p 'org-mode)
    (error "This function must be called in an Org-mode buffer"))
  (when (or (null path) (string-empty-p path))
    (error "No path provided"))
  (let* ((org-buffer (current-buffer))
         (path-expanded (expand-file-name path))
         (prompt-arg (if (and custom-prompt (not (string-empty-p custom-prompt)))
                         (format "--prompt \"%s\"" custom-prompt)
                       ""))
         (command (format "python3 %s \"%s\" --api_key \"%s\" --model \"%s\" --retry_delay %d %s"
                          org-meeting-summarizer-script-path
                          path-expanded
                          org-meeting-summarizer-api-key
                          org-meeting-summarizer-model
                          org-meeting-summarizer-retry-delay
                          prompt-arg))
         (temp-buffer (generate-new-buffer "*Temp Meeting Summaries*"))
         (summary-text ""))
    (message "Checking path: '%s'" path-expanded)
    (if (or (file-exists-p path-expanded) (file-directory-p path-expanded))
        (progn
          (message "Running summarization for '%s'..." path-expanded)
          (with-current-buffer temp-buffer
            (org-mode)
            (shell-command command temp-buffer)
            (goto-char (point-min))
            (message "Raw output in *Temp Meeting Summaries*: %s"
                     (buffer-substring (point-min) (min 500 (point-max))))
            (let ((in-summary nil))
              (while (not (eobp))
                (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
                  (message "Processing line: %s" line)
                  (cond
                   ((string-match-p "^\\*\\*Summary for" line)
                    (setq in-summary t)
                    (setq summary-text (concat summary-text line "\n")))
                   ((string-match-p "^\\* +\\(Processing\\|Error\\|Rate limit\\|No m4a files\\)" line)
                    (setq in-summary nil))
                   (in-summary
                    (setq summary-text (concat summary-text line "\n")))))
                (forward-line 1))
              (message "Captured summary text: %s"
                       (if (string-empty-p summary-text)
                           "Empty"
                         (substring summary-text 0 (min 100 (length summary-text))))))
          (with-current-buffer org-buffer
            (message "Inserting summary into subtree...")
            (unless (org-at-heading-p)
              (message "Not at a heading; moving to nearest parent heading...")
              (org-back-to-heading t))
            (org-end-of-subtree nil t)
            (when (not (bolp)) (insert "\n"))
            (if (and summary-text (not (string-empty-p summary-text)))
                (progn
                  (insert summary-text)
                  (message "Summary inserted successfully for '%s'" path-expanded))
              (progn
                (message "No valid summary generated for '%s'. Check *Temp Meeting Summaries* for details." path-expanded)
                (display-buffer temp-buffer))))
          (unless (string-empty-p summary-text)
            (kill-buffer temp-buffer)))
      (error "Path '%s' does not exist." path-expanded))))

(provide 'org-meeting-summarizer)
;;; org-meeting-summarizer.el ends here