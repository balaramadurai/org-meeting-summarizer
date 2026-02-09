;;; org-meeting-summarizer.el --- Record and summarize meetings in Org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Bala

;; Author: Bala <bala@balaramadurai.net>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: org, meetings, summarization, audio
;; URL: https://github.com/balaramadurai/org-meeting-summarizer
;; License: MIT

;;; Commentary:

;; This package records audio meetings using ffmpeg, summarizes them using the
;; Gemini API, and inserts summaries into Org-mode subtrees. Summaries have bold
;; titles (e.g., **Summary for ...**) and do not fold the subtree.
;; 
;; The code was initially generated with assistance from Grok, an AI created by xAI,
;; and further developed and enhanced by Claude, an AI assistant created by Anthropic.
;; Recent improvements include: default timer values, today's date auto-inclusion in
;; summaries, multi-format audio support (MP3, WAV, OGG, FLAC), flexible AI provider
;; system, improved timer display, temp file auto-generation, and comprehensive documentation.

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
  "API key for summarizing audio files.
For Gemini: your Google API key.
For Ollama cloud: your Ollama cloud API key.
For local Ollama: can be left empty."
  :type 'string
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-model "gemini-2.5-flash"
  "AI model to use for summarization.
For Gemini: gemini-2.5-flash, gemini-2.5-pro, etc.
For Ollama: llama3, mistral, gpt-oss:12b-cloud, etc."
  :type 'string
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-retry-delay 60
  "Delay in seconds between retries after hitting API rate limit."
  :type 'integer
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-api-provider "gemini"
  "AI API provider to use for summarization.
Options: \"gemini\" or \"ollama\"."
  :type '(choice (const :tag "Google Gemini" "gemini")
                 (const :tag "Ollama (local or cloud)" "ollama"))
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-ollama-api-base "http://localhost:11434"
  "Base URL for Ollama API.
For local Ollama: http://localhost:11434 (default)
For Ollama cloud: use your cloud provider URL (e.g., https://api.ollama.ai)"
  :type 'string
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-whisper-model "base"
  "Whisper model size for audio transcription.
Used when API provider is 'ollama' (since Ollama doesn't support audio directly).
Options: tiny, base, small, medium, large, large-v2, large-v3.
Larger models are more accurate but slower and require more memory."
  :type '(choice (const :tag "Tiny (fastest, least accurate)" "tiny")
                 (const :tag "Base (good balance)" "base")
                 (const :tag "Small" "small")
                 (const :tag "Medium" "medium")
                 (const :tag "Large" "large")
                 (const :tag "Large V2" "large-v2")
                 (const :tag "Large V3 (slowest, most accurate)" "large-v3"))
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-temp-dir
  (expand-file-name "org-meeting-summarizer" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for temporary recordings. Auto-generated files are stored here."
  :type 'string
  :group 'org-meeting-summarizer)

(defcustom org-meeting-summarizer-keep-temp-files nil
  "If non-nil, keep temporary recording files after summarization. Otherwise, delete them."
  :type 'boolean
  :group 'org-meeting-summarizer)

;; Internal variable to store insertion point marker
(defvar org-meeting-summarizer--insertion-marker nil
  "Marker for where to insert the summary after recording stops.")

(defun org-meeting-summarizer--ensure-temp-dir ()
  "Ensure the temp directory exists."
  (let ((temp-dir (expand-file-name org-meeting-summarizer-temp-dir)))
    (unless (file-directory-p temp-dir)
      (make-directory temp-dir t))
    temp-dir))

(defun org-meeting-summarizer--generate-temp-filename (format-ext)
  "Generate a temp filename with timestamp and FORMAT-EXT (e.g., 'm4a', 'mp3')."
  (format-time-string (format "recording-%%Y%%m%%d-%%H%%M%%S.%s" format-ext)))

;; Internal function: record audio (called programmatically, no interactive form)
(defun org-meeting-summarizer--record-audio-internal (output-file-expanded duration org-buffer custom-prompt &optional insert-at-point)
  "Internal function to record audio and handle process sentinel.
OUTPUT-FILE-EXPANDED: expanded file path
DURATION: recording duration in seconds (0 for manual stop)
ORG-BUFFER: buffer to auto-summarize into
CUSTOM-PROMPT: optional custom prompt for summarization
INSERT-AT-POINT: if non-nil, insert summary at cursor position instead of end of subtree"
  (let* ((command (if (and duration (> duration 0))
                      (format "ffmpeg -f pulse -i default -t %d -c:a aac '%s' 2>/dev/null" duration output-file-expanded)
                    (format "ffmpeg -f pulse -i default -c:a aac '%s' 2>/dev/null" output-file-expanded)))
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
          ;; Fixed duration recording with countdown spinner
          (let ((timer nil)
                (spinner (vector "⏺ " "◑ " "◕ " "◐ "))
                (spin-index 0))
            (setq timer
              (run-at-time
               0 1
               (lambda ()
                 (if (> remaining 0)
                     (progn
                       (message "%s [%02d:%02d] Recording '%s'"
                                (aref spinner (mod spin-index 4))
                                (/ remaining 60)
                                (mod remaining 60)
                                (file-name-nondirectory output-file-expanded))
                       (setq spin-index (1+ spin-index))
                       (setq remaining (1- remaining)))
                   (message "✅ Recording saved '%s'" (file-name-nondirectory output-file-expanded))
                   (cancel-timer timer)))))
            (set-process-sentinel
             process
             (lambda (proc event)
               (when (string-match-p "exited" event)
                 (cancel-timer timer)
                 (message "✅ Recording saved '%s'" (file-name-nondirectory output-file-expanded))
                 (when (not (file-exists-p output-file-expanded))
                   (message "Warning: Recorded file '%s' not found after process exit" output-file-expanded)))))
            process)
        ;; Manual stop recording with forward-counting timer
        (let ((elapsed-seconds 0)
              (manual-timer nil))
          (setq manual-timer
            (run-at-time
             0 1
             (lambda ()
               (message "🔴 [%02d:%02d] Recording '%s' - Press M-x org-meeting-summarizer-stop-recording to stop"
                        (/ elapsed-seconds 60)
                        (mod elapsed-seconds 60)
                        (file-name-nondirectory output-file-expanded))
               (setq elapsed-seconds (1+ elapsed-seconds)))))
          (set-process-sentinel
           process
           (lambda (proc event)
             (when (string-match-p "exited" event)
               (cancel-timer manual-timer)
               (message "✅ Recording saved [Total: %02d:%02d] '%s'"
                        (/ elapsed-seconds 60)
                        (mod elapsed-seconds 60)
                        (file-name-nondirectory output-file-expanded))
               (when (not (file-exists-p output-file-expanded))
                 (message "Warning: Recorded file '%s' not found after process exit" output-file-expanded))
               ;; Auto-summarize if org-buffer is provided
                (when org-buffer
                  (message "Verifying recorded file: '%s'" output-file-expanded)
                  (let ((retry-count 0)
                        (max-retries 3))
                    (while (and (not (file-exists-p output-file-expanded)) (< retry-count max-retries))
                      (message "File '%s' not found, retrying (%d/%d)..." output-file-expanded (1+ retry-count) max-retries)
                      (sleep-for 1)
                      (setq retry-count (1+ retry-count)))
                    (if (file-exists-p output-file-expanded)
                        (progn
                          (message "Summarizing recorded file '%s'..." output-file-expanded)
                          (if insert-at-point
                              (org-meeting-summarizer-at-point output-file-expanded custom-prompt)
                            (with-current-buffer org-buffer
                              (org-meeting-summarizer-in-subtree output-file-expanded custom-prompt))))
                      (message "Error: Recorded file '%s' does not exist after %d retries" output-file-expanded max-retries)))))))
          process)))))

;; User-facing recording function (with interactive form)
(defun org-meeting-summarizer-record-audio-to-m4a (output-file duration)
  "Record audio using ffmpeg and save to OUTPUT-FILE (.m4a, .mp3, etc.) for DURATION seconds.
If DURATION is nil or 0, record until manually stopped with M-x org-meeting-summarizer-stop-recording.
Supported formats: m4a, mp3, wav, ogg, flac"
  (interactive "FFile to save recording (e.g., ~/Documents/0Inbox/recording.m4a): \nNDuration in seconds (0 for manual stop): ")
  (org-meeting-summarizer--record-audio-internal (expand-file-name output-file) duration nil nil))

(defun org-meeting-summarizer-stop-recording ()
  "Stop the audio recording process."
  (interactive)
  (let ((process (get-process "audio-recording")))
    (if process
        (progn
          (interrupt-process process)
          ;; Give ffmpeg time to finalize the file
          (sleep-for 0.5)
          (message "Recording stopped"))
      (message "No recording process found"))))

(defun org-meeting-summarizer-record-and-summarize (duration &optional custom-prompt)
  "Record audio for DURATION seconds, then summarize and insert at current cursor position.
Recording is saved to temp dir (`org-meeting-summarizer-temp-dir') with auto-generated filename.
If DURATION is 0, record until manually stopped and summarize automatically.
If no date is mentioned in the recording, today's date will be used in the summary.
Optionally provide CUSTOM-PROMPT (leave empty for default with today's date).
Temp files are auto-deleted after summarization unless `org-meeting-summarizer-keep-temp-files' is non-nil."
  (interactive 
   (list (read-number "Duration in seconds (0 for manual stop, default: 0): " 0)
         (read-string "Custom Prompt (leave empty for default): ")))
  (when (and (string= org-meeting-summarizer-api-provider "gemini")
             (string-empty-p org-meeting-summarizer-api-key))
    (error "API key is required for Gemini provider. See README.org for setup instructions"))
  (let* ((temp-dir (org-meeting-summarizer--ensure-temp-dir))
         (temp-filename (org-meeting-summarizer--generate-temp-filename "m4a"))
         (output-file (expand-file-name temp-filename temp-dir))
         (org-buffer (current-buffer)))
    ;; Save current cursor position as marker for later insertion
    (setq org-meeting-summarizer--insertion-marker (point-marker))
    (message "Starting recording to '%s'..." output-file)
    (if (and duration (> duration 0))
        ;; Fixed duration: synchronous wait + auto-summarize at point
        (progn
          (org-meeting-summarizer--record-audio-internal output-file duration nil custom-prompt t)
          (message "Waiting for recording to complete...")
          (sleep-for (+ duration 1))
          (message "Verifying recorded file: '%s'" output-file)
          (let ((retry-count 0)
                (max-retries 3))
            (while (and (not (file-exists-p output-file)) (< retry-count max-retries))
              (message "File '%s' not found, retrying (%d/%d)..." output-file (1+ retry-count) max-retries)
              (sleep-for 1)
              (setq retry-count (1+ retry-count)))
            (if (file-exists-p output-file)
                (progn
                  (message "Summarizing recorded file '%s'..." output-file)
                  (org-meeting-summarizer-at-point output-file custom-prompt))
              (error "Recorded file '%s' does not exist after %d retries" output-file max-retries))))
      ;; Manual stop: async with auto-summarize at point via process sentinel
      (progn
        (org-meeting-summarizer--record-audio-internal output-file duration org-buffer custom-prompt t)
        (message "🔴 Recording started (auto-summarizes at cursor when done) - Press M-x org-meeting-summarizer-stop-recording to stop")))))

(defun org-meeting-summarizer--build-command (path-expanded prompt-arg)
  "Build the Python command with PATH-EXPANDED and PROMPT-ARG."
  (let ((base-cmd (format "python3 %s \"%s\" --api_key \"%s\" --model \"%s\" --retry_delay %d --api_provider \"%s\""
                          org-meeting-summarizer-script-path
                          path-expanded
                          org-meeting-summarizer-api-key
                          org-meeting-summarizer-model
                          org-meeting-summarizer-retry-delay
                          org-meeting-summarizer-api-provider)))
    (when (string= org-meeting-summarizer-api-provider "ollama")
      (setq base-cmd (concat base-cmd (format " --api_base \"%s\" --whisper_model \"%s\""
                                              org-meeting-summarizer-ollama-api-base
                                              org-meeting-summarizer-whisper-model))))
    (if (and prompt-arg (not (string-empty-p prompt-arg)))
        (concat base-cmd " " prompt-arg)
      base-cmd)))

(defun org-meeting-summarizer (path &optional custom-prompt)
  "Summarize audio meeting files (m4a, mp3, wav, ogg, flac) in PATH using AI.
Supports Gemini API and Ollama (local or cloud).
If no date is found in the recording, today's date will be used as default.
Optionally provide CUSTOM-PROMPT. Output goes to *Meeting Summaries* buffer."
  (interactive "fPath to file or folder: \nsCustom Prompt (leave empty for default): ")
  (when (and (string= org-meeting-summarizer-api-provider "gemini")
             (string-empty-p org-meeting-summarizer-api-key))
    (error "API key is required for Gemini provider. See README.org for setup instructions"))
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
           (org-meeting-summarizer--build-command path-expanded prompt-arg)
           output-buffer)
          (goto-char (point-min))
          (switch-to-buffer output-buffer))
      (error "Path '%s' does not exist." path-expanded))))

(defun org-meeting-summarizer-in-subtree (path &optional custom-prompt)
  "Summarize audio meeting files (m4a, mp3, wav, ogg, flac) in PATH and insert summary into current Org-mode subtree.
Supports Gemini API and Ollama (local or cloud).
If no date is found in the recording, today's date will be used as default.
Optionally provide CUSTOM-PROMPT (leave empty for default with today's date).
If PATH is in `org-meeting-summarizer-temp-dir' and `org-meeting-summarizer-keep-temp-files'
is nil, the temp file is deleted after successful summarization."
  (interactive "fPath to file or folder: \nsCustom Prompt (leave empty for default): ")
  (unless (derived-mode-p 'org-mode)
    (error "This function must be called in an Org-mode buffer"))
  (when (or (null path) (string-empty-p path))
    (error "No path provided"))
  (when (and (string= org-meeting-summarizer-api-provider "gemini")
             (string-empty-p org-meeting-summarizer-api-key))
    (error "API key is required for Gemini provider. See README.org for setup instructions"))
  (let* ((org-buffer (current-buffer))
         (path-expanded (expand-file-name path))
         (temp-dir (expand-file-name org-meeting-summarizer-temp-dir))
         (is-temp-file (string-prefix-p temp-dir path-expanded))
         (prompt-arg (if (and custom-prompt (not (string-empty-p custom-prompt)))
                         (format "--prompt \"%s\"" custom-prompt)
                       ""))
         (command (org-meeting-summarizer--build-command path-expanded prompt-arg))
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
            ;; Find the start of the summary section (single * from Python script)
            (if (or (re-search-forward "^\\*+ ?Summary for" nil t)
                    (re-search-forward "^\\*+ ?Meeting Summary" nil t))
                (progn
                  ;; Go back to the beginning of the line
                  (beginning-of-line)
                  ;; Capture everything from here to the end
                  (setq summary-text (buffer-substring (point) (point-max))))
              (message "No summary section found in output"))
            (message "Captured summary text: %s"
                     (if (string-empty-p summary-text)
                         "Empty"
                       (substring summary-text 0 (min 100 (length summary-text))))))
          (with-current-buffer org-buffer
            (message "Inserting summary into subtree for '%s'..." path-expanded)
            (unless (org-at-heading-p)
              (message "Not at a heading; moving to nearest parent heading...")
              (org-back-to-heading t))
            (org-end-of-subtree nil t)
            (when (not (bolp)) (insert "\n"))
            (if (and summary-text (not (string-empty-p summary-text)))
                (progn
                  (insert summary-text)
                  (message "Summary inserted successfully for '%s'" path-expanded)
                  ;; Delete temp file if applicable
                  (when (and is-temp-file
                             (not org-meeting-summarizer-keep-temp-files)
                             (file-exists-p path-expanded))
                    (delete-file path-expanded)
                    (message "✅ Deleted temp file: '%s'" path-expanded)))
              (progn
                (message "No valid summary generated for '%s'. Check *Temp Meeting Summaries* for details." path-expanded)
                (display-buffer temp-buffer))))
          (unless (string-empty-p summary-text)
            (kill-buffer temp-buffer))))
      (error "Path '%s' does not exist." path-expanded)))

(defun org-meeting-summarizer-at-point (path &optional custom-prompt)
  "Summarize audio meeting files in PATH and insert summary at current cursor position.
Supports Gemini API and Ollama (local or cloud).
If no date is found in the recording, today's date will be used as default.
Optionally provide CUSTOM-PROMPT (leave empty for default with today's date).
If PATH is in `org-meeting-summarizer-temp-dir' and `org-meeting-summarizer-keep-temp-files'
is nil, the temp file is deleted after successful summarization."
  (interactive "fPath to file or folder: \nsCustom Prompt (leave empty for default): ")
  (when (or (null path) (string-empty-p path))
    (error "No path provided"))
  (when (and (string= org-meeting-summarizer-api-provider "gemini")
             (string-empty-p org-meeting-summarizer-api-key))
    (error "API key is required for Gemini provider. See README.org for setup instructions"))
  (let* ((insertion-marker (or org-meeting-summarizer--insertion-marker (point-marker)))
         (org-buffer (marker-buffer insertion-marker))
         (path-expanded (expand-file-name path))
         (temp-dir (expand-file-name org-meeting-summarizer-temp-dir))
         (is-temp-file (string-prefix-p temp-dir path-expanded))
         (prompt-arg (if (and custom-prompt (not (string-empty-p custom-prompt)))
                         (format "--prompt \"%s\"" custom-prompt)
                       ""))
         (command (org-meeting-summarizer--build-command path-expanded prompt-arg))
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
            ;; Find the start of the summary section (single * from Python script)
            (if (or (re-search-forward "^\\*+ ?Summary for" nil t)
                    (re-search-forward "^\\*+ ?Meeting Summary" nil t))
                (progn
                  ;; Go back to the beginning of the line
                  (beginning-of-line)
                  ;; Capture everything from here to the end
                  (setq summary-text (buffer-substring (point) (point-max))))
              (message "No summary section found in output")))
          ;; Insert at the saved marker position
          (with-current-buffer org-buffer
            (goto-char insertion-marker)
            (when (not (bolp)) (insert "\n"))
            (if (and summary-text (not (string-empty-p summary-text)))
                (progn
                  (insert summary-text)
                  (message "Summary inserted at point for '%s'" path-expanded)
                  ;; Delete temp file if applicable
                  (when (and is-temp-file
                             (not org-meeting-summarizer-keep-temp-files)
                             (file-exists-p path-expanded))
                    (delete-file path-expanded)
                    (message "✅ Deleted temp file: '%s'" path-expanded)))
              (progn
                (message "No valid summary generated for '%s'. Check *Temp Meeting Summaries* for details." path-expanded)
                (display-buffer temp-buffer))))
          ;; Clean up marker
          (when org-meeting-summarizer--insertion-marker
            (set-marker org-meeting-summarizer--insertion-marker nil)
            (setq org-meeting-summarizer--insertion-marker nil))
          (unless (string-empty-p summary-text)
            (kill-buffer temp-buffer)))
      (error "Path '%s' does not exist." path-expanded))))

(provide 'org-meeting-summarizer)
;;; org-meeting-summarizer.el ends here
