# Quick Reference - New Features

## 🎯 Default Timer = 0 (Manual Stop)

| Action | Before | After | Example |
|--------|--------|-------|---------|
| **Duration prompt** | Required typing | Default to 0 | Just press ENTER ↵ |
| **Manual recording** | Type "0" | Press ENTER | Faster! ⚡ |
| **Fixed duration** | Type "30" | Type "30" | Still works as before |

**Usage:**
```
M-x org-meeting-summarizer-record-and-summarize
Duration in seconds (0 for manual stop, default: 0): ↵
→ Starts manual recording automatically!
```

---

## 📅 Today's Date in Summary

| Aspect | Before | After |
|--------|--------|-------|
| **Date if not mentioned** | "Unknown" | Today's date (2025-01-15) |
| **Date if mentioned** | Uses mentioned date | Uses mentioned date |
| **Default prompt** | No date context | Includes today's date |

**Example Summary:**
```
**Summary for recording.m4a**
- Date: 2025-01-15          ← Auto-filled with today's date!
- Title: Team Sync
- Attendees: John, Sarah
- Notes: Q1 planning
- Action Items: Update docs
```

---

## 🎵 Audio Format Support

| Format | Supported? | Example |
|--------|-----------|---------|
| .m4a | ✅ Yes | `recording.m4a` |
| .mp3 | ✅ Yes (NEW!) | `meeting.mp3` |
| .wav | ✅ Yes (NEW!) | `audio.wav` |
| .ogg | ✅ Yes (NEW!) | `podcast.ogg` |
| .flac | ✅ Yes (NEW!) | `lossless.flac` |

**Usage:**
```
M-x org-meeting-summarizer-in-subtree
Path to audio file or folder: ~/Downloads/meeting.mp3
→ Works! Automatically detects format and processes it
```

---

## 🔧 AI Provider Flexibility

**Current Default (Recommended):**
```
Provider: Gemini ✅
Model: gemini-2.5-flash (fastest)
API Key: GEMINI_API_KEY
```

**Other Gemini Models Available:**
```
Model: gemini-2.5-pro (more capable, slower)
Model: gemini-1.5-flash (older, still works)
```

**Future Extensible To:**
```
Provider: OpenAI (when implemented)
Provider: Anthropic (when implemented)
Provider: Local Models (when implemented)
```

---

## 📋 Command Summary

### Record & Summarize (with new defaults)
```bash
M-x org-meeting-summarizer-record-and-summarize
File: recording.m4a
Duration: [ENTER]          ← Defaults to 0 (manual stop)
Prompt: [ENTER]            ← Uses default with today's date
```

### Summarize Existing File (now supports MP3!)
```bash
M-x org-meeting-summarizer-in-subtree
Path: meeting.mp3          ← Now accepts MP3, WAV, etc!
Prompt: [ENTER]            ← Includes today's date automatically
```

### Using Hydra Menu
```bash
C-c m                      ← Opens Hydra
_r_: Record & summarize    ← Duration defaults to 0
_s_: Summarize file        ← Accepts any audio format
_t_: Stop recording        ← Stop manual recording
_q_: Quit                  ← Close menu
```

---

## 🔄 What Changed in Files

### org-meeting-summarizer.el
```lisp
;; BEFORE: N for number (no default)
(interactive "FFile: \nNDuration: \nsCustom Prompt: ")

;; AFTER: read-number with default value 0
(interactive 
 (list (read-file-name "File: " nil nil nil "recording.m4a")
       (read-number "Duration in seconds (0 for manual stop, default: 0): " 0)
       (read-string "Custom Prompt: ")))
```

### summarize_meetings.py
```python
# BEFORE: No date context
prompt = "summarize this meeting under date|title with attendees, notes and action items"

# AFTER: Today's date included automatically
today_date = datetime.now().strftime("%Y-%m-%d")
prompt = f"Today is {today_date}. Please summarize this meeting under: Date (use today's date if not mentioned), Title, Attendees, Notes, and Action Items."

# BEFORE: Only .m4a files
supported_formats = ('.m4a',)

# AFTER: Multiple formats
supported_formats = ('.m4a', '.mp3', '.wav', '.ogg', '.flac')

# NEW: Flexible AI provider
--api_provider gemini  # (future: openai, anthropic, etc.)
```

---

## ✨ Key Improvements Summary

| Improvement | Impact | Time Saved |
|------------|--------|-----------|
| Default 0 for manual stop | Don't type "0" | 2 seconds per session |
| Today's date auto-filled | No "Unknown" dates | 5 seconds of reading |
| MP3 support | Use any audio format | No format conversion |
| Flexible API system | Future-proof | Scalable to other AIs |

---

## 💡 Pro Tips

### Tip 1: Fastest Workflow
```
1. Just press ENTER for duration (defaults to 0)
2. Just press ENTER for prompt (gets today's date)
3. Start recording and talking!
```

### Tip 2: Use Hydra for Efficiency
```
C-c m r  ← Quick access to record-and-summarize
C-c m s  ← Quick access to summarize existing
C-c m t  ← Quick stop recording
```

### Tip 3: Custom Prompt Override
If you want a custom prompt (overrides today's date default):
```
M-x org-meeting-summarizer-record-and-summarize
Custom Prompt: Summarize this in bullet points with focus on action items
↓
Uses YOUR prompt instead of default with today's date
```

### Tip 4: Mix Audio Formats
You can have .m4a, .mp3, and .wav in the same folder:
```
M-x org-meeting-summarizer-in-subtree
Path: ~/Recordings/  (contains mix of formats)
↓
Automatically processes ALL supported formats!
```

---

## 🚀 Getting Started

### 1. Update Files
```bash
# Copy new files to your org-meeting-summarizer directory
cp org-meeting-summarizer.el ~/.emacs.d/org-meeting-summarizer/
cp summarize_meetings.py ~/.emacs.d/org-meeting-summarizer/scripts/
```

### 2. No Config Changes Needed!
Your existing Emacs config works as-is. The new defaults are built-in.

### 3. Start Using New Features
```
M-x org-meeting-summarizer-record-and-summarize
→ Press ENTER for duration (now defaults to 0)
→ Profit! ✅
```

---

## ❓ FAQ

**Q: Will my existing recordings still work?**
A: Yes! MP3 support is in addition to M4A. All your old M4A files still work.

**Q: Does the date default affect custom prompts?**
A: No. If you provide a custom prompt, that's used instead. The date default only applies to the default prompt.

**Q: Can I use a different Gemini model?**
A: Yes! Just change `org-meeting-summarizer-model` in your Emacs config.

**Q: When will other AI providers be supported?**
A: The infrastructure is in place. OpenAI, Anthropic, etc. can be added by extending the Python script.

**Q: What if I don't want today's date in the summary?**
A: Provide your own custom prompt without the date context.

---

## 📞 Troubleshooting

**Issue: "Unknown date" still appearing**
→ Make sure you're using the NEW `summarize_meetings.py` script

**Issue: MP3 file not accepted**
→ Check that the file exists and ends with `.mp3` (lowercase)

**Issue: Default "0" not working**
→ Make sure you're using the NEW `org-meeting-summarizer.el` file

**Issue: Duration prompt looks different**
→ Normal! New version uses `read-number` instead of raw input format
