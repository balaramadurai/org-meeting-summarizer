# Setup & Installation Guide

## Files You Need to Update

### 1. **org-meeting-summarizer.el** (Updated)
This file has been updated with:
- ✅ Default "0" for timer input
- ✅ Updated documentation for audio format support
- ✅ Updated documentation for date handling

**Installation:**
```bash
# If using straight.el (recommended):
# No action needed - straight will pull from GitHub when you update

# If manual installation:
cp org-meeting-summarizer.el ~/.emacs.d/org-meeting-summarizer/
```

### 2. **summarize_meetings.py** (NEW - MUST UPDATE!)
This is the script that handles AI summarization. It's been completely rewritten with:
- ✅ Today's date auto-inclusion in summaries
- ✅ MP3 and other audio format support (.wav, .ogg, .flac)
- ✅ Flexible AI provider system (ready for OpenAI, Anthropic, etc.)
- ✅ Better error handling

**Installation:**
```bash
# IMPORTANT: This file MUST be replaced for new features to work!

cp summarize_meetings.py ~/.emacs.d/org-meeting-summarizer/scripts/
# or
cp summarize_meetings.py ~/.emacs.d/straight/repos/org-meeting-summarizer/scripts/
```

---

## Installation Methods

### Method 1: Using straight.el (RECOMMENDED)

If you're using `straight.el`, the Emacs file will be updated automatically when you reload. However, **you must manually update the Python script**.

```bash
# Find your straight repo location (usually):
cd ~/.emacs.d/straight/repos/org-meeting-summarizer/

# Backup old script:
cp scripts/summarize_meetings.py scripts/summarize_meetings.py.bak

# Copy new script:
cp /path/to/new/summarize_meetings.py scripts/summarize_meetings.py

# Verify it's executable:
chmod +x scripts/summarize_meetings.py
```

### Method 2: Manual Installation

```bash
# Navigate to your installation directory:
cd ~/.emacs.d/org-meeting-summarizer/

# Backup old files:
cp org-meeting-summarizer.el org-meeting-summarizer.el.bak
cp scripts/summarize_meetings.py scripts/summarize_meetings.py.bak

# Copy new files:
cp /path/to/new/org-meeting-summarizer.el .
cp /path/to/new/summarize_meetings.py scripts/

# Make Python script executable:
chmod +x scripts/summarize_meetings.py
```

---

## Verification

### Check 1: Emacs File is Updated
```emacs-lisp
M-x eval-expression
(read-number "Test (default 0): " 0)
↓
Should have a default prompt with "default: 0"
```

### Check 2: Python Script is Updated
```bash
cd ~/.emacs.d/org-meeting-summarizer/scripts/
head -20 summarize_meetings.py

# Should see:
# - `from datetime import datetime`
# - `today_date = datetime.now().strftime("%Y-%m-%d")`
# - `supported_formats = ('.m4a', '.mp3', '.wav', '.ogg', '.flac')`
```

### Check 3: Test in Emacs
```
M-x org-meeting-summarizer-record-and-summarize

# Check the prompts:
1. Duration prompt should show "default: 0"
2. Recording should have timer showing "[MM:SS]"
3. Summary should have today's date automatically
```

---

## Configuration (No Changes Needed!)

Your existing Emacs configuration should work as-is. If you had:

```lisp
(use-package org-meeting-summarizer
  :custom
  (org-meeting-summarizer-api-key (getenv "GEMINI_API_KEY"))
  (org-meeting-summarizer-model "gemini-2.5-flash")
  (org-meeting-summarizer-retry-delay 60))
```

It will continue to work perfectly with all new features enabled!

### Optional: Customize Model

```lisp
;; For more capability (slower):
(setq org-meeting-summarizer-model "gemini-2.5-pro")

;; For speed (fastest):
(setq org-meeting-summarizer-model "gemini-2.5-flash")

;; Fallback (older version):
(setq org-meeting-summarizer-model "gemini-1.5-flash")
```

---

## Testing the New Features

### Test 1: Default Timer Value

```
M-x org-meeting-summarizer-record-and-summarize
File: ~/test_recording.m4a
Duration in seconds (0 for manual stop, default: 0): ↵ (just press ENTER)
```

**Expected:** Should default to 0 (manual stop mode)

### Test 2: Today's Date in Summary

After recording and summarizing:
```
**Summary for test_recording.m4a**
- Date: 2025-01-15          ← Should be today's date!
- Title: Your meeting title
- Attendees: Names
- Notes: ...
- Action Items: ...
```

**Expected:** Date should be today's date, not "Unknown"

### Test 3: MP3 File Support

```
M-x org-meeting-summarizer-in-subtree
Path to audio file: ~/Downloads/meeting.mp3
Custom Prompt: ↵ (leave empty for default)
```

**Expected:** Should process MP3 file without errors

### Test 4: Folder with Mixed Formats

```bash
# Create test folder with mixed audio formats:
mkdir ~/test_audio
# Copy some .m4a, .mp3, .wav files into it
```

```
M-x org-meeting-summarizer-in-subtree
Path to audio file: ~/test_audio/
Custom Prompt: ↵
```

**Expected:** Should process ALL audio files in the folder

---

## Troubleshooting

### Issue: "Unknown date" still appearing

**Solution:** You're using the old Python script.
```bash
# Check which script is being used:
M-x eval-expression
org-meeting-summarizer-script-path
↓
# Look at the path, verify it's the new one with mp3 support
```

### Issue: MP3 files not accepted

**Solution:** The old Python script is still running.
```bash
# Make sure you updated scripts/summarize_meetings.py
which python3
python3 --version  # Should be 3.6+

# Test the script directly:
python3 ~/.emacs.d/org-meeting-summarizer/scripts/summarize_meetings.py \
    "~/meeting.mp3" \
    --api_key "YOUR_KEY" \
    --model "gemini-2.5-flash"
```

### Issue: Duration input format looks different

**Normal!** The new version uses `read-number` which provides a better UI:
- Shows the default value clearly
- Validates numeric input
- More user-friendly

### Issue: Custom prompts not working

**Solution:** Make sure the Python script is updated.
```bash
# Check if custom prompt parameter is supported:
python3 ~/.emacs.d/org-meeting-summarizer/scripts/summarize_meetings.py --help
# Should show: --prompt CUSTOM_PROMPT
```

---

## File Locations Reference

### If using straight.el:
```
~/.emacs.d/straight/repos/org-meeting-summarizer/
├── org-meeting-summarizer.el          ← Update this
├── scripts/
│   └── summarize_meetings.py          ← Update this (CRITICAL!)
├── README.org
└── LICENSE
```

### If manually installed:
```
~/.emacs.d/org-meeting-summarizer/
├── org-meeting-summarizer.el          ← Update this
├── scripts/
│   └── summarize_meetings.py          ← Update this (CRITICAL!)
├── README.org
└── LICENSE
```

### Your config:
```
~/.emacs or ~/.emacs.d/init.el         ← No changes needed!
~/.zshrc or ~/.zshenv                  ← No changes needed!
```

---

## Environment Variables (Unchanged)

You should still have:
```bash
# In your ~/.zshrc or ~/.zshenv:
export GEMINI_API_KEY="your-api-key-here"
```

Or in Emacs:
```lisp
(setenv "GEMINI_API_KEY" "your-api-key-here")
```

No changes needed!

---

## Rollback (If Needed)

If something goes wrong:

```bash
# Restore old Python script:
cp ~/.emacs.d/org-meeting-summarizer/scripts/summarize_meetings.py.bak \
   ~/.emacs.d/org-meeting-summarizer/scripts/summarize_meetings.py

# Restore old Emacs file:
cp ~/.emacs.d/org-meeting-summarizer/org-meeting-summarizer.el.bak \
   ~/.emacs.d/org-meeting-summarizer/org-meeting-summarizer.el

# Reload Emacs:
M-x restart-emacs
```

---

## What's New (Summary)

| Feature | File | Impact |
|---------|------|--------|
| Default "0" for duration | org-meeting-summarizer.el | UX improvement |
| Today's date in summary | summarize_meetings.py | Data quality |
| MP3 & audio format support | summarize_meetings.py | File compatibility |
| Flexible AI providers | summarize_meetings.py | Future-proofing |

---

## Next Steps

1. **Download the updated files:**
   - `org-meeting-summarizer.el`
   - `summarize_meetings.py`

2. **Copy to your installation:**
   ```bash
   cp org-meeting-summarizer.el ~/.emacs.d/org-meeting-summarizer/
   cp summarize_meetings.py ~/.emacs.d/org-meeting-summarizer/scripts/
   ```

3. **Reload Emacs:**
   ```
   M-x restart-emacs
   # or
   M-x eval-buffer (on the updated file)
   ```

4. **Test the new features:**
   - Default duration: Just press ENTER
   - MP3 support: Try summarizing an MP3 file
   - Today's date: Check the summary has the date filled in

5. **Enjoy! 🎉**

---

## Support

If you encounter issues:

1. Check the **Troubleshooting** section above
2. Verify both files were updated (not just one!)
3. Make sure Python script is executable:
   ```bash
   chmod +x ~/.emacs.d/org-meeting-summarizer/scripts/summarize_meetings.py
   ```
4. Check error messages in `*Messages*` buffer
5. Test Python script directly from command line

---

## Questions?

Refer to:
- **NEW_FEATURES_GUIDE.md** - Detailed feature explanations
- **QUICK_REFERENCE.md** - Quick command reference
- **README.org** - Original documentation (still valid!)
