import os
import argparse
import time
import subprocess
import tempfile
from datetime import datetime

# Gemini imports (optional)
GEMINI_AVAILABLE = False
genai = None
ResourceExhausted = None
try:
    import google.generativeai as genai
    from google.api_core.exceptions import ResourceExhausted
    GEMINI_AVAILABLE = True
except ImportError:
    pass

# Ollama/requests imports (optional)
REQUESTS_AVAILABLE = False
requests = None
try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    pass

# Whisper imports (optional)
WHISPER_AVAILABLE = False
whisper = None
try:
    import whisper
    WHISPER_AVAILABLE = True
except ImportError:
    pass


def format_output(text):
    """Convert Markdown output to Org-mode format.
    
    Conversions:
    - # Heading -> * Heading (Org heading level 1)
    - ## Heading -> ** Heading (Org heading level 2)
    - ### Heading -> *** Heading (Org heading level 3)
    - **bold** -> *bold* (Org bold)
    - *italic* -> /italic/ (Org italic)
    - `code` -> =code= (Org verbatim)
    - - list item -> - list item (same)
    - [text](url) -> [[url][text]] (Org link)
    """
    import re
    
    lines = text.split('\n')
    formatted_lines = []
    
    for line in lines:
        original_line = line
        stripped_line = line.strip()
        
        # Convert Markdown headings to Org headings
        # Must check longer patterns first (#### before ### before ## before #)
        if stripped_line.startswith('#### '):
            line = line.replace('#### ', '**** ', 1)
        elif stripped_line.startswith('### '):
            line = line.replace('### ', '*** ', 1)
        elif stripped_line.startswith('## '):
            line = line.replace('## ', '** ', 1)
        elif stripped_line.startswith('# '):
            line = line.replace('# ', '* ', 1)
        
        # Convert Markdown bold **text** to Org bold *text*
        # Be careful not to affect headings (which now use * at start of line)
        line = re.sub(r'\*\*([^*]+)\*\*', r'*\1*', line)
        
        # Convert Markdown italic *text* to Org italic /text/
        # Only match single * not at start of line and not part of **
        line = re.sub(r'(?<!\*)(?<!\w)\*([^*\n]+)\*(?!\*)', r'/\1/', line)
        
        # Convert Markdown inline code `code` to Org verbatim =code=
        line = re.sub(r'`([^`]+)`', r'=\1=', line)
        
        # Convert Markdown links [text](url) to Org links [[url][text]]
        line = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'[[\2][\1]]', line)
        
        # Convert Markdown bullet lists (already - so no change needed)
        # Convert * bullets to - (Org typically uses -)
        if re.match(r'^(\s*)\* ', line) and not stripped_line.startswith('* ') or \
           (stripped_line.startswith('* ') and len(stripped_line) > 2 and stripped_line[2].islower()):
            # This is a list item, not a heading
            line = re.sub(r'^(\s*)\* ', r'\1- ', line)
        
        formatted_lines.append(line)
    
    return '\n'.join(formatted_lines)


def transcribe_with_whisper(audio_path, whisper_model="base"):
    """Transcribe audio file using OpenAI Whisper.
    
    Args:
        audio_path: Path to the audio file
        whisper_model: Whisper model size (tiny, base, small, medium, large)
    
    Returns:
        Transcribed text or None if failed
    """
    if not WHISPER_AVAILABLE:
        print("* Error: Whisper not installed. Run: pip install openai-whisper")
        return None
    
    try:
        print(f"* Transcribing with Whisper ({whisper_model} model)...")
        model = whisper.load_model(whisper_model)
        result = model.transcribe(audio_path)
        transcript = result["text"]
        print(f"* Transcription complete ({len(transcript)} characters)")
        return transcript
    except Exception as e:
        print(f"* Error transcribing audio: {str(e)}")
        return None


def transcribe_with_whisper_cli(audio_path, whisper_model="base"):
    """Transcribe audio file using whisper CLI (fallback if Python module not available).
    
    Args:
        audio_path: Path to the audio file
        whisper_model: Whisper model size (tiny, base, small, medium, large)
    
    Returns:
        Transcribed text or None if failed
    """
    try:
        print(f"* Transcribing with Whisper CLI ({whisper_model} model)...")
        
        # Create a temporary directory for output
        with tempfile.TemporaryDirectory() as temp_dir:
            # Run whisper CLI
            cmd = [
                "whisper", audio_path,
                "--model", whisper_model,
                "--output_dir", temp_dir,
                "--output_format", "txt"
            ]
            
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=600)
            
            if result.returncode != 0:
                print(f"* Error running whisper CLI: {result.stderr}")
                return None
            
            # Read the output file
            base_name = os.path.splitext(os.path.basename(audio_path))[0]
            txt_file = os.path.join(temp_dir, f"{base_name}.txt")
            
            if os.path.exists(txt_file):
                with open(txt_file, 'r', encoding='utf-8') as f:
                    transcript = f.read().strip()
                print(f"* Transcription complete ({len(transcript)} characters)")
                return transcript
            else:
                print(f"* Error: Transcription output file not found")
                return None
                
    except FileNotFoundError:
        print("* Error: whisper CLI not found. Install with: pip install openai-whisper")
        return None
    except subprocess.TimeoutExpired:
        print("* Error: Whisper transcription timed out")
        return None
    except Exception as e:
        print(f"* Error transcribing audio: {str(e)}")
        return None


def transcribe_audio(audio_path, whisper_model="base"):
    """Transcribe audio using Whisper (Python module or CLI).
    
    Args:
        audio_path: Path to the audio file
        whisper_model: Whisper model size (tiny, base, small, medium, large)
    
    Returns:
        Transcribed text or None if failed
    """
    # Try Python module first
    if WHISPER_AVAILABLE:
        return transcribe_with_whisper(audio_path, whisper_model)
    
    # Fallback to CLI
    return transcribe_with_whisper_cli(audio_path, whisper_model)


def summarize_with_gemini(model, prompt, full_path, mime_type, retry_delay, file):
    """Summarize audio using Gemini API."""
    retries = 3
    while retries > 0:
        try:
            print(f"* Processing {file} *")
            uploaded_file = genai.upload_file(full_path, mime_type=mime_type)
            response = model.generate_content([prompt, uploaded_file])
            summary = response.text
            formatted_summary = format_output(f"* Summary for {file}\n{summary}")
            print(f"{formatted_summary}\n")
            genai.delete_file(uploaded_file.name)
            return True
        except ResourceExhausted as e:
            retries -= 1
            if retries == 0:
                print(f"* Error processing {file}: Exhausted retries after hitting rate limit. {str(e)}")
                return False
            print(f"* Rate limit hit for {file}. Retrying after {retry_delay} seconds... ({retries} retries left)")
            time.sleep(retry_delay)
        except Exception as e:
            print(f"* Error processing {file}: {str(e)}")
            return False
    return False


def summarize_with_ollama(api_base, model_name, api_key, prompt, transcript, retry_delay, file):
    """Summarize transcript using Ollama API (local or cloud).
    
    Args:
        api_base: Base URL for Ollama API
        model_name: Name of the Ollama model
        api_key: API key (optional, for cloud)
        prompt: Summarization prompt
        transcript: Transcribed text from audio
        retry_delay: Delay between retries
        file: Original filename for display
    
    Returns:
        True if successful, False otherwise
    """
    retries = 3
    
    # Prepare the API endpoint
    chat_endpoint = f"{api_base.rstrip('/')}/api/chat"
    
    headers = {
        "Content-Type": "application/json"
    }
    
    # Add API key if provided (for Ollama cloud)
    if api_key:
        headers["Authorization"] = f"Bearer {api_key}"
    
    # Combine prompt with transcript
    full_prompt = f"{prompt}\n\nHere is the meeting transcript:\n\n{transcript}"
    
    while retries > 0:
        try:
            print(f"* Summarizing with Ollama ({model_name})...")
            
            payload = {
                "model": model_name,
                "messages": [
                    {
                        "role": "user",
                        "content": full_prompt
                    }
                ],
                "stream": False
            }
            
            response = requests.post(chat_endpoint, headers=headers, json=payload, timeout=300)
            
            if response.status_code == 200:
                result = response.json()
                # Handle different response formats
                if "message" in result:
                    summary = result["message"].get("content", "")
                elif "response" in result:
                    summary = result["response"]
                else:
                    summary = str(result)
                
                formatted_summary = format_output(f"* Summary for {file}\n{summary}")
                print(f"{formatted_summary}\n")
                return True
            elif response.status_code == 429:
                retries -= 1
                if retries == 0:
                    print(f"* Error processing {file}: Exhausted retries after hitting rate limit.")
                    return False
                print(f"* Rate limit hit for {file}. Retrying after {retry_delay} seconds... ({retries} retries left)")
                time.sleep(retry_delay)
            else:
                print(f"* Error processing {file}: HTTP {response.status_code} - {response.text}")
                return False
                
        except requests.exceptions.Timeout:
            retries -= 1
            if retries == 0:
                print(f"* Error processing {file}: Request timed out after retries.")
                return False
            print(f"* Timeout for {file}. Retrying after {retry_delay} seconds... ({retries} retries left)")
            time.sleep(retry_delay)
        except Exception as e:
            print(f"* Error processing {file}: {str(e)}")
            return False
    
    return False


def process_with_ollama(api_base, model_name, api_key, prompt, full_path, whisper_model, retry_delay, file):
    """Process audio file: transcribe with Whisper, then summarize with Ollama.
    
    Args:
        api_base: Base URL for Ollama API
        model_name: Name of the Ollama model
        api_key: API key (optional)
        prompt: Summarization prompt
        full_path: Path to audio file
        whisper_model: Whisper model size
        retry_delay: Delay between retries
        file: Filename for display
    
    Returns:
        True if successful, False otherwise
    """
    print(f"* Processing {file} *")
    
    # Step 1: Transcribe audio with Whisper
    transcript = transcribe_audio(full_path, whisper_model)
    if not transcript:
        print(f"* Error: Failed to transcribe {file}")
        return False
    
    # Step 2: Summarize transcript with Ollama
    return summarize_with_ollama(api_base, model_name, api_key, prompt, transcript, retry_delay, file)


def main():
    parser = argparse.ArgumentParser(
        description="Summarize audio meeting files (m4a, mp3, etc.) using Gemini API or Ollama (with Whisper transcription)."
    )
    parser.add_argument("path", type=str, help="Path to a single audio file or a folder containing audio files.")
    parser.add_argument(
        "--prompt", 
        type=str, 
        default=None,
        help="Custom prompt for summarization. If not provided, uses default with today's date."
    )
    parser.add_argument("--api_key", type=str, default="", help="API key for the AI service.")
    parser.add_argument(
        "--model", 
        type=str, 
        default="gemini-2.5-flash",
        help="AI model to use (default: gemini-2.5-flash). For Ollama: llama3, mistral, etc."
    )
    parser.add_argument(
        "--retry_delay", 
        type=int, 
        default=47,
        help="Delay in seconds between retries after hitting rate limit."
    )
    parser.add_argument(
        "--api_provider",
        type=str,
        default="gemini",
        choices=["gemini", "ollama"],
        help="AI API provider (default: gemini). Supports: gemini, ollama."
    )
    parser.add_argument(
        "--api_base",
        type=str,
        default="http://localhost:11434",
        help="Base URL for Ollama API (default: http://localhost:11434). For Ollama cloud, use the cloud URL."
    )
    parser.add_argument(
        "--whisper_model",
        type=str,
        default="base",
        choices=["tiny", "base", "small", "medium", "large", "large-v2", "large-v3"],
        help="Whisper model size for transcription (default: base). Used only with Ollama provider."
    )
    args = parser.parse_args()

    input_path = os.path.expanduser(args.path)
    if not os.path.exists(input_path):
        print(f"* Error: Path '{input_path}' does not exist.")
        return

    # Get today's date for default prompt
    today_date = datetime.now().strftime("%Y-%m-%d")
    
    # Use custom prompt if provided, otherwise use default with today's date
    if args.prompt:
        prompt = args.prompt
    else:
        prompt = f"Today is {today_date}. Please summarize this meeting under the following headings: Date (use today's date if not mentioned), Title, Attendees, Notes, and Action Items."
    
    # Initialize API based on provider
    model = None
    if args.api_provider == "gemini":
        if not GEMINI_AVAILABLE:
            print("* Error: Gemini API libraries not installed. Run: pip install google-generativeai")
            return
        if not args.api_key:
            print("* Error: API key is required for Gemini provider.")
            return
        genai.configure(api_key=args.api_key)
        model = genai.GenerativeModel(args.model)
    elif args.api_provider == "ollama":
        if not REQUESTS_AVAILABLE:
            print("* Error: requests library not installed. Run: pip install requests")
            return
        # Check if Whisper is available
        if not WHISPER_AVAILABLE:
            # Check if CLI is available
            try:
                subprocess.run(["whisper", "--help"], capture_output=True, timeout=5)
            except (FileNotFoundError, subprocess.TimeoutExpired):
                print("* Error: Whisper not available. Install with: pip install openai-whisper")
                return
        print(f"* Using Ollama API at {args.api_base} with model {args.model}")
        print(f"* Using Whisper ({args.whisper_model}) for transcription")
    else:
        print(f"* Error: API provider '{args.api_provider}' is not supported.")
        return

    # Supported audio formats (case-insensitive)
    supported_formats = ('.m4a', '.mp3', '.wav', '.ogg', '.flac')
    
    audio_files = []
    if os.path.isfile(input_path):
        file_ext = os.path.splitext(input_path)[1].lower()
        if file_ext in supported_formats:
            audio_files = [os.path.basename(input_path)]
            input_dir = os.path.dirname(input_path) or '.'
        else:
            print(f"* Error: '{input_path}' is not a supported audio file. Supported formats: {', '.join(supported_formats)}")
            return
    elif os.path.isdir(input_path):
        audio_files = [
            f for f in os.listdir(input_path) 
            if os.path.splitext(f)[1].lower() in supported_formats
        ]
        input_dir = input_path
    else:
        print(f"* Error: '{input_path}' is neither a file nor a directory.")
        return

    if not audio_files:
        print(f"* No audio files found in '{input_path}'. Supported formats: {', '.join(supported_formats)}")
        return

    # Map file extensions to MIME types
    mime_type_map = {
        '.m4a': 'audio/mp4',
        '.mp3': 'audio/mpeg',
        '.wav': 'audio/wav',
        '.ogg': 'audio/ogg',
        '.flac': 'audio/flac'
    }

    for file in audio_files:
        full_path = os.path.join(input_dir, file)
        file_ext = os.path.splitext(file)[1].lower()
        mime_type = mime_type_map.get(file_ext, 'audio/mpeg')
        
        if args.api_provider == "gemini":
            summarize_with_gemini(model, prompt, full_path, mime_type, args.retry_delay, file)
        elif args.api_provider == "ollama":
            process_with_ollama(
                args.api_base, 
                args.model, 
                args.api_key, 
                prompt, 
                full_path, 
                args.whisper_model,
                args.retry_delay, 
                file
            )


if __name__ == "__main__":
    main()
