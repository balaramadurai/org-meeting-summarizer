import os
import argparse
import time
from datetime import datetime
import google.generativeai as genai
from google.api_core.exceptions import ResourceExhausted

def format_output(text):
    """Format output for Org-mode: make Summary for ... bold, replace * with - for other lines."""
    lines = text.split('\n')
    formatted_lines = []
    for line in lines:
        stripped_line = line.strip()
        # Make Summary for ... bold with **
        if stripped_line.startswith('* Summary for'):
            line = line.replace('* Summary for', '**Summary for')
            line = line.replace('** Summary for', '**Summary for')  # Ensure no double **
            formatted_lines.append(line + '**')
        else:
            # Replace * at start of line with - for non-headings
            if stripped_line.startswith('*'):
                line = line.replace('*', '-', 1)
            # Replace ** with * for other headings
            if stripped_line.startswith('**'):
                line = line.replace('**', '*', 1)
            line = line.replace('**', '*')
            formatted_lines.append(line)
    return '\n'.join(formatted_lines)

def main():
    parser = argparse.ArgumentParser(
        description="Summarize audio meeting files (m4a, mp3, etc.) using Gemini API or compatible AI service."
    )
    parser.add_argument("path", type=str, help="Path to a single audio file or a folder containing audio files.")
    parser.add_argument(
        "--prompt", 
        type=str, 
        default=None,
        help="Custom prompt for summarization. If not provided, uses default with today's date."
    )
    parser.add_argument("--api_key", type=str, required=True, help="API key for the AI service.")
    parser.add_argument(
        "--model", 
        type=str, 
        default="gemini-2.5-flash",
        help="AI model to use (default: gemini-2.5-flash). Examples: gemini-2.5-pro, gemini-2.5-flash, gemini-1.5-flash"
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
        choices=["gemini"],
        help="AI API provider (default: gemini). Currently supports: gemini. Other providers can be added."
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
    
    # Initialize API (currently Gemini, extensible for others)
    if args.api_provider == "gemini":
        genai.configure(api_key=args.api_key)
        model = genai.GenerativeModel(args.model)
    else:
        print(f"* Error: API provider '{args.api_provider}' is not yet supported.")
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
                break
            except ResourceExhausted as e:
                retries -= 1
                if retries == 0:
                    print(f"* Error processing {file}: Exhausted retries after hitting rate limit. {str(e)}")
                    break
                print(f"* Rate limit hit for {file}. Retrying after {args.retry_delay} seconds... ({retries} retries left)")
                time.sleep(args.retry_delay)
            except Exception as e:
                print(f"* Error processing {file}: {str(e)}")
                break

if __name__ == "__main__":
    main()
