import os
import argparse
import time
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
    parser = argparse.ArgumentParser(description="Summarize m4a meeting files using Gemini API.")
    parser.add_argument("path", type=str, help="Path to a single m4a file or a folder containing m4a files.")
    parser.add_argument("--prompt", type=str, default="summarize this meeting under date|title with attendees, notes and action items",
                        help="Custom prompt for summarization.")
    parser.add_argument("--api_key", type=str, required=True, help="Gemini API key.")
    parser.add_argument("--model", type=str, default="gemini-1.5-flash",
                        help="Gemini model to use (e.g., gemini-2.5-pro, gemini-2.5-flash).")
    parser.add_argument("--retry_delay", type=int, default=47,
                        help="Delay in seconds between retries after hitting rate limit.")
    args = parser.parse_args()

    input_path = os.path.expanduser(args.path)
    if not os.path.exists(input_path):
        print(f"* Error: Path '{input_path}' does not exist.")
        return

    genai.configure(api_key=args.api_key)
    model = genai.GenerativeModel(args.model)

    m4a_files = []
    if os.path.isfile(input_path):
        if input_path.lower().endswith('.m4a'):
            m4a_files = [os.path.basename(input_path)]
            input_dir = os.path.dirname(input_path) or '.'
        else:
            print(f"* Error: '{input_path}' is not an m4a file.")
            return
    elif os.path.isdir(input_path):
        m4a_files = [f for f in os.listdir(input_path) if f.lower().endswith('.m4a')]
        input_dir = input_path
    else:
        print(f"* Error: '{input_path}' is neither a file nor a directory.")
        return

    if not m4a_files:
        print(f"* No m4a files found in '{input_path}'.")
        return

    for file in m4a_files:
        full_path = os.path.join(input_dir, file)
        retries = 3
        while retries > 0:
            try:
                print(f"* Processing {file} *")
                uploaded_file = genai.upload_file(full_path, mime_type='audio/mp4')
                response = model.generate_content([args.prompt, uploaded_file])
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