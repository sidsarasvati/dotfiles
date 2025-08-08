#!/usr/bin/env python3
"""
Append thinking mode instructions based on flags at end of prompt.

Flags:
  -t   -> "think hard" (moderate computation boost)
  -th  -> "think harder" (more computation time)  
  -ut  -> "ultrathink" (maximum thinking budget)

The flag is stripped from the prompt and appropriate instruction is appended.
"""

import json
import sys
import re

def main():
    try:
        # Read input from stdin
        input_data = json.load(sys.stdin)
        prompt = input_data.get('prompt', '')
        
        # Check for thinking flags at the end
        # Match flags with optional whitespace before them
        flag_pattern = r'\s+(-t|-th|-ut)$'
        match = re.search(flag_pattern, prompt)
        
        if match:
            flag = match.group(1)
            # Strip the flag from the prompt
            clean_prompt = prompt[:match.start()].rstrip()
            
            # Map flags to thinking modes
            thinking_modes = {
                '-t': 'think hard',
                '-th': 'think harder',
                '-ut': 'ultrathink'
            }
            
            # Get the appropriate thinking instruction
            thinking_mode = thinking_modes.get(flag)
            
            if thinking_mode:
                # Output the thinking instruction
                # This becomes additionalContext appended to the prompt
                print(f'use {thinking_mode}. Take all the time you need. It\'s much better if you do too much research and thinking than not enough.')
                
                # Log to stderr for debugging (won't affect output)
                print(f'[append_thinking] Applied {thinking_mode} mode', file=sys.stderr)
        
    except Exception as e:
        print(f'append_thinking hook error: {str(e)}', file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()