#!/usr/bin/env python3
"""
Interactive multi-turn chat for iterative image generation and refinement.

Usage:
    python multi_turn_chat.py [--model MODEL]

Examples:
    python multi_turn_chat.py
    python multi_turn_chat.py --model gemini-3-pro-image-preview

Commands during chat:
    save <filename>  - Save current image
    reset           - Start new conversation
    quit/exit       - Exit chat

Environment:
    GEMINI_API_KEY - Required API key
"""

import argparse
import os
import sys

from google import genai
from google.genai import types


def run_chat(model: str = "gemini-2.5-flash-image"):
    """Run interactive multi-turn image chat."""
    api_key = os.environ.get("GEMINI_API_KEY")
    if not api_key:
        raise EnvironmentError("GEMINI_API_KEY environment variable not set")

    client = genai.Client(api_key=api_key)

    chat = client.chats.create(
        model=model,
        config=types.GenerateContentConfig(response_modalities=["TEXT", "IMAGE"]),
    )

    current_image = None
    image_count = 0

    print(f"Gemini Image Chat ({model})")
    print("Commands: save <filename>, reset, quit/exit")
    print("-" * 40)

    while True:
        try:
            user_input = input("\nYou: ").strip()
        except (EOFError, KeyboardInterrupt):
            print("\nGoodbye!")
            break

        if not user_input:
            continue

        # Handle commands
        if user_input.lower() in ("quit", "exit"):
            print("Goodbye!")
            break

        if user_input.lower() == "reset":
            chat = client.chats.create(
                model=model,
                config=types.GenerateContentConfig(response_modalities=["TEXT", "IMAGE"]),
            )
            current_image = None
            image_count = 0
            print("Chat reset. Starting fresh.")
            continue

        if user_input.lower().startswith("save "):
            filename = user_input[5:].strip()
            if current_image:
                current_image.save(filename)
                print(f"Image saved to: {filename}")
            else:
                print("No image to save yet.")
            continue

        # Send message to model
        try:
            response = chat.send_message(user_input)

            text_parts = []
            for part in response.parts:
                if part.text:
                    text_parts.append(part.text)
                elif part.inline_data:
                    current_image = part.as_image()
                    image_count += 1
                    auto_filename = f"chat_image_{image_count}.png"
                    current_image.save(auto_filename)
                    print(f"\n[Image generated: {auto_filename}]")

            if text_parts:
                print(f"\nGemini: {' '.join(text_parts)}")

        except Exception as e:
            print(f"\nError: {e}")


def main():
    parser = argparse.ArgumentParser(
        description="Interactive multi-turn image chat with Gemini",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument(
        "--model", "-m",
        default="gemini-2.5-flash-image",
        choices=["gemini-2.5-flash-image", "gemini-3-pro-image-preview"],
        help="Model to use (default: gemini-2.5-flash-image)"
    )

    args = parser.parse_args()

    try:
        run_chat(model=args.model)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
