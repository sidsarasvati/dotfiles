#!/usr/bin/env python3
"""
Compose multiple images into one using Gemini API.

Usage:
    python compose_images.py "instruction" output.png image1.png image2.png [image3.png ...]

Examples:
    python compose_images.py "Create a collage of these photos" collage.png photo1.png photo2.png photo3.png
    python compose_images.py "Combine these people into a group photo" group.png person1.png person2.png
    python compose_images.py "Merge these product shots into one banner" banner.png product1.png product2.png --aspect 21:9

Environment:
    GEMINI_API_KEY - Required API key
"""

import argparse
import os
import sys

from PIL import Image
from google import genai
from google.genai import types


def compose_images(
    instruction: str,
    image_paths: list[str],
    output_path: str,
    model: str = "gemini-3-pro-image-preview",
    aspect_ratio: str | None = None,
    image_size: str | None = None,
) -> str | None:
    """Compose multiple images into one.

    Args:
        instruction: Text description of how to combine images
        image_paths: List of input image paths (up to 14)
        output_path: Path to save the composed image
        model: Gemini model to use (Pro recommended)
        aspect_ratio: Output aspect ratio
        image_size: Output resolution

    Returns:
        Any text response from the model, or None
    """
    api_key = os.environ.get("GEMINI_API_KEY")
    if not api_key:
        raise EnvironmentError("GEMINI_API_KEY environment variable not set")

    if len(image_paths) > 14:
        raise ValueError("Maximum 14 images supported")

    # Verify all images exist
    for path in image_paths:
        if not os.path.exists(path):
            raise FileNotFoundError(f"Image not found: {path}")

    client = genai.Client(api_key=api_key)

    # Load images
    images = [Image.open(path) for path in image_paths]

    # Build config
    config_kwargs = {"response_modalities": ["TEXT", "IMAGE"]}

    image_config_kwargs = {}
    if aspect_ratio:
        image_config_kwargs["aspect_ratio"] = aspect_ratio
    if image_size:
        image_config_kwargs["image_size"] = image_size

    if image_config_kwargs:
        config_kwargs["image_generation_config"] = image_config_kwargs

    config = types.GenerateContentConfig(**config_kwargs)

    # Build contents: instruction + all images
    contents = [instruction] + images

    response = client.models.generate_content(
        model=model,
        contents=contents,
        config=config,
    )

    text_response = None
    image_saved = False

    # Access parts via candidates[0].content.parts
    if response.candidates and response.candidates[0].content.parts:
        for part in response.candidates[0].content.parts:
            if hasattr(part, 'text') and part.text:
                text_response = part.text
            elif hasattr(part, 'inline_data') and part.inline_data:
                # Data is already bytes, write directly
                with open(output_path, 'wb') as f:
                    f.write(part.inline_data.data)
                image_saved = True

    if not image_saved:
        raise RuntimeError("No image was generated. Check your instruction and try again.")

    return text_response


def main():
    parser = argparse.ArgumentParser(
        description="Compose multiple images using Gemini API",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument("instruction", help="Composition instruction")
    parser.add_argument("output", help="Output file path")
    parser.add_argument("images", nargs="+", help="Input image paths (2-14 images)")
    parser.add_argument(
        "--model", "-m",
        default="gemini-3-pro-image-preview",
        choices=["gemini-2.5-flash-image", "gemini-3-pro-image-preview"],
        help="Model to use (default: gemini-3-pro-image-preview)"
    )
    parser.add_argument(
        "--aspect", "-a",
        choices=["1:1", "2:3", "3:2", "3:4", "4:3", "4:5", "5:4", "9:16", "16:9", "21:9"],
        help="Output aspect ratio"
    )
    parser.add_argument(
        "--size", "-s",
        choices=["1K", "2K", "4K"],
        help="Output resolution"
    )

    args = parser.parse_args()

    if len(args.images) < 2:
        print("Error: At least 2 images required for composition", file=sys.stderr)
        sys.exit(1)

    try:
        text = compose_images(
            instruction=args.instruction,
            image_paths=args.images,
            output_path=args.output,
            model=args.model,
            aspect_ratio=args.aspect,
            image_size=args.size,
        )

        print(f"Composed image saved to: {args.output}")
        if text:
            print(f"Model response: {text}")

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
