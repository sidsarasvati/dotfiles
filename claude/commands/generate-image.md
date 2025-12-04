# Generate/Edit/Compose Images with Gemini

Unified command for all image generation tasks. Auto-detects mode based on input.

## Usage
```
/generate-image <prompt>                          → Generate new image
/generate-image <image> "instruction"             → Edit image
/generate-image "instruction" <img1> <img2> ...   → Compose multiple images
```

## Mode Detection

**1. GENERATE** - No images provided
- Just text prompt → create from scratch
- Example: `/generate-image a wizard cat`

**2. EDIT** - One image provided
- Pasted image in chat (you can see it) → save to /tmp, then edit
- File path provided → edit that file
- Example: `/generate-image photo.png "add a rainbow"`
- Example: User pastes image + "make it look like Van Gogh"

**3. COMPOSE** - Multiple images provided
- Combine/merge multiple images
- Example: `/generate-image "group photo" person1.png person2.png`

## Handling Pasted Images

When user pastes an image in chat (appears as [Image] or you can see it):
1. Save it to temp file: `/tmp/input_image.png`
2. Use the Read tool to save the image data
3. Then run edit_image.py with that temp file

## Scripts

```bash
# Generate
python ~/.claude/skills/gemini-imagegen/scripts/generate_image.py "prompt" output.png

# Edit
python ~/.claude/skills/gemini-imagegen/scripts/edit_image.py input.png "instruction" output.png

# Compose
python ~/.claude/skills/gemini-imagegen/scripts/compose_images.py "instruction" output.png img1.png img2.png
```

## Smart Defaults

- **Output filename**: Derive from content (e.g., "wizard_cat.png", "edited_photo.png")
- **Aspect ratio**: Infer from context
  - Logos → 1:1
  - Landscapes → 16:9
  - Portraits → 9:16
  - Social media → platform-specific
- **Model**: Pro by default (gemini-3-pro-image-preview)

## Options

All scripts support:
- `--model`: `gemini-3-pro-image-preview` (default, quality) or `gemini-2.5-flash-image` (fast)
- `--aspect`: 1:1, 16:9, 9:16, 2:3, 3:2, 4:3, 3:4, 4:5, 5:4, 21:9
- `--size`: 1K, 2K, 4K (4K only on Pro)

## Examples

### Generate
```
/generate-image a majestic lion, photorealistic
→ python ~/.claude/skills/gemini-imagegen/scripts/generate_image.py "a majestic lion, photorealistic" lion.png
```

### Edit (file path)
```
/generate-image /tmp/lion.png "add a golden crown"
→ python ~/.claude/skills/gemini-imagegen/scripts/edit_image.py /tmp/lion.png "add a golden crown" lion_crowned.png
```

### Edit (pasted image)
```
User: /generate-image [pastes image] make it look like a watercolor painting
→ Save pasted image to /tmp/input_image.png
→ python ~/.claude/skills/gemini-imagegen/scripts/edit_image.py /tmp/input_image.png "make it look like a watercolor painting" watercolor_output.png
```

### Compose
```
/generate-image "create a team photo" alice.png bob.png charlie.png
→ python ~/.claude/skills/gemini-imagegen/scripts/compose_images.py "create a team photo" team_photo.png alice.png bob.png charlie.png
```

## Requirements

- `GEMINI_API_KEY` environment variable
- Python packages: `google-genai`, `pillow`
