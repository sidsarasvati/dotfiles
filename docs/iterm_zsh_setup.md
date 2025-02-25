# iTerm2 + Zsh: Emacs-style Keybindings & Minimal Setup

This guide documents how to configure iTerm2 and Zsh on macOS to behave like Emacs with keybindings for word navigation and deletion, along with a minimal aesthetic setup using the Tango Light/Dark themes and JetBrains Mono font.

## 1️⃣ Install iTerm2
If you haven't installed iTerm2 yet, use Homebrew:
```sh
brew install --cask iterm2
```

## 2️⃣ Configure iTerm2 to Send Meta Key
By default, macOS does not properly send **Meta (`⌥` / Option)** key sequences.

1. Open **iTerm2 > Preferences (`Cmd + ,`)**.
2. Go to **Profiles > Keys**.
3. Under **"Left Option Key" and "Right Option Key"**, set both to **"Esc+"**.

## 3️⃣ Set Tango Light/Dark Theme
Tango Light and Tango Dark are built-in iTerm2 themes that provide a clean, high-contrast aesthetic.

1. Open **iTerm2 > Preferences > Profiles > Colors**.
2. Click **Color Presets...** and select either **Tango Light** or **Tango Dark** based on your preference.

## 4️⃣ Set Minimal Font & Padding
For a clean and readable font, install JetBrains Mono:
```sh
brew tap homebrew/cask-fonts
brew install --cask font-jetbrains-mono-nerd-font
```
Then, in **iTerm2 > Preferences > Profiles > Text**:
- Set **Font** to `JetBrains Mono Nerd Font`.
- Set **Font Size** to `14`.
- Under **Window**, set **Padding** to `10px` for a clean UI.

## 5️⃣ Add Emacs-Style Keybindings in Zsh
Edit your `~/.zshrc` and add:

```sh
# Move cursor back/forward by word
bindkey "^[b" backward-word    # ⌥ + B → Move cursor back one word
bindkey "^[f" forward-word     # ⌥ + F → Move cursor forward one word

# Delete word backward/forward
bindkey "^[" backward-kill-word  # ⌥ + ⌫ → Delete previous word
bindkey "^[d" kill-word          # ⌥ + D → Delete next word
```

Apply changes:
```sh
source ~/.zshrc
```

## 6️⃣ (Optional) Install Oh My Zsh for More Features
If you haven't already installed Oh My Zsh:
```sh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```

## 7️⃣ Test the Keybindings & Aesthetic Setup
Now, restart iTerm2 (or open a new tab) and try:
- `⌥ + B` → Move **back** one word.
- `⌥ + F` → Move **forward** one word.
- `⌥ + ⌫` → **Delete** the previous word.
- `⌥ + D` → **Delete** the next word.

---
### 🎉 Done! Enjoy a Clean, Emacs-Friendly & Minimal iTerm2
This setup makes iTerm2 behave **just like Emacs** while keeping everything minimal and visually appealing with **Tango Light/Dark themes & JetBrains Mono font**. 🚀