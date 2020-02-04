#!/usr/bin/env bash

declare -a arr=(
  ###
  ### Iosevka Font - https://github.com/be5invis/Iosevka/releases
  ###
  "https://github.com/be5invis/Iosevka/releases/download/v3.0.0-alpha.4/iosevka-ss09-3.0.0-alpha.4.zip"           # Iosevka SS09 - Default
  "https://github.com/be5invis/Iosevka/releases/download/v3.0.0-alpha.4/iosevka-type-ss09-3.0.0-alpha.4.zip"      # Iosevka SS09 - Typesetting Font
  "https://github.com/be5invis/Iosevka/releases/download/v3.0.0-alpha.4/iosevka-term-lig-ss09-3.0.0-alpha.4.zip"  # Iosevka SS09 - Monospace w/ Ligatures
  "https://github.com/be5invis/Iosevka/releases/download/v3.0.0-alpha.4/iosevka-term-ss09-3.0.0-alpha.4.zip"      # Iosevka SS09 - Monospace w/o Ligatures
  "https://github.com/be5invis/Iosevka/releases/download/v3.0.0-alpha.4/ttc-iosevka-aile-3.0.0-alpha.4.zip"       # Iosevka Aile - Quasi Proportional
  "https://github.com/be5invis/Iosevka/releases/download/v3.0.0-alpha.4/ttc-iosevka-etoile-3.0.0-alpha.4.zip"     # Iosevka Etoile - Quasi Proportional Slab-Serif
  "https://github.com/be5invis/Iosevka/releases/download/v3.0.0-alpha.4/ttc-iosevka-sparkle-3.0.0-alpha.4.zip"    # Iosevka Sparkle - Quasi Proportional

  ###
  ### Symbola
  ###
  "https://fontlibrary.org/assets/downloads/symbola/cf81aeb303c13ce765877d31571dc5c7/symbola.zip"  # Great Unicode Support

  ###
  ### XITS
  ###
  "https://fontlibrary.org/assets/downloads/xits-math/ac15a89f7e6aa3dccd97957dd9615c89/xits-math.zip"  # Great Math Support
)

function move_font () {
  if [[ -e "$HOME/Library/Fonts/$(basename "$1")" ]]; then
      printf "Skipping %s" "$(basename "$1")"
      return
    fi
    printf "Installing %s to ~/Library/Fonts" "$(basename "$1")"
    # cp "$line" "$HOME/Library/Fonts"
}

for i in "${arr[@]}"; do
  TMPFILE=$(mktemp)
  TMPDIR="/tmp/font-temp"
  PWD=$(pwd)
  curl -s "$i" -L -o "$TMPFILE" && unzip -q "$TMPFILE" -d "$TMPDIR"
  rm "$TMPFILE"

  find -E "$TMPDIR"/* -iregex '.*\.(ttf|ttc|otf)' -not -path "*/ttf-unhinted/*" -print0 |
    xargs -0 -I{} sh -c 'if [[ -e "$HOME/Library/Fonts/$(basename {})" ]]; then printf "Not Insalled %s\n" "$(basename {})"; fi'

  # 'if [[ -e "$HOME/Library/Fonts/$(basename {})" ]]; then printf "Skipping %s\n" "$(basename {})"; else printf "Installing %s to ~/Library/Fonts\n" "$(basename {})"; cp {} $HOME/Library/Fonts; fi'


  rm -rf "$TMPDIR"
done
