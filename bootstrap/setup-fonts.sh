#!/usr/bin/env bash

NORMAL='\033[0m'
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'

marker="${BLUE}==>${NORMAL}"
status_good="${GREEN}✔${NORMAL}"
status_bad="${RED}✘${NORMAL}"

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


mapfile -t fonts < <(for i in ${arr[@]}; do
                       echo "$(basename $i)" |
                         sed 's/.zip//g' |
                         sed 's/-3.0.0-alpha.4//g'
                     done)





if [[ "$(test -d "$HOME/Library/Fonts && find $HOME/Library/Fonts/*" -name "iosevka-*" -type f | wc -l 2>/dev/null)" -lt 200 ]]; then

  printf "%b\n" "$marker Checking font installations - $status_bad"

  for i in "${arr[@]}"; do
    TMPFILE=$(mktemp)
    TMPDIR="/tmp/font-temp"
    PWD=$(pwd)
    curl -s "$i" -L -o "$TMPFILE" && unzip -q "$TMPFILE" -d "$TMPDIR"
    rm "$TMPFILE"

    find -E "$TMPDIR"/* -iregex '.*\.(ttf|ttc|otf)' -not -path "*/ttf-unhinted/*" -exec sh -c 'echo "Installing $(basename {}) to ~/Library/Fonts"' \; -exec cp {} ~/Library/Fonts/ \;
    #find "$TMPDIR"/* -type f -name "*.ttf" -name "*.ttc" -not -path "*/ttf-unhinted/*" -exec cp {} ~/Library/Fonts \;

    rm -rf /tmp/font-temp
  done

else
  printf "%b\n" "$marker Checking font installations - $status_good"
fi
