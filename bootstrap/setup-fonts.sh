#!/usr/bin/env bash

NORMAL='\033[0m'
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'

marker="${BLUE}==>${NORMAL}"
status="…"
status_good="${GREEN}✔${NORMAL}"
status_bad="${RED}✘${NORMAL}"

declare -a arr=("https://github.com/be5invis/Iosevka/releases/download/v2.3.3/01-iosevka-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/02-iosevka-term-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/03-iosevka-type-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/04-iosevka-cc-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/experimental-iosevka-aile-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/experimental-iosevka-etoile-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/iosevka-ss05-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/iosevka-ss09-2.3.3.zip"
                "https://fontlibrary.org/assets/downloads/symbola/cf81aeb303c13ce765877d31571dc5c7/symbola.zip"
               )

printf "%b\r" "$marker Checking font installations - $status"
if [[ "$(test -d $HOME/Library/Fonts && find $HOME/Library/Fonts/* -name "iosevka-*" -type f | wc -l 2>/dev/null)" -lt 200 ]]; then
  printf "%b\n" "$marker Checking font installations - $status_bad"

  for i in "${arr[@]}"; do
    echo "Installing $(basename "$i") to ~/Library/Fonts"
    TMPFILE=$(mktemp)
    TMPDIR="/tmp/font-temp"
    PWD=$(pwd)
    curl -s "$i" -L -o "$TMPFILE" && unzip -q "$TMPFILE" -d "$TMPDIR"
    rm "$TMPFILE"

    find "$TMPDIR"/* -type f -name "*.ttf" -not -path "*/ttf-unhinted/*" -exec cp {} ~/Library/Fonts \;

    rm -rf /tmp/font-temp
  done

else
  printf "%b\n" "$marker Checking font installations - $status_good"
fi
