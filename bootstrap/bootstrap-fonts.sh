#!/bin/bash

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
