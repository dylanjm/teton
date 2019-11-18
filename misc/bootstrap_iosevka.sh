#!/bin/bash

declare -a arr=("https://github.com/be5invis/Iosevka/releases/download/v2.3.3/02-iosevka-term-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/03-iosevka-type-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/experimental-iosevka-aile-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/experimental-iosevka-etoile-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/iosevka-ss05-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/iosevka-term-ss09-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/01-iosevka-2.3.3.zip"
                "https://github.com/be5invis/Iosevka/releases/download/v2.3.3/04-iosevka-cc-2.3.3.zip"
               )

for i in "${arr[@]}"; do
    echo "Installing $(basename $i) to ~/Library/Fonts"
    TMPFILE=`mktemp`
    PWD=`pwd`
    curl -s "$i" -L -o $TMPFILE && unzip -q $TMPFILE -d "/tmp/font-temp"
    rm $TMPFILE

    find /tmp/font-temp/* -type f -name "*.ttf" -not -path "./ttf-unhinted/*" |\
        xargs -I{} cp {} ~/Library/Fonts

    rm -rf /tmp/font-temp
done
