#!/usr/bin/env bash

declare -a arr=(
  # Iosevka Font - Great Monospace and Proportional Font
  "https://github.com/be5invis/Iosevka/releases/download/v3.2.2/ttf-iosevka-curly-3.2.2.zip"
  "https://github.com/be5invis/Iosevka/releases/download/v3.2.2/ttf-iosevka-term-curly-3.2.2.zip"
  "https://github.com/be5invis/Iosevka/releases/download/v3.2.2/pkg-iosevka-sparkle-3.2.2.zip"

  # Symbola - Great Unicode Support
  "https://fontlibrary.org/assets/downloads/symbola/cf81aeb303c13ce765877d31571dc5c7/symbola.zip"

  # XITS - Great Math Support
  "https://fontlibrary.org/assets/downloads/xits-math/ac15a89f7e6aa3dccd97957dd9615c89/xits-math.zip"
)

remove-old-fonts() {
  rm "${HOME}/Library/Fonts/iosevka-*"
  rm "${HOME}/Library/Fonts/symbola*"
  rm "${HOME}/Library/Fonts/xits*"
}

move_font() {
  local fontname="${1}"
  if [[ -e "${HOME}/Library/Fonts/$(basename "${fontname}")" ]]; then
    printf "Skipping %s\n" "$(basename "${fontname}")"
  else
    printf "Installing %s to ~/Library/Fonts\n" "$(basename "${fontname}")"
    cp "${fontname}" "${HOME}/Library/Fonts/"
  fi
}

create_tmp_dir() {
  local fonturl="${1}"
  printf "%s\n" "${fonturl}"
  TMPFILE=$(mktemp)
  TMPDIR="/tmp/font-temp"
  curl "${fonturl}" -L -o "${TMPFILE}" && unzip -q "${TMPFILE}" -d "${TMPDIR}"
  rm "${TMPFILE}"
}

install_fonts() {
  local i
  for i in "${arr[@]}"; do
    create_tmp_dir "${i}"

    while read -r line; do
      move_font "${line}"
    done < <(find -E "${TMPDIR}"/* \
                  -iregex '.*\.(ttf|ttc|otf)' -not -path "*/ttf-unhinted/*")

    rm -rf "${TMPDIR}"
  done
}

main() {
  install_fonts
}

if [[ "${BASH_SOURCE[0]}" = "${0}" ]]; then
  main
fi

# Local Variables:
# mode: shell-script
# sh-basic-offset: 2
# End:
