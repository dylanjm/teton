#!/usr/bin/env bash

readonly BREW_URL="https://raw.githubusercontent.com/Homebrew/install/master/install.sh"

bootstrap-homebrew() {
  if [[ ! -e "$(command -v brew)" ]]; then
    /bin/bash -c "$(curl -fsSL "${BREW_URL}")"
  else
    brew upgrade
  fi
}

bootstrap-bundle() {
  while true; do
    read -r -p "Do you wish to install programs from Brewfile? [Yes/No] " yn
    case "${yn}" in
      [Yy]* ) brew bundle --file="$(pwd)/local/share/bootstrap/Brewfile"
              break
              ;;
      [Nn]* ) break
              ;;
      * ) echo "Please answer yes or no.";;
    esac
  done
}

main() {
  bootstrap-homebrew
  bootstrap-bundle
}

main

# Local Variables:
# mode: sh-script
# sh-indentation: 2
# End:
