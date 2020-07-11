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
      [Yy]* ) brew bundle --file="~/teton/local/share/bootstrap/Brewfile"
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

if [[ "${BASH_SOURCE[0]}" = "${0}" ]]; then
  main
fi

# Local Variables:
# mode: shell-script
# sh-basic-offset: 2
# End:
