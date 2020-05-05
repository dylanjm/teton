#!/usr/bin/env bash

bootstrap-homebrew() {
    # Check for Homebrew
    if [[ ! -e "$(command -v brew)" ]]; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    else
        brew upgrade > /dev/null
    fi
}

bootstrap-bundle() {
    while true; do
        read -r -p "Do you wish to install programs from Brewfile? [Yes/No] " yn
        case $yn in
            [Yy]* ) brew bundle --file="$(pwd)/bootstrap/Brewfile"; break;;
            [Nn]* ) break;;
            * ) echo "Please answer yes or no.";;
        esac
    done
}

main() {
    bootstrap-homebrew
    bootstrap-bundle
}

main
