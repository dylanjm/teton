#!/usr/bin/env bash

function bootstrap-homebrew() {
    # Check for Homebrew
    if [[ ! -e "$(command -v brew)" ]]; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    else
        brew upgrade >/dev/null
    fi
}

function bootstrap-bundle() {
    printf "\n%s\n" "Would you like to upgrade/install dependencies from Brewfile?"
    PS3="> "
    select opt in "Yes" "No"; do
        case "$REPLY" in
        1)
            printf "%b\n" "Installing Brewfile"
            brew bundle --file="$(pwd)/bootstrap/Brewfile"
            break
            ;;
        2)
            printf "%b\n\n" "Skipping Brewfile install"
            break
            ;;
        *)
            printf "%s\n" "Invalid option! 1 for Yes, 2 for No"
            continue
            ;;
        esac
    done

}

main() {
    bootstrap-homebrew
    bootstrap-bundle
}

main
