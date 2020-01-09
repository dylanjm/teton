#!/usr/bin/env bash

NORMAL='\033[0m'
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'

marker="${BLUE}==>${NORMAL}"
status="…"
status_good="${GREEN}✔${NORMAL}"
status_bad="${RED}✘${NORMAL}"

function brew_shell() {
    printf "%b\r" "$marker Checking for $1 installation - $status"
    if test ! -e "/usr/local/bin/$1"; then
        printf "%b\n" "$marker Checking for $1 installation - $status_bad"
        printf "%s\n\n" "Installing latest version of $1 from homebrew"
        brew install "$1"
    else
        printf "%b\n" "$marker Checking for $1 installation - $status_good"
    fi

}

function allowed_shells() {
    printf "%b\r" "$marker Checking /etc/shells - $status"
    if ! grep -Fxq "/usr/local/bin/$1" /etc/shells; then
        printf "%b\n" "$marker Checking /etc/shells - $status_bad"
        printf "Adding /usr/local/bin/%s to /etc/shells\n" "$1"
        echo "/usr/local/bin/$1" | sudo tee -a /etc/shells 1>/dev/null
    else
        printf "%b\n" "$marker Checking /etc/shells - $status_good"
    fi
}

function change_shell() {
    printf "%b\r" "$marker Checking \$SHELL - $status"
    if [ "$SHELL" != "/usr/local/bin/$1" ]; then
        printf "%b\n" "$marker Checking \$SHELL - $status_bad"
        sudo chsh -s "/usr/local/bin/$1" "$USER"
        printf "%s is now your default shell. Please restart your terminal once this script has finished\n" "$1"
    else
        printf "%b\n" "$marker Checking \$SHELL - $status_good"
    fi
}

function bootstrap_shell() {
    brew_shell "$1"
    allowed_shells "$1"
    change_shell "$1"
}

printf "\n%s\n" "Please select a default shell:"
PS3="> "
options=("zsh" "bash" "fish" "skip shell config")

select opt in "${options[@]}"; do
    case "$REPLY" in
    1)
        printf "\n%b\n" "Setting up ${GREEN}$opt${NORMAL}:"
        bootstrap_shell "$opt"
        break
        ;;
    2)
        printf "\n%b\n" "Setting up ${GREEN}$opt${NORMAL}:"
        bootstrap_shell "$opt"
        break
        ;;
    3)
        printf "\n%b\n" "Setting up ${GREEN}$opt${NORMAL}:"
        bootstrap_shell "$opt"
        break
        ;;
    4)
        printf "\n%s\n" "Skipping Shell Config..."
        break
        ;;
    *)
        echo "Invalid option. Try another one."
        continue
        ;;
    esac
done
