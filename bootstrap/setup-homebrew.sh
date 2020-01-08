#!/usr/bin/env bash

NORMAL='\033[0m'
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'

marker="${BLUE}==>${NORMAL}"
status="…"
status_good="${GREEN}✔${NORMAL}"
status_bad="${RED}✘${NORMAL}"

function bootstrap-homebrew () {
  # Check for Homebrew
  printf "%b\r" "$marker Checking for Homebrew installation - $status"
  if [[ ! -e "$(command -v brew)" ]]; then
    printf "%b\n" "$marker Checking for Homebrew installation - $status_bad"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  else
    printf "%b\n" "$marker Checking for Homebrew installation - $status_good"
    brew upgrade > /dev/null
  fi
}

function bootstrap-bundle () {
  printf "\n%s\n" "Would you like to upgrade/install dependencies from Brewfile?"
  PS3="> "
  select opt in "Yes" "No"; do
    case "$REPLY" in
      1)
        printf "%b\n" "Installing Brewfile $status"
        brew bundle --file="$(pwd)/bootstrap/Brewfile"
        break
        ;;
      2)
        printf "%b\n\n" "Skipping Brewfile install $status"
        break
        ;;
      *)
        printf "%s\n" "Invalid option! 1 for Yes, 2 for No"
        continue
        ;;
    esac
  done


}

bootstrap-homebrew
bootstrap-bundle
