#!/usr/bin/env bash

brew_shell() {
  local shell="${1}"
  [[ ! -e "/usr/local/bin/${shell}" ]] && brew install "${shell}"
}

allowed_shells() {
  local shell="${1}"
  ! grep -Fxq "/usr/local/bin/${shell}" /etc/shells && {
    echo "/usr/local/bin/${shell}" | sudo tee -a /etc/shells 1>/dev/null
  }
}

change_shell() {
  local shell="${1}"
  [[ "${SHELL}" != "/usr/local/bin/${shell}" ]] &&
      sudo chsh -s "/usr/local/bin/${shell}" "$USER"
}

bootstrap_shell() {
  local shell="${1}"
  brew_shell "${shell}"
  allowed_shells "${shell}"
  change_shell "${shell}"
}

main() {
  printf "\n%s\n" "Please select a default shell:"
  PS3="> "
  options=("zsh" "bash" "fish" "skip shell config")

  select opt in "${options[@]}"; do
    case "$REPLY" in
    1)
      printf "\n%b\n" "Setting up $opt:"
      bootstrap_shell "$opt"
      break
      ;;
    2)
      printf "\n%b\n" "Setting up $opt:"
      bootstrap_shell "$opt"
      break
      ;;
    3)
      printf "\n%b\n" "Setting up $opt:"
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
}

if [[ "${BASH_SOURCE[0]}" = "${0}" ]]; then
  main "${@}"
fi
