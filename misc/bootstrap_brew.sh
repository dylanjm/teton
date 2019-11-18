#!/usr/bin/env bash

sudo -v

# Check for xcode-CLT
if [ "$(xcode-select -p 2>/dev/null; echo $?)" = "2" ]; then
  echo "Installing Xcode Command Line Tools for you."
  touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress;
  PROD=$(softwareupdate -l |
      grep "\*.*Command Line" |
      head -n 1 | awk -F"*" '{print $2}' |
      sed -e 's/^ *//' |
      tr -d '\n')
  softwareupdate -i "$PROD" --verbose
  rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
fi

set -e

# Check for Homebrew
if test ! $(which brew); then
  echo "Installing Homebrew for you."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

if test ! -e /usr/local/bin/zsh; then
   echo "Installing latest version of ZSH from homebrew"
   brew install zsh
fi

if ! grep -Fxq "/usr/local/bin/zsh" /etc/shells; then
    echo "Adding /usr/local/bin/zsh to /etc/shells file"
    echo '/usr/local/bin/zsh' | sudo tee -a /etc/shells
fi

if [ "$SHELL" != "/usr/local/bin/zsh" ]; then
    chsh -s /usr/local/bin/zsh
    echo "ZSH is now your default shell. Please restart your terminal once this script has finished"
fi
