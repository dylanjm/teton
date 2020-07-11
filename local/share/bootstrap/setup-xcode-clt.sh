#!/usr/bin/env bash

install-clt () {
  touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
  PROD=$(softwareupdate -l \
           | grep "\*.*Command Line" \
           | head -n 1 \
           | awk -F"*" '{print $2}' \
           | sed -e 's/^ *//' \
           | tr -d '\n')

  sudo softwareupdate -i "$PROD" --verbose
  rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
}

bootstrap-xcode-clt () {
  # Check for xcode-CLT
  [[ ! -d "$(xcode-select -p)" ]] && install-clt && return 0
  printf "command line tools already installed!\n"
}

bootstrap-xcode-clt
