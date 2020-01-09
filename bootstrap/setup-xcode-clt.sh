#!/usr/bin/env bash

NORMAL='\033[0m'
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'

marker="${BLUE}==>${NORMAL}"
status="…"
status_good="${GREEN}✔${NORMAL}"
status_bad="${RED}✘${NORMAL}"

function bootstrap-xcode-clt () {
  # Check for xcode-CLT
  printf "%b\r" "$marker Checking for Command Line Tools - $status"
  if [ "$(xcode-select -p 2>/dev/null; echo $?)" = "2" ]; then

    printf "%b\n" "$marker Checking for Command Line Tools - $status_bad"

    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress

    PROD=$(softwareupdate -l |
             grep "\*.*Command Line" |
             head -n 1 |
             awk -F"*" '{print $2}' |
             sed -e 's/^ *//' |
             tr -d '\n')

    softwareupdate -i "$PROD" --verbose

    rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress

  else

    printf "%b\n" "$marker Checking for Command Line Tools - $status_good"

  fi
}

function install-clt () {
  touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress

  PROD=$(softwareupdate -l |
           grep "\*.*Command Line" |
           head -n 1 |
           awk -F"*" '{print $2}' |
           sed -e 's/^ *//' |
           tr -d '\n')

  softwareupdate -i "$PROD" --verbose

  rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
}

bootstrap-xcode-clt
