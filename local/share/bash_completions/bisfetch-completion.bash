#!/usr/bin/env bash

_bisfetch_completions() {
  mapfile -t COMPREPLY < <(compgen -W \
                                   "$(find "$HOME/Documents/projects/bison/assessment/LWR/validation" \
                                   -type d -mindepth 1 -maxdepth 1 -exec basename {} \;)" \
                                   -- "${COMP_WORDS[1]}")
}

complete -F _bisfetch_completions bisfetch
