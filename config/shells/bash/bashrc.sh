#!/usr/bin/env bash

export CACHE="$HOME/.cache"
export DOTFILES="$HOME/teton"
export PYENV_ROOT="$HOME/.pyenv"

export PATH="$PYENV_ROOT/bin:$PYENV_ROOT/shims:$DOTFILES/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/texlive/2019/bin/x86_64-darwin:/usr/local/opt/texinfo/bin:$PATH"

# ~/.bashrc: executed by bash(1) for non-login shells.

# shellcheck source=settings/lscl.bash
source "$DOTFILES/shells/bash/settings/lscl.bash"

# shellcheck source=settings/history.bash
source "$DOTFILES/shells/bash/settings/history.bash"

# shellcheck source=settings/defaults.bash
source "$DOTFILES/shells/bash/settings/defaults.bash"

# shellcheck source=settings/navigation.bash
source "$DOTFILES/shells/bash/settings/navigation.bash"

# shellcheck source=plugins/git_completion.bash
source "$DOTFILES/shells/bash/plugins/git_completion.bash"

export LESS="-R -F -i -J -M -R -W -x4 -X -z-4"
export LESSOPEN="|lessfilter %s"
export LESS_ADVANCED_PREPROCESSOR=1

alias ls='gls -la -G --color=tty --group-directories-first'
alias lx='exa -lah --icons --time=changed --time-style long-iso --group-directories-first'
alias notifyDone='terminal-notifier -title "Terminal" -message "Finished"'
alias dot='cd $DOTFILES'

alias proj='cd ~/Documents/projects'
alias bis='cd ~/Documents/projects/bison'
alias bisd='cd ~/Documents/projects/bison_data'
alias dotbin='cd $DOTFILES/bin'
alias valid='cd ~/Documents/projects/bison/assessment/LWR/validation'
alias moose='cd ~/Documents/projects/moose'

eval "$(starship init bash)"
