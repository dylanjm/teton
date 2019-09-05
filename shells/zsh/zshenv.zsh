typeset -aU path cdpath fpath manpath
export CACHE_HOME="$HOME/.cache"
export DOTFILES="$HOME/dotz"
export ZSH="$DOTFILES/shells/zsh"
export ZSH_PYENV_LAZY_VIRTUALENV=true

fpath+=($ZSH/functions)

export PYENV_ROOT="$HOME/.pyenv"
export EDITOR='emacsclient -c'
export ALTERNATE_EDITOR=""
export LANG=en_US.UTF-8

export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$DOTFILES/bin"

export PATH_SAVE=$PATH
autoload -Uz time-shell countdown nuke convert-to-md config-powerline colorize colorlist prettycsv em


