typeset -aU path cdpath fpath manpath
export CACHE_HOME="$HOME/.cache"
export DOTFILES="$HOME/dotz"
export ZSH="$DOTFILES/shells/zsh"
export MODULEPATH="/usr/local/Cellar/modules/4.3.0/modulefiles:/opt/moose/Modules/3.2.10/modulefiles"
export MOOSE_DIR="$HOME/Documents/projects/bison/moose"

fpath+=($ZSH/functions)

export PYENV_ROOT="$HOME/.pyenv"
export EDITOR='emacsclient -c'
export ALTERNATE_EDITOR=""
export LANG=en_US.UTF-8
export EXA_COLORS="bd=31:cd=32:pi=34"

path=($HOME/.pyenv/bin
      $HOME/.pyenv/shims
      /usr/local/bin
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      $DOTFILES/bin
      /usr/local/texlive/2019/bin/x86_64-darwin/
      /opt/X11/bin
      /usr/local/Cellar/modules/4.3.0/bin)

export PATH

autoload -Uz time-shell countdown nuke convert-to-md colorlist prettycsv em ptyless
