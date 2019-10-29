typeset -aU path cdpath fpath manpath
export LANG=en_US.UTF-8
export TERM=xterm-256color

export EDITOR='emacsclient -a "" -c'

export CACHE_HOME="$HOME/.cache"
export HISTFILE="$CACHE_HOME/zsh/zsh_history"
export DOTFILES="$HOME/dotz"

export EMACSD="$DOTFILES/editors/emacs.d"
export ZSH="$DOTFILES/shells/zsh"

export MODULEPATH="/usr/local/Cellar/modules/4.3.0/modulefiles:/opt/moose/Modules/3.2.10/modulefiles"
export PYENV_ROOT="$HOME/.pyenv"

fpath+=($ZSH/functions)

path=($PYENV_ROOT/plugins/pyenv-virtualenv/shims
      $PYENV_ROOT/bin
      $PYENV_ROOT/shims
      /usr/local/bin
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      $DOTFILES/bin
      /usr/local/texlive/2019/bin/x86_64-darwin/
      /opt/X11/bin
      /usr/local/opt/texinfo/bin
      /usr/local/Cellar/modules/4.3.0/bin)

export PATH

autoload -Uz time-shell nuke convert-to-md colorlist
