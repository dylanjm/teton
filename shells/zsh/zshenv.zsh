typeset -aU path cdpath fpath manpath module_path
typeset -A ZPLGM

export LANG=en_US.UTF-8

export CACHE="$HOME/.cache"
export DOTFILES="$HOME/dotz"
export PYENV_ROOT="$HOME/.pyenv"
export EMACSD="$DOTFILES/editors/emacs.d"
export EDITOR='emacsclient -a "" -c'

export ZSH="$DOTFILES/shells/zsh"
export ZSH_CACHE="$CACHE/zsh"
export HISTFILE="$ZSH_CACHE/zsh_history"

export GPG_TTY=$(tty)

export ZPLGM[HOME_DIR]="$ZSH_CACHE/zplugin"
export ZPLGM[BIN_DIR]="$ZPLGM[HOME_DIR]/bin"
export ZPLGM[ZCOMPDUMP_PATH]="$ZSH_CACHE/zcompdump"

fpath+=($ZSH/functions)
module_path+=($HOME/.cache/zsh/zplugin/bin/zmodules/Src)

path=($ZPLGM[HOME_DIR]/plugins/direnv---direnv/direnv
      $ZPLGM[HOME_DIR]/polaris/bin
      $PYENV_ROOT/bin
      $PYENV_ROOT/shims
      $DOTFILES/bin
      /usr/local/bin
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      /usr/local/texlive/2019/bin/x86_64-darwin
      /usr/local/opt/texinfo/bin)

typeset -U PATH
export PATH
autoload -Uz time-shell nuke convert-to-md \
         colorlist open-apps iterm-config
