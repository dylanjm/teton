#!/usr/local/bin/zsh
typeset -aU path cdpath fpath manpath module_path
typeset -A ZPLGM

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

export CACHE="$HOME/.cache"
export DOTFILES="$HOME/dotz"
export ZSH="$DOTFILES/shells/zsh"
export ZSH_CACHE="$CACHE/zsh"

export LANG=en_US.UTF-8
export EDITOR='emacsclient -a "" -c'
export HISTFILE="$ZSH_CACHE/zsh_history"
export LESS="-R -F -i -J -M -R -W -x4 -z-4"
export LESSOPEN='|lessfilter %s'
export LESSHISTFILE="$ZSH_CACHE/lesshst"

export _Z_DATA="$CACHE/zsh/z"
export GNUPGHOME="$HOME/.config/gnupg"
export CCACHE_DIR="$CACHE/ccache"

export PYENV_ROOT="$HOME/.config/pyenv"
export ZSH_PYENV_LAZY_VIRTUALENV=true

export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

export GPG_TTY=$(tty)

export ZPLGM[HOME_DIR]="$ZSH_CACHE/zplugin"
export ZPLGM[BIN_DIR]="$ZPLGM[HOME_DIR]/bin"
export ZPLGM[ZCOMPDUMP_PATH]="$ZSH_CACHE/zcompdump"

fpath+=($ZSH/functions)
module_path+=($HOME/.cache/zsh/zplugin/bin/zmodules/Src)

path=($PYENV_ROOT/bin
      $PYENV_ROOT/shims
      $ZPLGM[HOME_DIR]/polaris/bin
      $DOTFILES/bin
      /usr/local/opt/fzf/bin
      /usr/local/bin
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      /usr/local/texlive/2019/bin/x86_64-darwin
      /usr/local/opt/texinfo/bin)

typeset -U PATH FPATH MODULE_PATH
export PATH
export FPATH
export MODULE_PATH

autoload -Uz time-shell nuke convert-to-md \
         colorlist open-apps iterm-config _zpcompinit_fast \
         bundle-name
