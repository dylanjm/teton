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

#export PKG_CONFIG_PATH="/opt/X11/lib/pkgconfig"

export LANG=en_US.UTF-8
export EDITOR='emacsclient -a "" -c'
export HISTFILE="$ZSH_CACHE/zsh_history"

export LESS="-R -F -i -J -M -R -W -x4 -z-4"
export LESSOPEN='|lessfilter %s'
export LESSHISTFILE="$ZSH_CACHE/lesshst"
export LESS_TERMCAP_mb=$'\E[01;31m'             # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'             # begin bold
export LESS_TERMCAP_me=$'\E[0m'                 # end mode
export LESS_TERMCAP_se=$'\E[0m'                 # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'          # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'                 # end underline
export LESS_TERMCAP_us=$'\E[01;32m'             # begin underline

export _Z_DATA="$CACHE/zsh/z"
export GNUPGHOME="$HOME/.config/gnupg"
export CCACHE_DIR="$CACHE/ccache"

export PYENV_ROOT="$HOME/.config/pyenv"
export ZSH_PYENV_LAZY_VIRTUALENV=true
export PYLINTHOME="XDG_CACHE_HOME/pylint.d"

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git"'

export ENHANCD_DIR="$XDG_CACHE_HOME/zsh/enhancd"
export ENHANCD_FILTER=fzf:fzy:peco
export ENHANCD_COMPLETION_BEHAVIOR=history
export ENHANCD_DISABLE_HOME=1

#export JULIA_BINDIR="$HOME/.config/julia"

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
      /usr/local/opt/texinfo/bin
      /opt/X11/bin)

typeset -U PATH FPATH MODULE_PATH
export PATH
export FPATH
export MODULE_PATH

autoload -Uz time-shell nuke convert-to-md \
         colorlist open-apps iterm-config _zpcompinit_fast \
         bundle-name
