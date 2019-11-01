typeset -aU path cdpath fpath manpath module_path modulepath
typeset -A ZPLGM

export LANG=en_US.UTF-8
export TERM=xterm-256color

export CACHE="$HOME/.cache"
export DOTFILES="$HOME/dotz"
export PYENV_ROOT="$HOME/.pyenv"
export EMACSD="$DOTFILES/editors/emacs.d"
export EDITOR='emacsclient -a "" -c'

export ZSH="$DOTFILES/shells/zsh"
export ZSH_CACHE="$CACHE/zsh"
export HISTFILE="$ZSH_CACHE/zsh_history"

ZPLGM[HOME_DIR]="$ZSH_CACHE/zplugin"
ZPLGM[BIN_DIR]="$ZPLGM[HOME_DIR]/bin"
ZPLGM[ZCOMPDUMP_PATH]="$ZSH_CACHE/zcompdump"

module_path+=($HOME/.cache/zsh/zplugin/bin/zmodules/Src)
modulepath=(/usr/local/Cellar/modules/4.3.0/modulefiles)

fpath+=($ZSH/functions)

path=($PYENV_ROOT/bin
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

autoload -Uz time-shell nuke convert-to-md colorlist
