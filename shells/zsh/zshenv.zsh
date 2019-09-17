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

export LESS='-F -i -J -M -R -W -x4 -X -z-4'
# Set colors for less. Borrowed from https://wiki.archlinux.org/index.php/Color_output_in_console#less .
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

path=($HOME/.pyenv/bin
      $HOME/.pyenv/shims
      /usr/local/bin
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      $DOTFILES/bin
      /opt/X11/bin)

export PATH

if type lesspipe.sh >/dev/null 2>&1; then
  export LESSOPEN='|lesspipe.sh %s'
fi

if type pygmentize >/dev/null 2>&1; then
  export LESSCOLORIZER='pygmentize -) '
fi

autoload -Uz time-shell countdown nuke convert-to-md config-powerline colorize colorlist prettycsv em ptyless


