#!/usr/local/bin/zsh
typeset -aU path cdpath fpath manpath module_path
typeset -A ZPLGM

###
### XDG-Configuration
###
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

###
### Config
###
export LANG="en_US.UTF-8"
export EDITOR='emacsclient -a "" -c'
export HISTFILE="$XDG_CACHE_HOME/zsh/zsh_history"
export HISTSIZE=120000
export SAVEHIST=100000
export CACHE="$XDG_CACHE_HOME"
export DOTFILES="$HOME/dotz"

###
### ZSH
###
export ZSH="$DOTFILES/shells/zsh"

###
### Zplugin
###
export ZPLGM[HOME_DIR]="$XDG_CACHE_HOME/zsh/zplugin"
export ZPLGM[BIN_DIR]="$ZPLGM[HOME_DIR]/bin"
export ZPLGM[ZCOMPDUMP_PATH]="$XDG_CACHE_HOME/zsh/zcompdump"

###
### Less
###
export LESS="-R -F -i -J -M -R -W -x4 -z-4"
export LESSOPEN='|lessfilter %s'
export LESSHISTFILE="$XDG_CACHE_HOME/zsh/lesshst"
export LESS_TERMCAP_mb=$'\E[01;31m'             # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'             # begin bold
export LESS_TERMCAP_me=$'\E[0m'                 # end mode
export LESS_TERMCAP_se=$'\E[0m'                 # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'          # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'                 # end underline
export LESS_TERMCAP_us=$'\E[01;32m'             # begin underline

###
### Z
###
export _Z_DATA="$XDG_CACHE_HOME/zsh/z"

###
### CCACHE
###
export CCACHE_DIR="$XDG_CACHE_HOME/ccache"

###
### FZF
###
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git"'

###
### Enhancd
###
export ENHANCD_DIR="$XDG_CACHE_HOME/zsh/enhancd"
export ENHANCD_FILTER=fzf:fzy:peco
export ENHANCD_COMPLETION_BEHAVIOR=history
export ENHANCD_DISABLE_HOME=1

###
### Julia
###
export JULIA_NUM_THREADS=12
export JULIA_DEPOT_PATH="$XDG_CONFIG_HOME/julia"

###
### Rust/Cargo
###
export CARGO_HOME="$XDG_CONFIG_HOME/cargo"

###
### Python
###
export PYENV_ROOT="$XDG_CONFIG_HOME/pyenv"
export IPYTHONDIR="$XDG_CONFIG_HOME/ipython"
export ZSH_PYENV_LAZY_VIRTUALENV=true
export PYLINTHOME="XDG_CACHE_HOME/pylint.d"

###
### Gnupg
###
export GPG_TTY=$(tty)
export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"

###
### Paths
###
fpath=($XDG_CACHE_HOME/zsh/zplugin/completions
       /usr/local/share/zsh/site-functions
       /usr/local/Cellar/zsh/5.7.1/share/zsh/functions
       $ZSH/functions)

module_path+=($HOME/.cache/zsh/zplugin/bin/zmodules/Src)

path=($XDG_CONFIG_HOME/cargo/bin    # Rust CLI Utils
      $PYENV_ROOT/bin               # Pyenv CLI Utils
      $PYENV_ROOT/shims             # Python Libraries
      $ZPLGM[HOME_DIR]/polaris/bin  # Zplugin Installed Programs
      $DOTFILES/bin                 # Personal CLT Tools
      /usr/local/opt/fzf/bin        # FZF Completions
      /usr/local/bin                # Homebrew Installations
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      /usr/local/texlive/2019/bin/x86_64-darwin
      /usr/local/opt/texinfo/bin
      /opt/X11/bin)

export PATH
export FPATH
export MODULE_PATH

###
### Personal Functions
###
autoload -Uz time-shell nuke convert-to-md \
         colorlist open-apps iterm-config _zpcompinit_fast \
         bundle-name
