#!/usr/bin/env zsh
unsetopt GLOBAL_RCS  # Don’t source /etc/zshrc or other global zsh startup files.
typeset -gaU fpath module_path path
declare -gA ZINIT
typeset -g ZPLG_MOD_DEBUG=1

###
### XDG-Configuration
###
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_BIN_HOME="${HOME}/.local/bin"
export CACHE="${XDG_CACHE_HOME}"

###
### Config
###
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export DOTFILES="${HOME}/teton"
export ORG_FILES="${HOME}/Documents/forked_github/org-files"
export EDITOR="/usr/local/bin/emacsclient -t -c -a '/usr/local/bin/emacs'"
export EMACS="/usr/local/bin/emacs"

###
### History
###
export HISTFILE="${XDG_DATA_HOME}/zsh_history"
export HIST_STAMPS="mm/dd/yyyy"
export HISTSIZE=500000
export SAVEHIST=500000

###
### Atom
###
export ATOM_HOME="${XDG_DATA_HOME}/atom"

###
### CCACHE
###
export CCACHE_DIR="${XDG_CACHE_HOME}/ccache"

###
### Conda
###
export CONDARC="${XDG_CONFIG_HOME}/condarc"

###
### Emacs-Anywhere
###
export EA_PATH="${XDG_DATA_HOME}/emacs_anywhere"

###
### FZF
###
export FZF_DEFAULT_COMMAND='fd -HI -L --exclude .git --color=always'
export FZF_DEFAULT_OPTS='
  --ansi
  --info inline
  --height 40%
  --reverse
  --border
  --multi
  --color fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
  --color info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54
'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview '(bat --theme ansi-dark --color always {} 2> /dev/null || exa --tree --color=always {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
export FZF_ALT_C_OPTS="--preview 'exa --tree --color=always {} | head -200'"

###
### Gnupg
###
export GPG_TTY=$(tty)
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"
export SSH_AUTH_SOCK=$(/usr/local/bin/gpgconf --list-dirs agent-ssh-socket)

###
### Goku
###
export GOKU_EDN_CONFIG_FILE="${XDG_CONFIG_HOME}/karabiner/karabiner.edn"

###
### Guile
###
export GUILE_AUTO_COMPILE=1
export GUILE_HISTORY="${XDG_DATA_HOME}/guile_history"

###
### Java
###
export JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk-14.0.1.jdk/Contents/Home"
export JUNIT_HOME="/Library/JUNIT"
export CLASSPATH="${CLASSPATH}:${JUNIT_HOME}/junit-4.10.jar:."

###
### Julia
###
export JULIA_NUM_THREADS=16
export JULIA_DEPOT_PATH="${XDG_DATA_HOME}/julia"

###
### Less
###
export LESS="-g -i -F -M -N -R -S -w -z-4"
export LESSOPEN='| /usr/bin/env lessfilter %s 2>&-'
export LESSHISTFILE="${XDG_DATA_HOME}/lesshst"
export LESS_TERMCAP_mb=$'\E[01;31m'    # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'    # begin bold
export LESS_TERMCAP_me=$'\E[0m'        # end mode
export LESS_TERMCAP_se=$'\E[0m'        # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'        # end underline
export LESS_TERMCAP_us=$'\E[01;32m'    # begin underline

###
### Python
###
export PYTHONPATH="$HOME/Documents/projects/moose/python:$PYTHONPATH"
export PYENV_ROOT="${XDG_DATA_HOME}/pyenv"
export IPYTHONDIR="${XDG_CONFIG_HOME}/ipython"
export PYLINTHOME="${XDG_CACHE_HOME}/pylint.d"
export PYTHONSTARTUP="${XDG_CONFIG_HOME}/pythonrc"
export MPLCONFIGDIR="${XDG_DATA_HOME}/matplotlib"
export MYPY_CACHE_DIR="${XDG_CACHE_HOME}/mypy_cache"
export PYGMENTIZE_STYLE='paraiso-dark'

###
### R
###
export R_USER="${XDG_CONFIG_HOME}/R"
export R_ENVIRON_USER="${XDG_CONFIG_HOME}/R/Renviron"
export R_PROFILE_USER="${XDG_CONFIG_HOME}/R/Rprofile"
export R_MAKEVARS_USER="${XDG_CONFIG_HOME}/R/Makevars"
export R_HISTFILE="${XDG_DATA_HOME}/Rhistory"
export R_LIBS_USER="${HOME}/Library/R/4.0/library"
export R_HISTSIZE=100000
export R_STARTUP_DEBUG=TRUE
export MKL_NUM_THREADS=16
export OMP_NUM_THREADS=16

###
### Rust/Cargo
###
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"

###
### Subversion
###
export SUBVERSION_HOME="${XDG_CONFIG_HOME}/subversion"

###
### Terminfo
###
export TERMINFO="${XDG_DATA_HOME}/terminfo"

###
### TreeSitter
###
export TREE_SITTER_DIR="${XDG_DATA_HOME}/tree-sitter"

###
### Weechat
###
export WEECHAT_HOME="${XDG_CONFIG_HOME}/weechat"

###
### ZSH
###
export ZSH="${DOTFILES}/config/shells/zsh"

###
### Zplugin
###
export ZINIT[HOME_DIR]="${XDG_DATA_HOME}/zinit"
export ZINIT[BIN_DIR]="$ZINIT[HOME_DIR]/bin"
export ZINIT[PLUGINS_DIR]="$ZINIT[HOME_DIR]/plugins"
export ZINIT[ZCOMPDUMP_PATH]="${XDG_CACHE_HOME}/zcompdump"
export ZINIT[COMPINIT_OPTS]="-C"
export ZPFX="$ZINIT[HOME_DIR]/polaris"

###
### Zoxide
###
export _ZO_DATA_DIR="${XDG_DATA_HOME}/zoxide"
export _ZO_ECHO=0
export _ZO_MAXAGE=100

###
### ZSH-Auto-Suggestions
###
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

###
### MOOSE
###
export MOOSE_DIR="$HOME/Documents/projects/moose"

###
### Paths
###
fpath+=(/usr/local/opt/zsh/share/zsh/functions
        /usr/local/share/zsh/site-functions
        $ZINIT[HOME_DIR]/completions
        $DOTFILES/local/share/zsh_completions
        $ZSH/functions)

module_path+=($ZINIT[BIN_DIR]/zmodules/Src)

path=($CARGO_HOME/bin               # Rust CLI Utils
      $PYENV_ROOT/bin               # Pyenv CLI Utils
      $PYENV_ROOT/shims             # Python Libraries
      $XDG_BIN_HOME                 # Personal CLI Tools
      /usr/local/bin                # Homebrew Installations
      /usr/bin
      /bin
      /usr/local/sbin
      /usr/sbin
      /sbin
      /usr/local/texlive/2020/bin/x86_64-darwin
      /usr/local/opt/texinfo/bin
      /opt/X11/bin)

export PATH
export FPATH

export MANPATH="/usr/local/share/man:/usr/share/man:${MANPATH}"
export INFOPATH="/usr/local/share/info:/usr/share/info:${INFOPATH}"
export HELPDIR="${MANPATH}"

###
### Personal Functions
###
autoload -Uz time-shell \
         zicompinit_fast \
         brew \
         nuke \
         conda \
         pyenv \
         knitit \
         vterm_printf \
         vterm_cmd \
         vterm_prompt_end \
         colorlist

# Local Variables:
# mode: shell-script
# sh-basic-offset: 2
# End:
