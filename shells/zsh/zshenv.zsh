export CACHE_HOME="$HOME/.cache"
export DOTFILES="$HOME/dotz"
export ZSH="$DOTFILES/shells/zsh"

fpath+=($ZSH/functions)

export PYENV_ROOT="$HOME/.pyenv"
export EDITOR='emacsclient -c'
export ALTERNATE_EDITOR=""
export LANG=en_US.UTF-8

export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.pyenv/bin:$DOTFILES/bin"

if which pyenv > /dev/null; then eval "$(pyenv init - zsh --no-rehash)"; fi
if which pyenv > /dev/null; then eval "$(pyenv init - zsh --no-rehash)"; fi

export PATH_SAVE=$PATH

autoload time-shell


