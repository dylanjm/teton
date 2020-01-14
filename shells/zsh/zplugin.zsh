#!/usr/local/bin/zsh

###
### Check for zplugin install
###
[[ ! -f $ZPLGM[HOME_DIR]/bin/zplugin.zsh ]] && {
    command mkdir -p $ZPLGM[HOME_DIR]
    command git clone https://github.com/psprint/zplugin $ZPLGM[HOME_DIR]/bin
}

source $ZPLGM[HOME_DIR]/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

### Check if zmodule is installed and built.
zmodload zdharma/zplugin 2>/dev/null || zpl module build

### Zplugin is hard-coded to create this directory.
### See: https://github.com/zdharma/zplugin/issues/197
test -d $XDG_CACHE_HOME/zplugin && { rm -rf $XDG_CACHE_HOME/zplugin }

###
### Load Zsh Plugins
###
# Set this variable to light or load for easy debugging.
# Change to load=load for debugging.
load=light

zplugin $load romkatv/powerlevel10k

zplugin $load davidparsson/zsh-pyenv-lazy

zplugin ice wait'1' lucid
zplugin $load hlissner/zsh-autopair

zplugin ice wait'1' lucid
zplugin $load laggardkernel/zsh-thefuck

zplugin ice wait'1' lucid blockf
zplugin $load zsh-users/zsh-completions

zplugin ice wait'1' atinit'zstyle ":history-search-multi-word" page-size "15"' lucid
zplugin $load zdharma/history-search-multi-word

zplugin ice wait'1a' lucid blockf
zplugin $load rupa/z

zplugin ice wait'1b' lucid blockf
zplugin $load changyuheng/fz

zplugin ice wait'1c' multisrc"*.zsh" lucid
zplugin $load $ZSH/interactive

zplugin ice wait'2b' atinit"ZPLGM[COMPINIT_OPTS]=-C; _zpcompinit_fast; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting

zplugin ice wait'2c' atload"!_zsh_autosuggest_start" lucid
zplugin $load zsh-users/zsh-autosuggestions

ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_MANUAL_REBIND=true
