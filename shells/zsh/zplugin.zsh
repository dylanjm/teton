#!/usr/local/bin/zsh
typeset -F4 SECONDS=0

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
#zmodload -i zsh/complist

### Zplugin is hard-coded to create this directory.
### See: https://github.com/zdharma/zplugin/issues/197
test -d $XDG_CACHE_HOME/zplugin && { rm -rf $XDG_CACHE_HOME/zplugin }

###
### Load Zsh Plugins
###
# Set this variable to light or load for easy debugging.
# Change to load=load for debugging.
load=light

zplugin light-mode for \
    zplugin/z-a-test \
    zplugin/z-a-patch-dl \
    zplugin/z-a-submods \
    zplugin/z-a-bin-gem-node \
    zplugin/z-a-rust \
    zplugin/z-a-man

zplugin ice wait'!' pick"/dev/null" multisrc"{async,pure}.zsh" atload'!prompt_pure_precmd' nocd
zplugin $load sindresorhus/pure

zplugin ice wait'1' lucid
zplugin $load hlissner/zsh-autopair

zplugin ice wait'1' lucid
zplugin $load laggardkernel/zsh-thefuck

export LS_COLORS="$(vivid generate snazzy)"
export EXA_COLORS="da=38;5;4:uu=38;5;2:sn=38;5;124;lp=38;5;5;b0=37;41;1"

zplugin ice wait'1' lucid blockf
zplugin $load zsh-users/zsh-completions

zplugin ice wait'1' multisrc"*.zsh" lucid
zplugin $load $ZSH/interactive

zplugin ice wait'1' lucid
zplugin $load unixorn/git-extra-commands

zplugin ice wait'1' lucid
zplugin $load zdharma/zsh-lint

zplugin ice wait'1' atinit'zstyle ":history-search-multi-word" page-size "7"' lucid
zplugin $load zdharma/history-search-multi-word

zplugin ice wait'1a' lucid blockf
zplugin $load rupa/z

zplugin ice wait'1b' lucid blockf
zplugin $load changyuheng/fz

zplugin ice wait'2c' atinit"ZPLGM[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay"
zplugin $load zdharma/fast-syntax-highlighting

zplugin ice wait'2b' atload"!_zsh_autosuggest_start"
zplugin $load zsh-users/zsh-autosuggestions

ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_MANUAL_REBIND=true

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
