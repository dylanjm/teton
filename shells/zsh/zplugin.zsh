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

zplugin ice
zplugin $load romkatv/powerlevel10k

# zplugin ice pick'spacezsh.zsh' \
#   compile'{presets/^(*.zwc),lib/**/^(*.zwc),sections/^(*.zwc)}'
# zplugin $load laggardkernel/spacezsh-prompt

# zplugin ice lucid atinit'fpath+=($PWD/functions.zwc $PWD/functions $PWD/modules.zwc $PWD/modules)' \
#   atload'source $DOTFILES/shells/zsh/themes/apollo.zsh'
# zplugin light mjrafferty/apollo-zsh-theme

# zplugin ice wait'!' pick"/dev/null" multisrc"{async,pure}.zsh" atload'!prompt_pure_precmd' nocd
# zplugin $load sindresorhus/pure

zplugin ice wait'1' lucid
zplugin $load hlissner/zsh-autopair

zplugin ice wait'1' lucid
zplugin $load laggardkernel/zsh-thefuck

export LS_COLORS="$(vivid generate gruvbox-dark)"
export EXA_COLORS="tr=38;5;3:tw=38;5;1:tx=38;5;2:su=38;5;5:sf=38;5;5:xa=38;5;15:sn=38;5;6:sb=38;5;14:df=38;5;6:ds=38;5;14:uu=38;5;10:un=38;5;8:gu=38;5;11:gn=38;5;8:lc=38;5;202:lm=38;5;211:ga=38;5;112:gm=38;5;11:gd=38;5;9:gv=38;5;91:gt=38;5;202:xx=38;5;102:da=38;5;12:in=38;5;5:bl=38;5;6:hd=38;5;250:lp=38;5;7:cc=38;5;208:b0=37;41;1:"

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

zplugin ice wait'2b' atinit"ZPLGM[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting

zplugin ice wait'2c' atload"!_zsh_autosuggest_start" lucid
zplugin $load zsh-users/zsh-autosuggestions

ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_MANUAL_REBIND=true

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
