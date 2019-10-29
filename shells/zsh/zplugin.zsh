#!/usr/local/bin/zsh
typeset -A ZPLGM
typeset -F4 SECONDS=0

ZPLGM[HOME_DIR]="$CACHE_HOME/zsh/zplugin"
ZPLGM[ZCOMPDUMP_PATH]=$CACHE_HOME/zsh/zcompdump

if [[ ! -f $ZPLGM[HOME_DIR]/bin/zplugin.zsh ]]; then
    git clone https://github.com/psprint/zplugin $ZPLG_HOME/bin
fi

source $ZPLGM[HOME_DIR]/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

module_path+=( "$HOME/.cache/zsh/zplugin/bin/zmodules/Src" )
zmodload zdharma/zplugin

load=light

zplugin ice wait'!' pick"async.zsh" src"pure.zsh" lucid nocd
zplugin $load sindresorhus/pure

zplugin ice svn wait'' pick'init.zsh' lucid
zplugin snippet PZT::modules/history

zplugin ice svn wait'' pick'init.zsh' lucid
zplugin snippet PZT::modules/directory

zplugin ice svn wait'' pick'init.zsh' lucid
zplugin snippet PZT::modules/completion

zplugin ice wait'2' lucid
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zplugin ice wait'2' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
        atpull'%atclone' pick"clrs.zsh" nocompile'!' \
        atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

zplugin ice wait'1' atload:'_zsh_autosuggest_start' lucid
zplugin $load zsh-users/zsh-autosuggestions
zplugin cdclear -q

zplugin ice wait'1b' blockf lucid
zplugin $load zsh-users/zsh-completions
zplugin cdclear -q

zplugin ice wait'1c' atinit"ZPLGM[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
