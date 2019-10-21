#!/usr/bin/env zsh
typeset -A ZPLGM
typeset -F4 SECONDS=0
ZPLG_HOME=${CACHE_HOME:=~/.cache}/zsh/zplugin
ZPLGM[HOME_DIR]=$ZPLG_HOME
ZPLGM[ZCOMPDUMP_PATH]=$CACHE_HOME/zsh/zcompdump

if [[ ! -f $ZPLG_HOME/bin/zplugin.zsh ]]; then
    git clone https://github.com/psprint/zplugin $ZPLG_HOME/bin
    zcompile $ZPLG_HOME/bin/*.zsh
fi

module_path+=( "$HOME/.cache/zsh/zplugin/bin/zmodules/Src" )
zmodload zdharma/zplugin

source $ZPLG_HOME/bin/zplugin.zsh
load=light

# zplugin ice atload'source ~/.p10k.zsh;' lucid nocd
# zplugin $load romkatv/powerlevel10k
zplugin ice wait'!' pick"async.zsh" src"pure.zsh" lucid nocd
zplugin $load sindresorhus/pure

zplugin ice wait'3' lucid
zplugin snippet OMZ::lib/history.zsh

zplugin ice svn wait'3' pick'init.zsh' lucid
zplugin snippet PZT::modules/directory

zplugin ice wait'3' lucid
zplugin snippet OMZ::lib/completion.zsh

zplugin ice wait'3' lucid
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zplugin ice wait'3' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

if test "$USER" = "mcdodj"; then
    # zplugin ice wait'2' id-as'moose' pick'moose_profile.zsh' lucid
    # zplugin $load /opt/moose/environments
fi

zplugin ice wait'3' atload:'_zsh_autosuggest_start' \
        wrap_track"_zsh_autosuggest_start" lucid
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice wait'3' blockf lucid
zplugin $load zsh-users/zsh-completions

zplugin ice wait'!3' atinit"ZPLGM[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
