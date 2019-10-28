#!/usr/local/bin/zsh
typeset -A ZPLGM
typeset -F4 SECONDS=0
ZPLG_HOME=${CACHE_HOME:=~/.cache}/zsh/zplugin
ZPLGM[HOME_DIR]=$ZPLG_HOME
ZPLGM[ZCOMPDUMP_PATH]=$CACHE_HOME/zsh/zcompdump

if [[ ! -f $ZPLG_HOME/bin/zplugin.zsh ]]; then
    git clone https://github.com/psprint/zplugin $ZPLG_HOME/bin
fi

source $ZPLG_HOME/bin/zplugin.zsh
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
zplugin cdclear -q

zplugin ice wait'2' lucid
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zplugin ice wait'2' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
        atpull'%atclone' pick"clrs.zsh" nocompile'!' \
        atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

if test "$USER" = "mcdodj"; then
    # zplugin ice wait'2' id-as'moose' pick'moose_profile.zsh' lucid
    # zplugin $load /opt/moose/environments
fi

zplugin ice wait'1' atload:'_zsh_autosuggest_start' lucid
zplugin $load zsh-users/zsh-autosuggestions
zplugin cdclear -q

zplugin ice wait'1b' blockf lucid
zplugin $load zsh-users/zsh-completions
zplugin cdclear -q

zplugin ice wait'1c' atinit"ZPLGM[COMPINIT_OPTS]=-C; _zcomp; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting

# Compile to decrease startup speed (only if $1 is older than a day)
_zcompare() {
    _comp_files=("${1}"(Nm-20))
    if (( $#_comp_files )); then
        zpcompinit
        zcompile ${1}
    fi
}

_zcomp() {
    zshrc="$ZSH/zshrc.zsh"
    zcompdump="$ZPLGM[ZCOMPDUMP_PATH]"
    _zcompare "$zshrc"
    _zcompare "$zcompdump"
}

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
