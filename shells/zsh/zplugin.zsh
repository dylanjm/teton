#! zsh
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

zplugin ice wait'!' \
        atload'source ~/.p10k.zsh; _p9k_precmd' blockf lucid nocd
zplugin $load romkatv/powerlevel10k

zplugin ice wait'2a' lucid
zplugin snippet OMZ::lib/history.zsh

zplugin ice svn wait'2b' pick'init.zsh' lucid 
zplugin snippet PZT::modules/directory

zplugin ice wait'2c' lucid
zplugin snippet OMZ::lib/completion.zsh

zplugin ice wait'2d' lucid
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zplugin ice wait'2' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

# if test "$USER" = "mcdodj"; then
#     zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
#             atpull'%atclone' pick"direnv" src"zhook.zsh" wait'2' lucid blockf
#     zplugin $load direnv/direnv
# fi

zplugin ice wait'2e' atload:'_zsh_autosuggest_start' \
        wrap_track"_zsh_autosuggest_start" lucid
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice wait'2f' blockf lucid
zplugin $load zsh-users/zsh-completions

zplugin ice wait'!2g' atinit"ZPLGM[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" lucid
zplugin $load zdharma/fast-syntax-highlighting

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
