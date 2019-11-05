typeset -F4 SECONDS=0

###
### Check for zplugin install
###
if [[ ! -f $ZPLGM[HOME_DIR]/bin/zplugin.zsh ]]; then
    command mkdir -p $ZPLGM[HOME_DIR]
    command git clone https://github.com/psprint/zplugin $ZPLGM[HOME_DIR]/bin
fi

source $ZPLGM[HOME_DIR]/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

### Zplugin is hard-coded to create this directory.
### See: https://github.com/zdharma/zplugin/issues/197
if [[ -e $CACHE/zplugin ]]; then
   rm -rf $CACHE/zplugin;
fi

zmodload zdharma/zplugin &> /dev/null
if (( $? == 1 )); then zpl module build &; fi

# Set this variable to light or load for easy debugging.
load=light

zplugin ice wait'!' pick"async.zsh" src"pure.zsh" lucid nocd
zplugin $load sindresorhus/pure

zplugin ice wait'2' lucid
zplugin $load raxod502/wdx

zplugin ice wait'2' lucid
zplugin $load laggardkernel/zsh-thefuck

zplugin ice wait'1a' lucid
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zplugin ice wait'1b' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
        atpull'%atclone' pick"clrs.zsh" nocompile'!' \
        atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

if test "$USER" = "mcdodj"; then
    zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
            atpull'%atclone' pick"direnv" src"zhook.zsh" wait'2' lucid blockf
    zplugin $load direnv/direnv
fi

if [[ -e $ZPLGM[PLUGINS_DIR]/zsh-users---zshcompletions ]]; then
    zplugin ice wait'3' blockf atpull'zplugin creinstall -q' lucid
    zplugin $load zsh-users/zsh-completions
fi

zplugin ice wait'3a' atload:'_zsh_autosuggest_start' lucid
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice wait'3b' atinit"_zpcompinit_fast; zpcdreplay; " lucid
zplugin $load zdharma/fast-syntax-highlighting


zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
