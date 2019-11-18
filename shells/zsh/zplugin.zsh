typeset -F4 SECONDS=0

###
### Check for zplugin install
###
if test ! -f $ZPLGM[HOME_DIR]/bin/zplugin.zsh; then
    command mkdir -p $ZPLGM[HOME_DIR]
    command git clone https://github.com/psprint/zplugin $ZPLGM[HOME_DIR]/bin
fi

source $ZPLGM[HOME_DIR]/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

### Zplugin is hard-coded to create this directory.
### See: https://github.com/zdharma/zplugin/issues/197
if test -e $CACHE/zplugin; then
   rm -rf $CACHE/zplugin
fi

### Check if zmodule is installed and built.
if [ "$(zmodload zdharma/zplugin 2>/dev/null; echo $?)" = "1" ]; then
    print "ZPlugin module not built! Building now..."
    zpl module build
fi

###
### Load Zsh Plugins
###
# Set this variable to light or load for easy debugging.
# Change to load=load for debugging.
load=light

zplugin ice wait'!' lucid nocd
zplugin load mengelbrecht/slimline

zplugin ice as"program" wait'!' make'!' atclone'./direnv hook zsh > zhook.zsh' \
        atpull'%atclone' pick"direnv" src"zhook.zsh" lucid blockf
zplugin $load direnv/direnv

zplugin ice wait'4' lucid
zplugin $load laggardkernel/zsh-thefuck

zplugin ice wait'4' lucid
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zplugin ice wait'4' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
        atpull'%atclone' pick"clrs.zsh" nocompile'!' \
        atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

zplugin ice wait'3' blockf atpull'zplugin creinstall -q' lucid
zplugin $load zsh-users/zsh-completions

zplugin ice wait'3a' atload:'_zsh_autosuggest_start' lucid
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice wait'3b' atload"zpcompinit; zpcdreplay; " lucid
zplugin $load zdharma/fast-syntax-highlighting

zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
