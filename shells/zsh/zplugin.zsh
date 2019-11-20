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
[[ -d $CACHE/zplugin ]] && {
   rm -rf $CACHE/zplugin
}

###
### Load Zsh Plugins
###
# Set this variable to light or load for easy debugging.
# Change to load=load for debugging.
load=light

zplugin ice wait lucid
zplugin $load mafredri/zsh-async

zplugin ice wait'!' lucid nocd atinit'source $ZSH/misc/slimline.zsh'
zplugin $load mengelbrecht/slimline

zplugin ice as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh"
zplugin $load direnv/direnv

zplugin ice wait'1' lucid
zplugin $load laggardkernel/zsh-thefuck

zplugin ice wait'1' lucid
zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

zplugin ice wait'1' lucid
zplugin snippet OMZ::plugins/colorize/colorize.plugin.zsh

zplugin ice wait'1' atclone"gdircolors -b LS_COLORS > clrs.zsh" \
    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”' lucid
zplugin $load trapd00r/LS_COLORS

zplugin ice wait'1' lucid
zplugin $load davidparsson/zsh-pyenv-lazy

zplugin ice wait'1' lucid
zplugin $load $ZSH/interactive

zplugin wait'3' lucid for \
        atload"!_zsh_autosuggest_start" zsh-users/zsh-autosuggestions \
        atpull"zplugin creinstall -q ." blockf zsh-users/zsh-completions \
        atinit"ZPLGM[COMPINIT_OPTS]=-C; _zpcompinit_fast; zpcdreplay" zdharma/fast-syntax-highlighting




zflai-msg "[zshrc] Zplugin block took ${(M)$(( SECONDS * 1000 ))#*.?} ms"
