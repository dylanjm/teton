#!/usr/bin/env zsh
## Check for zinit install
[[ ! -f "$ZINIT[BIN_DIR]/zinit.zsh" ]] && {
  command mkdir -p "$ZINIT[HOME_DIR]"
  git clone https://github.com/zdharma-continuum/zinit "$ZINIT[BIN_DIR]"
}

source "$ZINIT[BIN_DIR]/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

###
### Load Zsh Plugins
###

zinit ice lucid depth=1
zinit light romkatv/powerlevel10k

zinit ice wait lucid src"general.zsh" id-as'personal/config'
zinit light $ZSH/interactive

zinit ice wait lucid \
      atclone'gdircolors -b LS_COLORS > clrs.zsh' \
      atpull'%atclone' pick'clrs.zsh' nocompile'!' \
      atload'zstyle ":completion:*" list-colors "${(s.:.)LS_COLORS}"'
zinit light trapd00r/LS_COLORS

zinit ice wait lucid
zinit light hlissner/zsh-autopair

zinit ice wait src'histdb-interactive.zsh' lucid
zinit light larkery/zsh-histdb
bindkey '^r' _histdb-isearch

zinit wait lucid for \
 atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions \
 atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions

# Local Variables:
# mode: shell-script
# sh-basic-offset: 2
# End:
