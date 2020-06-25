#!/usr/bin/env zsh
###
### Check for zplugin install
###
[[ ! -f "$ZINIT[BIN_DIR]/zinit.zsh" ]] && {
  command mkdir -p "$ZINIT[HOME_DIR]"
  git clone https://github.com/zdharma/zinit.git "$ZINIT[BIN_DIR]"
}

source "$ZINIT[BIN_DIR]/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

### Check if zmodule is installed and built.
zmodload zdharma/zplugin 2>/dev/null || zinit module build

###
### Load Zsh Plugins
###
# Set this variable to light or load for easy debugging.
# Change to load=load for debugging.
load=light

zinit ice depth=1
zinit $load romkatv/powerlevel10k

zinit ice wait lucid blockf
zinit $load marlonrichert/zsh-autocomplete

zinit wait lucid light-mode for \
  id-as'fzf/completion' https://github.com/junegunn/fzf/blob/master/shell/completion.zsh \
  id-as'fzf/key-bindings' https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh

zinit ice wait ver"dev" lucid
zinit $load hlissner/zsh-autopair

zinit ice wait lucid blockf atpull'zinit creinstall -q .'
zinit $load zsh-users/zsh-completions

zinit ice wait multisrc"{aliases,general}.zsh" lucid blockf
zinit $load $ZSH/interactive

zinit ice wait'1a' atinit'zicompinit_fast; zicdreplay' lucid
zinit $load zdharma/fast-syntax-highlighting

zinit ice wait'1b' atload"!_zsh_autosuggest_start" lucid
zinit $load zsh-users/zsh-autosuggestions

zinit light-mode for atclone'gdircolors -b LS_COLORS > clrs.zsh' \
      atpull'%atclone' pick'clrs.zsh' \
      nocompile'!' atload'zstyle ":completion:*" list-colors "${(s.:.)LS_COLORS}"' \
      trapd00r/LS_COLORS
