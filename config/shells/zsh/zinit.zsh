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

zinit ice lucid depth=1
zinit $load romkatv/powerlevel10k

zinit ice wait lucid ver"dev"
zinit $load marlonrichert/zsh-autocomplete

zinit ice wait lucid atpull'zinit creinstall -q .' blockf
zinit $load zsh-users/zsh-completions

zinit ice wait lucid id-as'fzf/completion'
zinit snippet https://github.com/junegunn/fzf/blob/master/shell/completion.zsh

zinit ice wait lucid id-as'fzf/key-bindings'
zinit snippet https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh

zinit ice wait lucid src"general.zsh" id-as'personal/config'
zinit $load $ZSH/interactive

zinit ice wait lucid id-as'zoxide/init' \
      atclone'zoxide init zsh > zoxide-init.zsh' \
      atpull'!%atclone' pick'zoxide-init.zsh'
zinit $load zdharma/null

zinit ice wait lucid \
      atclone'gdircolors -b LS_COLORS > clrs.zsh' \
      atpull'%atclone' pick'clrs.zsh' nocompile'!' \
      atload'zstyle ":completion:*" list-colors "${(s.:.)LS_COLORS}"'
zinit $load trapd00r/LS_COLORS

zinit ice wait'1a' lucid atinit'zicompinit_fast; zicdreplay' blockf
zinit $load zdharma/fast-syntax-highlighting

zinit ice wait'1b' atload"!_zsh_autosuggest_start" lucid blockf
zinit $load zsh-users/zsh-autosuggestions

zinit ice wait'1c' lucid
zinit $load hlissner/zsh-autopair

# Local Variables:
# mode: shell-script
# sh-basic-offset: 2
# End:
