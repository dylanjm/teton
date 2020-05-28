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

zinit ice wait multisrc"*.zsh" lucid
zinit $load $ZSH/interactive

zinit ice wait lucid
zinit $load hlissner/zsh-autopair

zinit ice wait blockf lucid
zinit $load zsh-users/zsh-completions

zinit ice lucid
zinit $load zdharma/history-search-multi-word

zinit ice wait lucid
zinit $load zsh-users/zsh-history-substring-search
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

zinit ice wait'1a' atinit'_zpcompinit_fast; zpcdreplay' lucid
zinit $load zdharma/fast-syntax-highlighting

zinit ice wait'1b' atload"!_zsh_autosuggest_start" lucid
zinit $load zsh-users/zsh-autosuggestions
