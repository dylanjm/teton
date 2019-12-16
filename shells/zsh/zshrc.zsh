#!/usr/local/bin/zsh
typeset -ga mylogs
zflai-msg() { mylogs+=( "$1" ); }
zflai-assert() { mylogs+=( "$4"${${${1:#$2}:+FAIL}:-OK}": $3" ); }

source $ZSH/zplugin.zsh

zflai-msg "[zshrc] Finishing, loaded custom modules: ${(j:, :@)${(k)modules[@]}:#zsh/*}"

# _fzf_compgen_path() {
#   fd --hidden --follow --exclude ".git" . "$1"
# }

# # Use fd to generate the list for directory completion
# _fzf_compgen_dir() {
#   fd --type d --hidden --follow --exclude ".git" . "$1"
# }

eval "$(direnv hook zsh)"

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
