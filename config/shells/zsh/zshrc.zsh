#!/usr/bin/env zsh
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source "${ZSH}/zinit.zsh"

[[ ! -f "${ZSH}/themes/p10k.zsh" ]] || source "${ZSH}/themes/p10k.zsh"

/usr/local/bin/gpgconf --launch gpg-agent

# [[ "$INSIDE_EMACS" == "vterm" ]] && {
#   setopt PROMPT_SUBST
#   [[ ! -f "${ZSH}/themes/p10k_vterm.zsh" ]] || source "${ZSH}/themes/p10k_vterm.zsh"
#   PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
# }

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
