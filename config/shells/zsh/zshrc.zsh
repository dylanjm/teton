#!/usr/bin/env zsh
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source "${ZSH}/zinit.zsh"

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

case "${INSIDE_EMACS:-}" in
  "")
    source "${ZSH}/themes/p10k.zsh"
    ;;
  "vterm")
    autoload -U add-zsh-hook
    add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }
    setopt prompt_subst
    source "${ZSH}/themes/p10k_vterm.zsh"
    PROMPT="$PROMPT'%{$(vterm_prompt_end)%}'"
    ;;
esac

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true

# Local Variables:
# mode: shell-script
# sh-basic-offset: 2
# End:
