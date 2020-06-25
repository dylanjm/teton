#!/usr/bin/env zsh
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source "${ZSH}/zinit.zsh"

case "${INSIDE_EMACS:-}" in
  "")
    source "${ZSH}/themes/p10k.zsh"
    ;;
  "vterm")
    setopt PROMPT_SUBST
    source "${ZSH}/themes/p10k_vterm.zsh"
    PROMPT="$PROMPT'%{$(vterm_prompt_end)%}'"
    ;;
esac

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/usr/local/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
#         . "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh"
#     else
#         export PATH="/usr/local/Caskroom/miniconda/base/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
