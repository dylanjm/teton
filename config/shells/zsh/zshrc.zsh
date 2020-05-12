#!/usr/bin/env zsh
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ $TERM == "dumb" ]]; then
  unsetopt zle
  PS1='$ '
  return
else
  if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/p10k-instant-prompt-${(%):-%n}.zsh"
  fi
fi

source $ZSH/zplugin.zsh

[[ "$INSIDE_EMACS" == "vterm" ]] && {
  setopt PROMPT_SUBST
  [[ ! -f $ZSH/themes/p10k_vterm.zsh ]] || source $ZSH/themes/p10k_vterm.zsh
  PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
}

[[ ! -f $ZSH/themes/p10k.zsh ]] || source $ZSH/themes/p10k.zsh

/usr/local/bin/gpgconf --launch gpg-agent

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/usr/local/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/usr/local/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

