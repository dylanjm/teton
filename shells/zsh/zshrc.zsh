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

if [[ "$INSIDE_EMACS" == "vterm" ]]; then
  setopt PROMPT_SUBST
  [[ ! -f $DOTFILES/shells/zsh/themes/p10k_vterm.zsh ]] || source $DOTFILES/shells/zsh/themes/p10k_vterm.zsh
  PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
else
  [[ ! -f $DOTFILES/shells/zsh/themes/p10k.zsh ]] || source $DOTFILES/shells/zsh/themes/p10k.zsh
fi

eval "$(direnv hook $0)"
/usr/local/bin/gpgconf --launch gpg-agent


# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
