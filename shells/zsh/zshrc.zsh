#!/usr/bin/env zsh
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source $ZSH/zplugin.zsh

[[ ! -f $DOTFILES/shells/zsh/themes/p10k.zsh ]] || source $DOTFILES/shells/zsh/themes/p10k.zsh

#source "$XDG_DATA_HOME/iterm2_shell_integration.zsh"

eval "$(direnv hook $0)"
/usr/local/bin/gpgconf --launch gpg-agent
# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
