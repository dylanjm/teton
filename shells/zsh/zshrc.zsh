#!/usr/local/bin/zsh
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/p10k-instant-prompt-${(%):-%n}.zsh"
fi

typeset -ga mylogs
zflai-msg() { mylogs+=( "$1" ); }
zflai-assert() { mylogs+=( "$4"${${${1:#$2}:+FAIL}:-OK}": $3" ); }

source $ZSH/zplugin.zsh
eval "$(direnv hook zsh)"

zflai-msg "[zshrc] Finishing, loaded custom modules: ${(j:, :@)${(k)modules[@]}:#zsh/*}"

vterm_prompt_end() {
    printf "\e]51;A$(whoami)@$(hostname):$(pwd)\e\\";
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f $DOTFILES/shells/zsh/themes/p10k.zsh ]] || source $DOTFILES/shells/zsh/themes/p10k.zsh

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
