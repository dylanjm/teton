#!/usr/bin/env zsh

###
### Global
###
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../../'
alias -g G='| grep'
alias -g WC='| wc -l'
alias -g TF='| tail -f'
alias -g DN='/dev/null'

###
### Emacs
###
alias zconf="$EDITOR $ZSH &"
alias econf="$EDITOR $DOTFILES/editors/emacs.d &"
alias kemc="brew services restart emacs-head"
alias et="emacsclient -t"
[[ "$INSIDE_EMACS" = 'vterm' ]] && alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

###
### Common Lisp
###
alias sbcl="rlwrap -p \"red\" -D 0 -H \"$XDG_DATA_HOME/sbcl_history\" sbcl"

###
### Navigation
###
alias de="cd $HOME/Desktop"
alias dc="cd $HOME/Documents"
alias dl="cd $HOME/Downloads"

alias dot="cd $DOTFILES"
alias dotapp="cd $DOTFILES/apps"
alias dotshell="cd $ZSH"
alias dotem="cd $DOTFILES/editors/emacs.d"
alias dotbin='cd $DOTFILES/bin'

alias cache='cd $XDG_CACHE_HOME'
alias cachem='cd $XDG_CACHE_HOME/emacs'
alias cachez='cd $XDG_CACHE_HOME/zsh'
alias plugins='cd $XDG_CACHE_HOME/zsh/zplugin/plugins'

alias org='cd $HOME/Documents/org-files'

###
### Commands
###
alias ls='gls --color=tty --group-directories-first'
alias la='gls -la --color=tty --group-directories-first'
alias lx='exa -lah --icons --time=changed --time-style long-iso --group-directories-first'
alias lxg='exa -lah --icons --time=changed --time-style long-iso --git --sort=modified'
alias lxm='exa -lah --icons --time=changed --time-style long-iso --sort=modified'
alias bc='eva'
alias cat='bat'

###
### Misc
###
alias refresh='source $HOME/.zshrc; echo "Reloaded .zshrc."'
alias reload='exec $SHELL -l -i'
alias mypic='wget https://uninformedpriors.org/img/dylan.png'
alias bubu='brew update && brew upgrade && brew cleanup'
alias padd="rename -e 's/\d+/sprintf(\"%02d\",$&)/e' --"
alias rws="rename 's/ /_/g' --"
alias modname="rename 's/_-_lang_en_vs[0-9]+//g' --"
alias punc="rename -e 's/\&/and/g' -e 's/[^0-9A-Za-z_.-]//g' --"

###
### Ledger
###
alias aa='ledger bal Allocation --current --format "\
  %-17((depth_spacer)+(partial_account))\
  %10(percent(market(display_total), market(parent.total)))\
  %16(market(display_total))\n%/"'

###
### Git aliases
###
alias gss='git status -s'
alias gsa='git status'
alias gaa='git add --all'
alias gcm='git commit -m'
alias gp='git push'
alias gco='git checkout'
alias gcob='git checkout -b'

###
### Personal Aliases
###
alias omscs="cd ~/Documents/College/OMSCS"
alias bayes="cd ~/Documents/College/OMSCS/isye_6420"
alias rl="cd ~/Documents/College/OMSCS/cs_7642"
[[ "$USER" == "djm" ]] && alias kbai="cd ~/Documents/College/OMSCS/cs7637-kbai"
[[ "$USER" == "djm" ]] && alias code="cd ~/Documents/Code/Github"

###
### Work Aliases
###
[[ "$USER" == "mcdodj" ]] && alias code="cd ~/Documents/forked_github"
[[ "$USER" == "mcdodj" ]] && alias kbai="cd ~/Documents/forked_github/cs7637-kbai"
alias proj='cd ~/Documents/projects'
alias bis='cd ~/Documents/projects/bison'
alias bisd='cd ~/Documents/projects/bison_data'
alias valid='cd ~/Documents/projects/bison/assessment/LWR/validation'
alias moose='cd ~/Documents/projects/moose'
alias papers='cd ~/Documents/projects/papers'
