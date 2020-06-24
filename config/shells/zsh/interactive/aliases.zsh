#!/usr/bin/env zsh

###
### Global
###
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../../'
alias -g G='| grep'
alias -g P='| pbcopy'
alias -g WC='| wc -l'
alias -g TF='| tail -f'
alias -g DN='/dev/null'

###
### Emacs
###
alias zconf="${EDITOR} ${ZSH}"
alias econf="${EDITOR} ${DOTFILES}/config/emacs"
alias kemc="brew services restart emacs-head"
alias em="/usr/local/bin/emacsclient -t -c"
[[ "${INSIDE_EMACS}" == "vterm" ]] && alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

###
### Common Lisp
###
alias sbcl="rlwrap -p \"red\" -D 0 -H \"${XDG_DATA_HOME}/sbcl_history\" sbcl"

###
### Navigation
###
alias dtop="cd ${HOME}/Desktop"
alias docs="cd ${HOME}/Documents"
alias dl="cd ${HOME}/Downloads"

alias dot="cd ${DOTFILES}"
alias dotconfig="cd ${DOTFILES}/config"
alias dotshell="cd ${ZSH}"
alias dotem="cd ${DOTFILES}/config/emacs"
alias dotbin="cd ${DOTFILES}/local/bin"
alias dotboo="cd ${DOTFILES}/local/share/bootstrap"
alias cache="cd $XDG_CACHE_HOME"
alias org="cd ${HOME}/Documents/org-files"

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
alias cam='conda activate moose'
alias cdm='conda deactivate'

###
### Misc
###
alias refresh="source ${HOME}/.zshrc; echo 'Reloaded .zshrc.'"
alias reload="exec ${SHELL} -l -i"
alias bubu='brew update && brew upgrade && brew cleanup'

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
alias libmesh='cd ~/Documents/projects/libmesh'
alias papers='cd ~/Documents/projects/papers'
