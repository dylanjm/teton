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
alias -g d='dirs -v'
for index ({1..9}) alias "$index"="cd -${index}"; unset index
alias -L run-help > /dev/null && unalias run-help
autoload -Uz  run-help    run-help-git  run-help-ip   run-help-openssl \
              run-help-p4 run-help-sudo run-help-svk  run-help-svn

###
### Emacs
###
alias tconf="${EDITOR} ${DOTFILES}"
alias zconf="${EDITOR} ${ZSH}"
alias econf="${EDITOR} ${DOTFILES}/config/emacs"
alias kemc="brew services restart emacs-head@30"
alias em="/usr/local/bin/emacsclient -t -c -a ''"

###
### Common Lisp
###
breakchars="(){}[],^%$#@\"\";:''|\\"
alias sbcl="rlwrap -pred -D 0 -H ${XDG_DATA_HOME}/sbcl_history -f ${XDG_DATA_HOME}/lisp_completions/sbcl_completions sbcl"

###
### Navigation
###
alias dtop="cd ${HOME}/Desktop"
alias docs="cd ${HOME}/Documents"
alias dl="cd ${HOME}/Downloads"

alias dot="cd ${DOTFILES}"
alias dotconf="cd ${DOTFILES}/config"
alias dotshell="cd ${ZSH}"
alias dotem="cd ${DOTFILES}/config/emacs"
alias dotbin="cd ${DOTFILES}/local/bin"
alias dotsha="cd ${DOTFILES}/local/share"
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
alias car='conda activate raven_libraries'
alias cdm='conda deactivate'
alias rg='rg --color=always --hidden --glob !.git --ignore-case --line-number --no-heading --sort=path'

###
### Misc
###
alias refresh="source ${HOME}/.zshrc; echo 'Reloaded .zshrc.'"
alias reload="exec ${SHELL} -l -i"
alias bubu='brew update && brew upgrade && brew upgrade --cask && brew cleanup'
alias nunu='zpl self-update; zpl update --all; knitit; kemc; bubu'
alias fcc='echo -ne "\e[5 q"' # Whenever I use reverse search it messes up my cursor

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
if [[ "${USER}" == "djm" ]]; then
  alias code="cd ~/Documents/Code/Github"
fi

###
### Work Aliases
###
if [[ "${USER}" == "mcdodj" ]]; then
  alias code="cd ~/Documents/forked_github"
  alias proj='cd ~/Documents/projects'
  alias ra='cd ~/Documents/projects/raven'
  alias he='cd ~/Documents/projects/HERON'
  alias fo='cd ~/Documents/projects/FORCE'
  alias teal='cd ~/Documents/projects/TEAL'
fi

# Local Variables:
# mode: shell-script
# sh-basic-offset: 2
# End:
