###
### Emacs
###
alias me="$emacs -Q"
alias e="$EDITOR -n"
alias emc="$EDITOR -n -c "
alias ef="$EDITOR -c "
alias te="$EDITOR -a '' -nw "
alias kemc="$EDITOR -e '(let ((last-nonmenu-event nil) (kill-emacs-query-functions nil)) (save-buffers-kill-emacs t))'"

alias zconf="$EDITOR $ZSH"
alias econf="$EDITOR $EMACSD"


###
### Navigation
###
alias desk="cd $HOME/Desktop"
alias docs="cd $HOME/Documents"
alias dot="cd $DOTFILES"
alias dotapp="cd $DOTFILES/apps"
alias dotm="cd $DOTFILES/misc"
alias dotshell="cd $ZSH"
alias dotem="cd $DOTFILES/editors/emacs.d"
alias dotbin='cd $DOTFILES/bin'
alias plugins='cd $CACHE_HOME/zsh/zplugin/plugins'


###
### Commands
###
alias ls='gls --color=tty --group-directories-first'
alias lx='exa -lah --icons --time=changed --time-style long-iso --group-directories-first'
alias glx='exa -lah --icons --time=changed --time-style long-iso --git --sort=modified'
alias lxm='exa -lah --icons --time=changed --time-style long-iso --sort=modified'


###
### Misc
###
alias refresh='source $HOME/.zshrc; echo "Reloaded .zshrc."'
alias mypic='wget https://uninformedpriors.org/img/dylan.png'
#alias brew='env PATH=${PATH//$(pyenv root)\/shims:/} brew'


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
alias openbugs="wine ~/.wine/drive_c/Program\ Files\ \(x86\)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
alias omscs="cd ~/Documents/College/OMSCS"
alias bayes="cd ~/Documents/College/OMSCS/isye_6420"
alias rl="cd ~/Documents/College/OMSCS/cs_7642"
alias code="cd ~/Documents/Code/Github"


###
### Work Aliases
###
alias proj='cd ~/Documents/projects'
alias bis='cd ~/Documents/projects/bison'
alias bisd='cd ~/Documents/projects/bison_data'
alias valid='cd ~/Documents/projects/bison/assessment/LWR/validation'
alias moose='cd ~/Documents/projects/moose'
alias papers='cd ~/Documents/projects/papers'
