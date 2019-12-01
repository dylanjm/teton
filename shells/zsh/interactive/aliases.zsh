###
### Emacs
###
alias et='emacs -Q --load $HOME/.emacs.d/mini-init.el -nw'
# TODO

###
### Navigation
###
alias de="cd $HOME/Desktop"
alias dc="cd $HOME/Documents"
alias dl="cd $HOME/Downloads"

alias dot="cd $DOTFILES"
alias dotapp="cd $DOTFILES/apps"
alias dotm="cd $DOTFILES/misc"
alias dotshell="cd $ZSH"
alias dotem="cd $DOTFILES/editors/emacs.d"
alias dotbin='cd $DOTFILES/bin'

alias cache='cd $CACHE'
alias cachem='cd $CACHE/emacs'
alias cachez='cd $CACHE/zsh'
alias plugins='cd $CACHE/zsh/zplugin/plugins'

alias org='cd $HOME/org'

###
### Commands
###
alias ls='gls -la --color=tty --group-directories-first'
alias lx='exa -lah --icons --time=changed --time-style long-iso --group-directories-first'
alias glx='exa -lah --icons --time=changed --time-style long-iso --git --sort=modified'
alias lxm='exa -lah --icons --time=changed --time-style long-iso --sort=modified'

###
### Misc
###
alias refresh='source $HOME/.zshrc; echo "Reloaded .zshrc."'
alias mypic='wget https://uninformedpriors.org/img/dylan.png'
alias bubu='brew update && brew upgrade'
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
alias omscs="cd ~/Documents/College/OMSCS"
alias bayes="cd ~/Documents/College/OMSCS/isye_6420"
alias rl="cd ~/Documents/College/OMSCS/cs_7642"
alias code="cd ~/Documents/Code/Github"

###
### Work Aliases
###
alias forkg='cd ~/Documents/forked_github'
alias proj='cd ~/Documents/projects'
alias bis='cd ~/Documents/projects/bison'
alias bisd='cd ~/Documents/projects/bison_data'
alias valid='cd ~/Documents/projects/bison/assessment/LWR/validation'
alias moose='cd ~/Documents/projects/moose'
alias papers='cd ~/Documents/projects/papers'
