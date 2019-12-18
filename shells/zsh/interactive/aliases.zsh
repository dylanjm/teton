###
### ZSH
###
# unalias run-help
# autoload run-help

###
### Emacs
###
alias zconf="$EDITOR $ZSH &"
alias econf="$EDITOR $DOTFILES/editors/emacs.d &"
alias kemc="brew services restart emacs-head"
alias et="emacsclient -t"
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='printf "\e]51;Evterm-clear-scrollback\e\\";tput clear'
fi


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
alias ls='gls --color=tty --group-directories-first'
alias la='gls -la --color=tty --group-directories-first'
alias lx='exa -lah --icons --time=changed --time-style long-iso --group-directories-first'
alias lxg='exa -lah --icons --time=changed --time-style long-iso --git --sort=modified'
alias lxm='exa -lah --icons --time=changed --time-style long-iso --sort=modified'

###
### Misc
###
alias refresh='source $HOME/.zshrc; echo "Reloaded .zshrc."'
alias reload='exec $SHELL -l -i'
alias mypic='wget https://uninformedpriors.org/img/dylan.png'
alias bubu='brew update && brew upgrade'

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
[[ "$USER" == "djm" ]] && alias code="cd ~/Documents/Code/Github"

###
### Work Aliases
###
[[ "$USER" == "mcdodj" ]] && alias code="cd ~/Documents/forked_github"
alias proj='cd ~/Documents/projects'
alias bis='cd ~/Documents/projects/bison'
alias bisd='cd ~/Documents/projects/bison_data'
alias valid='cd ~/Documents/projects/bison/assessment/LWR/validation'
alias moose='cd ~/Documents/projects/moose'
alias papers='cd ~/Documents/projects/papers'
