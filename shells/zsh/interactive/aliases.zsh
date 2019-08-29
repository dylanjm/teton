alias refresh='source $HOME/.zshrc; echo "Reloaded .zshrc."'

alias emc='emacsclient -n -c'

alias kemc="emacsclient -e '(kill-emacs)'; countdown"

alias emt="emacsclient -t"

alias lx='exa -lahH --color-scale --icons --sort=changed --time=changed --time-style long-iso -r'

alias brew="env PATH=${PATH//$HOME/.pyenv\/shims:/} brew"

alias desk="cd $HOME/Desktop"

alias docs="cd $HOME/Documents"

alias dot="cd $DOTFILES"

alias dotz="cd $ZSH"

alias dotem="cd $DOTFILES/editors/emacs.d"

alias bin='cd $DOTFILES/bin'

if test "$USER" = "djm"; then 
    alias openbugs="wine ~/.wine/drive_c/Program\ Files\ \(x86\)/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
    alias omscs="cd ~/Documents/College/OMSCS"
    alias bayes="cd ~/Documents/College/OMSCS/isye_6420"
    alias rl="cd ~/Documents/College/OMSCS/cs_7642"
    alias github="cd ~/Documents/Code/Github"
fi

if test "$USER" = "mcdodj"; then
    alias proj='cd ~/Documents/projects'
    alias bis='cd ~/Documents/projects/bison'
    alias bisd='cd ~/Documents/projects/bison_data'
    alias valid='cd ~/Documents/projects/bison/assessment/LWR/validation'
    alias moose='cd ~/Documents/projects/moose'
    alias papers='cd ~/Documents/projects/papers'
fi
