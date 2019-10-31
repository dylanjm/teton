. /usr/local/Cellar/modules/4.3.0/init/bash
. /usr/local/Cellar/modules/4.3.0/init/bash_completion

export MODULEPATH="$MODULEPATH:/opt/moose/Modules/3.2.10/modulefiles"

if [ -f /opt/moose/environments/moose_profile ]; then
    . /opt/moose/environments/moose_profile
    module load moose-dev-clang ccache icecream
fi

alias cubit='/Applications/Cubit.app/Contents/MacOS/cubitclx'
alias ls='ls -G'
alias em='emacs'
alias emc='emacsclient'
alias notifyDone='terminal-notifier -title "Terminal" -message "Finished"'
alias projects='cd ~/Documents/projects'
alias bis='cd ~/Documents/projects/bison'
alias bisd='cd ~/Documents/projects/bison_data'
alias dotbin='cd ~/dotz/bin/'
alias valid='cd ~/Documents/projects/bison/assessment/LWR/validation'
alias moose='cd ~/Documents/projects/moose'


bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'
