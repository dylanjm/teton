ssh() {
if [[ $1 == "falcon1" || $1 == "falcon2" || $1 == "flogin1" || $1 == "flogin2" || $1 == "bechler" || $1 == "hpclogin" || $1 == "hpclogin.inl.gov" || $1 == "lemhi1" || $1 == "lemhi2" ]]; then
    command ssh mcdodyla@$1
elif [ $1 == "icecream" ]; then
    command ssh -Y mcdodj@134.20.196.52
else
    command ssh "$@"
fi; }

#if [ -f /opt/moose/environments/moose_profile ]; then
#    . /opt/moose/environments/moose_profile
#fi

#source ~/git_completion.sh
#source ~/git_prompt.sh
#export PS1='$(__git_ps1 "(%s)")$ '

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

alias cubit='/Applications/Cubit.app/Contents/MacOS/cubitclx'
alias ls='ls -G'
alias em='emacs'
alias emc='emacsclient'
alias notifyDone='terminal-notifier -title "Terminal" -message "Finished"'
alias projects='cd ~/Documents/projects'
alias bison='cd ~/Documents/projects/bison'
alias bisond='cd ~/Documents/projects/bison_data'
alias scripts='cd ~/Documents/scripts'
alias validation='cd ~/Documents/projects/bison/assessment/LWR/validation'
alias moose='cd ~/Documents/projects/moose'

#module load ccache icecream

convert_to_md() { pandoc --ascii -f latex -t markdown-multiline_tables-simple_tables --atx-headers -o "$1" "$2";}

nuke() {
    read -r -p "Are you sure? [y/N] " response
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]
    then
	git clean -dfx && git submodule foreach --recursive git clean -dfx
    else
	return 0
    fi
}

bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'
