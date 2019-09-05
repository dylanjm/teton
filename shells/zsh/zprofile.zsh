# This prevents /etc/zprofile from overwriting path on macOS
# Answer found @ https://github.com/sorin-ionescu/prezto/issues/381#issuecomment-294549773
# Order files are sourced on new terminal:
# /etc/zshenv + .zshenv + .zprofile + .zshrc

if [ -n "$PATH_SAVE" ]; then
    PATH_SAVE=:$PATH_SAVE:;
    PATH=$PATH:
    while [ -n "$PATH" ]; do
        x=${PATH%%:*}  # the first remaining entry
        case $PATH_SAVE in
            *:$x:*) ;;  # already there
            *) PATH_SAVE=$PATH_SAVE$x: ;;  # not there yet
        esac
        PATH=${PATH#*:}
    done
    PATH=${PATH_SAVE:1:-1}
    unset PATH_SAVE x
    export PATH
fi
