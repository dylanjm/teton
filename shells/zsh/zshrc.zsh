ZDOTDIR="$HOME/dotz/shells/zsh"
source $ZDOTDIR/zplugin.zsh
for f in $ZDOTDIR/interactive/*(N); source $f

zpcompinit && zpcdreplay
([[ $ZPGLM[ZCOMPDUMP_PATH].zwc -nt $ZPGLM[ZCOMPDUMP_PATH] ]] || zcompile $ZPLGM[ZCOMPDUMP_PATH]) &!

[[ -o login ]] && for f in $ZDOTDIR/interactive+login/*(N); source $f

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
