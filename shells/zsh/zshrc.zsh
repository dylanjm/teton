typeset -ga mylogs
zflai-msg() { mylogs+=( "$1" ); }
zflai-assert() { mylogs+=( "$4"${${${1:#$2}:+FAIL}:-OK}": $3" ); }

source $ZSH/zplugin.zsh
for f in $ZSH/interactive/*(N); do
    source $f
done

([[ $ZPGLM[ZCOMPDUMP_PATH].zwc -nt $ZPGLM[ZCOMPDUMP_PATH] ]] || zcompile $ZPLGM[ZCOMPDUMP_PATH]) &!

zflai-msg "[zshrc] Finishing, loaded custom modules: ${(j:, :@)${(k)modules[@]}:#zsh/*}"

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true
