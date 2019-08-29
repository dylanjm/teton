source $ZSH/zplugin.zsh
for f in $ZSH/interactive/*(N); source $f

#zpcompinit && zpcdreplay
([[ $ZPGLM[ZCOMPDUMP_PATH].zwc -nt $ZPGLM[ZCOMPDUMP_PATH] ]] || zcompile $ZPLGM[ZCOMPDUMP_PATH]) &!

# If zsh init ends with a failing command (like a conditional) the prompt will
# show the "error" colour on first launch. To avoid this, we simply end with a
# true command:
true

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
