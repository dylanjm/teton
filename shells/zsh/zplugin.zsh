#! zsh
typeset -A ZPLGM
ZPLG_HOME=${XDG_CACHE_HOME:=~/.cache}/zsh/zplugin
ZPLGM[HOME_DIR]=$ZPLG_HOME
ZPLGM[ZCOMPDUMP_PATH]=$XDG_CACHE_HOME/zsh/zcompdump

if [[ ! -f $ZPLG_HOME/bin/zplugin.zsh ]]; then
	git clone https://github.com/psprint/zplugin $ZPLG_HOME/bin
	zcompile $ZPLG_HOME/bin/zplugin.zsh
fi
source $ZPLG_HOME/bin/zplugin.zsh
load=light

zplugin ice wait '0' silent
zplugin $load willghatch/zsh-saneopt

zplugin ice wait '0' silent
zplugin $load mafredri/zsh-async

zplugin ice wait '0' silent
zplugin $load rupa/z

zplugin snippet ~/.p10k
zplugin $load romkatv/powerlevel10k

zplugin ice wait:1 atload:_zsh_autosuggest_start silent
zplugin $load zsh-users/zsh-autosuggestions

zplugin ice silent blockf;
zplugin $load zsh-users/zsh-completions

zplugin ice wait atinit"zpcompinit; zpcdreplay" lucid
zplugin light zdharma/fast-syntax-highlighting
