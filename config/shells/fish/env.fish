set -gx PATH /opt/X11/bin
append-to-path /usr/local/opt/texinfo/bin
append-to-path /usr/local/texlive/2019/bin/x86_64-darwin
append-to-path /sbin
append-to-path /usr/sbin
append-to-path /usr/local/sbin
prepend-to-path /bin
prepend-to-path /usr/bin
prepend-to-path /usr/local/bin
prepend-to-path $XDG_BIN_HOME
prepend-to-path $PYENV_ROOT/shims
prepend-to-path $PYENV_ROOT/bin
prepend-to-path $XDG_CONFIG_HOME/cargo/bin

# Set locale
set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8

# Configure fzf to use fd by default (fd respects .gitignore defaults)
set -gx FZF_DEFAULT_COMMAND 'fd --type f'