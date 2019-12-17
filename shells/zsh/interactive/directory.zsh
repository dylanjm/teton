###
### Directory
###
setopt auto_cd              # auto changes to a directory without typing cd.
setopt auto_pushd           # push the old directory onto the stack on cd.
setopt pushd_ignore_dups    # do not store duplicates in the stack.
setopt pushd_silent         # do not print the directory stack after pushd or popd.
setopt pushd_tohome         # push to home directory when no argument is given.
setopt pushd_minus          # allows you to 'cd -n' to n previous directories
setopt cdable_vars          # change directory to a path stored in a variable.
setopt mult_ios             # write to multiple descriptors.
setopt extended_glob        # use extended globbing syntax.
setopt chase_links          # fully resolve file-path.
unsetopt clobber            # do not overwrite existing files with > and >>.
                            # use >! and >>! to bypass.

alias d='dirs -v'
for index ({1..9}) alias "$index"="cd +${index}"; unset index
