###
### Directory
###
setopt autocd              # auto changes to a directory without typing cd.
setopt autopushd           # push the old directory onto the stack on cd.
setopt pushdignoredups     # do not store duplicates in the stack.
setopt pushdsilent         # do not print the directory stack after pushd or popd.
setopt pushdtohome         # push to home directory when no argument is given.
setopt pushdminus          # allows you to 'cd -n' to n previous directories
setopt cdablevars          # change directory to a path stored in a variable.
setopt multios             # write to multiple descriptors.
setopt extendedglob        # use extended globbing syntax.
setopt chaselinks          # fully resolve file-path.
unsetopt clobber           # do not overwrite existing files with > and >>.
                           # use >! and >>! to bypass.
