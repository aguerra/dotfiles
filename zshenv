# zshenv

# The global zshenv file is always read. When GLOBAL_RCS is unset here, global
# zprofile, zshrc, zlogin and zlogout files will not be sourced, making the
# shell more consistent across machines.
setopt NO_GLOBAL_RCS

# Basic exports
export EDITOR=emacsclient
export GOPATH=~/.go

# Keep only the first occurrence of each duplicated value
typeset -U path
path=(~/.local/bin /usr/local/go/bin /usr/games $path)

# Things that should be always available
. ~/.sdkman/bin/sdkman-init.sh
. /usr/share/virtualenvwrapper/virtualenvwrapper.sh

# Local hook
. ~/.zshenv.local
