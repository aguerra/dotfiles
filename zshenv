# zshenv

# The global zshenv is always read; however, if GLOBAL_RCS is unset,
# global zprofile, zshrc, zlogin, and zlogout files are not.
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
