# zshenv

# The global zshenv file is always read. When GLOBAL_RCS is unset here, global
# zprofile, zshrc, zlogin and zlogout files will not be sourced, making the
# shell more consistent across machines.
setopt NO_GLOBAL_RCS

export EDITOR=emacsclient
export GOPATH=~/.go
export PATH="~/.local/bin:/usr/local/go/bin:$PATH"

. ~/.zshenv.local 2>/dev/null || :
