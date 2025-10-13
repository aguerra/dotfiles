# zshenv

# The global zshenv file is always read. When GLOBAL_RCS is unset here, global
# zprofile, zshrc, zlogin and zlogout files will not be sourced, making the
# shell more consistent across machines.
setopt NO_GLOBAL_RCS

# Basic exports
export EDITOR=emacsclient
export GOPATH=~/.go
export PATH=~/.local/bin:/usr/local/go/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

# Things that should be always available
. ~/.sdkman/bin/sdkman-init.sh 2>/dev/null || :
. /usr/share/virtualenvwrapper/virtualenvwrapper.sh 2>/dev/null || :

# Local hook
. ~/.zshenv.local 2>/dev/null || :
