# zshenv

# Don't read the global zprofile, zshrc, zlogin, and zlogout
setopt no_global_rcs

# Basic exports
export EDITOR=emacsclient
export GOPATH=~/.go

# Remove duplicated values. PATH is tied to path
typeset -U path=(~/.local/bin /usr/local/go/bin /usr/games $path)
typeset -U fpath=(~/.zsh.d $fpath)

# Local hook
. ~/config/zshenv
