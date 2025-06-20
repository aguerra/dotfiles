# zshenv

# The global zshenv file is always read. When GLOBAL_RCS is unset here, global
# zprofile, zshrc, zlogin and zlogout files will not be sourced, making the
# shell more consistent across machines.
setopt NO_GLOBAL_RCS

export ALTERNATE_EDITOR=vim
export EDITOR=emacsclient
export PATH="/usr/lib/ccache:$HOME/.local/bin:$PATH"

[[ -f ~/.zshenv.local ]] && . ~/.zshenv.local || :
