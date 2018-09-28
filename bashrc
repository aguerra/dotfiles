# bashrc

# Exports
export GOPATH="$HOME/go"
export PATH="~/bin:/usr/lib/ccache:$PATH:/sbin:/usr/sbin:/usr/local/go/bin:$GOPATH/bin"

# Misc
umask 022

VENV_WRAPPER=/usr/share/virtualenvwrapper/virtualenvwrapper.sh
[[ -f $VENV_WRAPPER ]] && source $VENV_WRAPPER

# Local hook
[[ -f ~/.bashrc.local ]] && source ~/.bashrc.local
