# zshrc

# Enable persistent history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# If a command can't be executed and it is a directory name, cd to it.
setopt auto_cd

# Treat #, ~ and ^ as part of patterns for # filename generation, etc.
setopt extended_glob

# Adding a command that duplicates an older one removes the previous
# entry from the history.
setopt hist_ignore_all_dups

# Like APPEND_HISTORY except that new history lines are added
# incrementally as soon as they are entered.
setopt inc_append_history

# Disable all beeps
setopt no_beep

# Parameter expansion, command substitution and arithmetic expansion
# are performed in prompts.
setopt prompt_subst

# Autoload functions supplied with zsh without alias expansion
autoload -U add-zsh-hook compinit vcs_info

# Initialize new style completion
compinit

# Add vcs_info to precmd hook to get updated info in the prompt
add-zsh-hook precmd vcs_info

# Setup a minimalist prompt
prompt='%n@%m:%F{cyan}%~
%F{magenta}[%l]%F{yellow}${vcs_info_msg_0_} %(?.%F{green}✔.%F{red}✘) %F{white}> '

# Menu completion will be started unconditionally
zstyle ':completion:*' menu select

zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' rehash true

# vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git*' formats "(%s/%b/%c/%u/%m)"
zstyle ':vcs_info:git*+set-message:*' hooks git-aheadbehind


# User aliases
alias cp='cp -i'
alias debuild='debuild -us -uc'
alias disassemble='objdump -d'
alias g=git
alias grep='grep --color=auto'
alias http-mirror='wget -mkEp'
alias http-server='python3 -mhttp.server 8080'
alias la='ls -A'
alias ll='ls -l'
alias ls='ls --color=auto'
alias mirror='rsync -aci --delete'
alias mkvirtualenv3='mkvirtualenv -p /usr/bin/python3'
alias mv='mv -i'
alias openssl-cert-info='openssl x509 -text -noout -in'
alias ppager='ps axuf | pager'
alias reset-terminal='dconf reset -f /org/gnome/terminal/legacy/profiles:/'
alias safe-vim='vim -i NONE'
alias rm='rm -i'
alias tree='tree -C'
alias x=xdg-open

# Misc
umask 022

# Functions

# git: Show +N/-N when your local branch is ahead-of or behind remote HEAD
# Make sure you have added misc to your 'formats': %m
function +vi-git-aheadbehind()
{
    local ahead behind
    local -a gitstatus

    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null \
            | wc -l)
    (($ahead)) && gitstatus+=("%B%F{blue}+${ahead}%f%b")

    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null \
             | wc -l)
    (($behind)) && gitstatus+=("%B%F{red}-${behind}%f%b")

    hook_com[misc]+=${(j::)gitstatus}
}

function ssh-tunnel()
{
    local mode="$1"
    local port="$2"
    local host="$3"
    local dest="$4"
    shift 4

    print ssh "$mode" "$port:$dest" "$host" "$@"
    ssh "$mode" "$port:$dest" "$host" "$@"
}

function ssh-remote-tunnel()
{
    ssh-tunnel -R "$@"
}

function ssh-local-tunnel()
{
    ssh-tunnel -L "$@"
}

function tcp-file()
{
    local file="$1"
    local port="$2"

    socat -u FILE:"$file" TCP-L:"$port",reuseaddr,fork
}

# Setup sdkman, virtualenvwrapper and fzf
. ~/.sdkman/bin/sdkman-init.sh
. /usr/share/virtualenvwrapper/virtualenvwrapper.sh
. <(fzf --zsh)

# Local hook
. ~/.zshrc.local
