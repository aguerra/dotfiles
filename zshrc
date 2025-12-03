# zshrc

# Enable persistent history
HISTFILE=~/config/zsh_history
HISTSIZE=100000
SAVEHIST=100000

# If a command can't be executed and it is a directory name, cd to it
setopt auto_cd

# Try to correct the spelling of all arguments in a line
setopt correct_all

# Treat #, ~ and ^ as part of patterns for filename generation, etc
setopt extended_glob

# Adding duplicates removes the previous entry from the history
setopt hist_ignore_all_dups

# New lines are added incrementally as soon as they are entered
setopt inc_append_history

# Disable all beeps
setopt no_beep

# Expansions and substitutions are performed in prompts
setopt prompt_subst

# Autoload functions supplied with zsh without alias expansion
autoload -U add-zsh-hook compinit vcs_info

# Initialize new style completion
compinit

# Add vcs_info to precmd hook to get updated info in prompt
add-zsh-hook precmd vcs_info

# Setup a not so minimalist prompt
prompt='%n@%m:%F{cyan}%~
%F{magenta}[%l]%F{yellow}${vcs_info_msg_0_} %(?.%F{green}✔.%F{red}✘) %F{white}> '

# Menu completion will be started unconditionally
zstyle ':completion:*' menu select

# Use $LS_COLORS for completion as well
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Automatically find new executables in $PATH
zstyle ':completion:*' rehash true

# Only try to detect git
zstyle ':vcs_info:*' enable git

# Show when the working directory has uncommitted changes
zstyle ':vcs_info:*' check-for-changes true

# Call git-ahead when populating the message variables
zstyle ':vcs_info:git+set-message:*' hooks git-ahead

# Add %m (misc) to the default formats
zstyle ':vcs_info:git*' formats ' (%s)-[%b]%u%c-%m'

function +vi-git-ahead() {
    hook_com[misc]=$(git rev-list --count @{u}..HEAD 2>/dev/null)
}

# Useful with auto_cd
alias ...=../..

# Prompt before overwrite/removal
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# Use colors when stdout is connected to a terminal
alias ls='ls --color=auto'

# List all entries except . and ..
alias la='ls -A'

# Use long listing format with human readable sizes
alias ll='ls -lh'

# Combine ll and la
alias lla='ll -A'

# See the ls alias
alias grep='grep --color=auto'

# Always use colors
alias tree='tree -C'

# Run command based on file type
alias x=xdg-open

# The default permissionis are 644 for files and 755 for directories
umask 022

# Setup external stuff
. ~/.sdkman/bin/sdkman-init.sh
. /usr/share/virtualenvwrapper/virtualenvwrapper.sh
. <(fzf --zsh)

# Local hook
. ~/.zshrc.local
