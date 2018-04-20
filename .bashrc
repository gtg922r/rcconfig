
# Full `ls` listing
alias ls='ls -laGh'

# Command line emacs opens in shell
alias emacs='emacs -nw'

# Set Command line formatting
export PS1="\[$(tput setaf 11)\](\t) \[\033[38;5;246m\]\u\[$(tput sgr0)\]\[\033[38;5;236m\] @ \[$(tput sgr0)\]\[\033[38;5;241m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\]: \[$(tput setaf 4)\]\w \[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "  

# Use Color on the command line
export CLICOLOR=1

# Use emacs as default editor
export EDITOR="emacs"

# Add binaries installed via Homebrew to our PATH.
export PATH=$HOME/.cask/bin:$PATH
export PATH=$HOME/homebrew/bin:$PATH


# export PS1="\[$(tput setab 8)\]\[$(tput setaf 10)\](\t) \[$(tput setaf 11)\]\u\[$(tput setaf 10)\]: \[$(tput setaf 4)\]\w \[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "
