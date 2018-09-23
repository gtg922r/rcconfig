# Import Command Line
source ~/.bash_prompt

# Full `ls` listing
alias ls='ls -laGh'

# Command line emacs opens in shell
alias emacs='emacs -nw'

# Use Color on the command line
export CLICOLOR=1

# Use emacs as default editor
export EDITOR="emacs"

# Add binaries installed via Homebrew to our PATH.
export PATH=$HOME/.cask/bin:$PATH
export PATH=$HOME/homebrew/bin:$PATH

# Pyenv Setup (https://github.com/pyenv/)
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

