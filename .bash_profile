if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

# Pyenv Setup (https://github.com/pyenv/) if installed
if command -v pyenv 1>/dev/null 2>&1; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
fi

