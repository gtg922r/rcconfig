# ~/.rcconfig/init.sh — sourced from ~/.bashrc (after interactive guard).
# Sets up PATH, credential vault env, and shell helpers.

export PATH="$HOME/.rcconfig/bin:$PATH"

# Load vault secrets (also loaded before the .bashrc interactive guard
# for non-interactive shells, and via BASH_ENV for service-spawned shells)
[ -f ~/.credentialVault/.env ] && . ~/.credentialVault/.env

# Shell wrappers so vault-open/close affect the current shell's env
vault-open() {
    command vault-open "$@" && [ -f ~/.credentialVault/.env ] && source ~/.credentialVault/.env
}
vault-close() {
    local _vars=""
    [ -f ~/.credentialVault/.env ] && _vars=$(grep -oP '(?<=^export )\w+' ~/.credentialVault/.env 2>/dev/null)
    command vault-close "$@"
    for v in $_vars; do unset "$v" 2>/dev/null; done
}
