# Sourced via BASH_ENV by services/agents — loads vault secrets only.
# For services that spawn bare `bash -c` shells that don't read .bashrc.
[ -f ~/.credentialVault/.env ] && . ~/.credentialVault/.env
