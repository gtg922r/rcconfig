#!/bin/bash
# Symlink shared shell configs from ~/.rcconfig/shell into ~
# Idempotent — safe to run multiple times.

set -euo pipefail

SHELL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

links=(
    "bashrc:.bashrc"
    "bash_prompt:.bash_prompt"
    "bash_aliases:.bash_aliases"
)

for entry in "${links[@]}"; do
    src="${SHELL_DIR}/${entry%%:*}"
    dst="${HOME}/${entry##*:}"

    if [[ -L "$dst" && "$(readlink -f "$dst")" == "$(readlink -f "$src")" ]]; then
        echo "  ✓ $dst (already linked)"
    else
        [[ -e "$dst" ]] && mv "$dst" "${dst}.bak.$(date +%s)"
        ln -sf "$src" "$dst"
        echo "  → $dst -> $src"
    fi
done

echo
echo "Shell configs linked. Run 'source ~/.bashrc' to activate."
