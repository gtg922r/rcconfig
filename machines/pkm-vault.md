# PKM-Vault

> Personal knowledge management and Obsidian vault operations.

Created: 2025-07-14

## Packages

Install via apt:
- bat (syntax-highlighting cat replacement; alias `bat` to `batcat`)
- emacs
- figlet (required by mkbanner)

Install Node.js 22 via NodeSource apt repo (required by obsidian-headless).

## CLI Tools

- **GitHub CLI** — install via apt, authenticate as gtg922r (https protocol)
- **Cursor CLI** — install via the official installer (see https://cursor.com/docs/cli/headless), available as `agent` command

## npm Global Packages

- `obsidian-headless` — Obsidian Sync headless client (`ob` command)
- `@google/gemini-cli@preview` — run with `gemini`

## Banner

Generate `~/.bash_banner` using `~/.rcconfig/tools/mkbanner/mkbanner`.

- Title: PKM-VAULT
- Subtitle: "knowledge management VM"
- Nord blue gradient: start 136,192,208 end 56,112,148
- Padding: 2
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version

## Systemd Services

- **obsidian-sync** — runs `ob sync --path /home/exedev/vault --continuous` to keep the "Personal" vault in sync. Config: bidirectional, all file types, merge conflicts, device name `exe-dev-vm`.

## Notes

- Dedicated VM for PKM workflows, Obsidian vault sync, and knowledge base tooling.
- Obsidian "Personal" vault syncs to `/home/exedev/vault/` via obsidian-headless.
