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

## Binaries

- **SilverBullet** — browser-based markdown PKM, installed as a single binary at `/usr/local/bin/silverbullet`. Update via `silverbullet upgrade`.

## Systemd Services

- **obsidian-sync** — runs `ob sync --path /home/exedev/vault --continuous` to keep the "Personal" vault in sync. Config: bidirectional, all file types, merge conflicts, device name `exe-dev-vm`.
- **silverbullet** — SilverBullet server on `127.0.0.1:3000`, serving `~/vault`. Shell backend disabled. Ignores `.obsidian/*`.
- **nginx** — reverse proxy on ports 80 and 8000, proxies to SilverBullet. Enforces email whitelist (`rc@ryancash.com`) via `X-ExeDev-Email` header. Allows unauthenticated access to PWA assets (`/service_worker.js`, `/.client/`).

## Web Access

- SilverBullet is accessible at `https://pkm-vault.exe.xyz/` via the exe.dev HTTPS proxy (private, requires exe.dev login).
- nginx provides a second auth layer: only requests with `X-ExeDev-Email: rc@ryancash.com` are allowed through.
- The exe.dev proxy target port must be set to 80 via `ssh exe.dev share port pkm-vault 80` (run from outside the VM).

## Notes

- Dedicated VM for PKM workflows, Obsidian vault sync, and knowledge base tooling.
- Obsidian "Personal" vault syncs to `/home/exedev/vault/` via obsidian-headless.
- SilverBullet and Obsidian sync coexist on `~/vault` — SB ignores `.obsidian/*`, Obsidian ignores `_silverbullet/`.
