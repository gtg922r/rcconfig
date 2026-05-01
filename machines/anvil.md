# Anvil

> FoundryVTT host — dedicated VM for running a Foundry Virtual Tabletop instance.

Created: 2025-05-01
Type: VM (exe.dev)

## Packages

Install via apt:
- bat (syntax-highlighting cat replacement; alias `bat` to `batcat`)
- emacs
- figlet (required by mkbanner)

Install Node.js LTS as a system package via NodeSource apt repo (`deb.nodesource.com`) — required by FoundryVTT.
Install nvm (latest stable) with no default Node version — fallback for projects needing a different Node version. System `/usr/bin/node` is primary.

## CLI Tools

- **GitHub CLI** — install via apt, authenticate as gtg922r (https protocol)
- **Cursor CLI** — install via the official installer (see https://cursor.com/docs/cli/headless), available as `agent` command

## npm Global Packages

- `@google/gemini-cli@preview` — run with `gemini`
- `@foundryvtt/foundryvtt-cli` — run with `fvtt`; `dataPath` configured to
  `~/.local/share/FoundryVTT`. See the `foundryvtt-cli` Shelley skill for usage.

## Banner

Generate `~/.bash_banner` using `~/.rcconfig/tools/mkbanner/mkbanner`.

- Title: ANVIL
- Subtitle: "FoundryVTT host"
- Orange → brown gradient: start 240,160,80 end 140,70,30
- Padding: 2
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version

## Machine-Specific Shell Config

Generate into `~/.bash_local`:

```bash
# Anvil — FoundryVTT host
export MACHINE_ROLE="foundryvtt"
export MACHINE_NAME="anvil"
```

## FoundryVTT

- Install location: `~/foundryvtt/` (unzipped Linux distribution)
- User data: `~/.local/share/FoundryVTT/` (Foundry default)
- Run via systemd unit `foundryvtt.service` (`/etc/systemd/system/foundryvtt.service`)
- Listens on port **3000** (within exe.dev proxy range 3000-9999)
- UPnP disabled (`--noupnp`); access via `https://anvil.exe.xyz:3000/` once the port is set public
- Manage: `systemctl status|restart foundryvtt`, `journalctl -u foundryvtt -f`
- Upgrade: stop service, replace `~/foundryvtt/` contents with new release zip, start service

## Repositories

(None yet — FoundryVTT install lives outside the dotfiles repo.)

## Agent Integration

Shelley is the primary LLM coding agent. The root-level agent config at `~/.config/shelley/AGENTS.md` should include:

> This machine's environment is managed by `~/.rcconfig`. Read `~/.rcconfig/AGENTS.md` for the full contract. The short version: when you install tools, add aliases, or change the environment, update `~/.rcconfig/this_machine.md` first, then apply the change. The spec is the source of truth.

## Notes

- Dedicated VM for hosting FoundryVTT (Virtual Tabletop).
- Orange/brown (forge/anvil) color theme — fitting the name.
- Shelley is the primary agent on this VM.
