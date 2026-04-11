# Selene-AI

> AI Agent host — Selene, an autonomous coding and task agent.

Created: 2025-07-15
Type: VM (exe.dev)

## Packages

Install via apt:
- bat (syntax-highlighting cat replacement; alias `bat` to `batcat`)
- emacs
- figlet (required by mkbanner)

Node.js 24 installed as system package via NodeSource apt repo (`deb.nodesource.com/node_24.x`).
nvm installed (no default node version) — available for projects needing a different Node version.
System `/usr/bin/node` is the primary; nvm is a fallback tool, not the default.

## CLI Tools

- **GitHub CLI** — install via apt, authenticate as gtg922r (https protocol)
- **Cursor CLI** — install via the official installer (see https://cursor.com/docs/cli/headless), available as `agent` command

## npm Global Packages

- `@google/gemini-cli@preview` — run with `gemini`

## Banner

Generate `~/.bash_banner` using `~/.rcconfig/tools/mkbanner/mkbanner`.

- Title: SELENE
- Subtitle: "autonomous AI agent"
- Red-purple gradient: start 255,60,80 end 140,30,180
- Padding: 2
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version

## Machine-Specific Shell Config

Generate into `~/.bash_local`:

```bash
# Selene-AI — autonomous AI agent host
export MACHINE_ROLE="ai-agent"
export MACHINE_NAME="selene-ai"
```

## Repositories

(None yet — repos will be added as projects are created.)

## Agent Integration

Shelley is the primary LLM coding agent. The root-level agent config at `~/.config/shelley/AGENTS.md` should include:

> This machine's environment is managed by `~/.rcconfig`. Read `~/.rcconfig/AGENTS.md` for the full contract. The short version: when you install tools, add aliases, or change the environment, update `~/.rcconfig/this_machine.md` first, then apply the change. The spec is the source of truth.

## Notes

- Dedicated VM for Selene, an AI coding/task agent.
- Red-purple color theme throughout.
- Shelley is the primary agent on this VM.
