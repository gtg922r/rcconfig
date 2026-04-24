# Mira-AI

> AI Agent host — Mira, an autonomous coding and task agent.

Created: 2025-11-19
Type: VM (exe.dev)

## Packages

Install via apt:
- bat (syntax-highlighting cat replacement; alias `bat` to `batcat`)
- emacs
- figlet (required by mkbanner)

Node.js LTS installed as system package via NodeSource apt repo (`deb.nodesource.com`).
nvm installed (no default node version) — available for projects needing a different Node version.
System `/usr/bin/node` is the primary; nvm is a fallback tool, not the default.

## CLI Tools

- **GitHub CLI** — install via apt, authenticate as gtg922r (https protocol)
- **Cursor CLI** — install via the official installer (see https://cursor.com/docs/cli/headless), available as `agent` command

## npm Global Packages

- `@google/gemini-cli@preview` — run with `gemini`

## Banner

Generate `~/.bash_banner` using `~/.rcconfig/tools/mkbanner/mkbanner`.

- Title: MIRA
- Subtitle: "autonomous AI agent"
- Blue-green gradient: start 80,200,190 end 40,110,180
- Padding: 2
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version

## Machine-Specific Shell Config

Generate into `~/.bash_local`:

```bash
# Mira-AI — autonomous AI agent host
export MACHINE_ROLE="ai-agent"
export MACHINE_NAME="mira-ai"
```

## Repositories

(None yet — repos will be added as projects are created.)

## Agent Integration

Shelley is the primary LLM coding agent. The root-level agent config at `~/.config/shelley/AGENTS.md` should include:

> This machine's environment is managed by `~/.rcconfig`. Read `~/.rcconfig/AGENTS.md` for the full contract. The short version: when you install tools, add aliases, or change the environment, update `~/.rcconfig/this_machine.md` first, then apply the change. The spec is the source of truth.

## Notes

- Dedicated VM for Mira, an AI coding/task agent.
- Blue-green (teal→deep blue) color theme throughout.
- Shelley is the primary agent on this VM.
