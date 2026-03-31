# Willow-AI

> AI family assistant host — automation, scheduling, and household intelligence.

Created: 2025-07-15
Type: VM (exe.dev)

## Packages

Install via apt:
- bat (syntax-highlighting cat replacement; alias `bat` to `batcat`)
- emacs
- figlet (required by mkbanner)

Install nvm (latest stable), then Node.js LTS as the default.

## CLI Tools

- **GitHub CLI** — install via apt, authenticate as gtg922r (https protocol)
- **Cursor CLI** — install via the official installer (see https://cursor.com/docs/cli/headless), available as `agent` command

## npm Global Packages

- `@google/gemini-cli@preview` — run with `gemini`

## Banner

Generate `~/.bash_banner` using `~/.rcconfig/tools/mkbanner/mkbanner`.

- Title: WILLOW-AI
- Subtitle: "AI family assistant"
- Pink → purple gradient: start 255,130,200 end 160,60,210
- Padding: 2
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version

## Machine-Specific Shell Config

Generate into `~/.bash_local`:

```bash
# Willow-AI — AI family assistant host
export MACHINE_ROLE="ai-assistant"
export MACHINE_NAME="willow-ai"
```

## Repositories

(None yet — repos will be added as projects are created.)

## Agent Integration

Shelley is the primary LLM coding agent. The root-level agent config at `~/.config/shelley/AGENTS.md` should include:

> This machine's environment is managed by `~/.rcconfig`. Read `~/.rcconfig/AGENTS.md` for the full contract. The short version: when you install tools, add aliases, or change the environment, update `~/.rcconfig/this_machine.md` first, then apply the change. The spec is the source of truth.

## Notes

- Dedicated VM for AI-powered family assistant services.
- Pink-purple color theme throughout.
- Shelley is the primary agent on this VM.
