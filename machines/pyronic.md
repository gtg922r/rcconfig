# Pyronic

> Primary exe.dev development VM. General-purpose coding, experimentation, and project hosting.

Created: 2025-01-13

## Packages

Install via apt:
- bat (syntax-highlighting cat replacement; alias `bat` to `batcat`)
- emacs
- figlet (required by mkbanner)

Install Node.js LTS as a system package via NodeSource apt repo (`deb.nodesource.com`).
Install nvm (latest stable) with no default Node version — available as a fallback for projects needing a different version. System `/usr/bin/node` is primary.

## CLI Tools

- **GitHub CLI** — install via apt, authenticate as gtg922r (https protocol)
- **Cursor CLI** — install via the official installer (see https://cursor.com/docs/cli/headless), available as `agent` command

## npm Global Packages

- `@google/gemini-cli@preview` — run with `gemini`

## Banner

Generate `~/.bash_banner` using `~/.rcconfig/tools/mkbanner/mkbanner`.

- Title: PYRONIC
- Subtitle: "exe.dev development VM"
- Lavender → purple gradient: start 200,170,220 end 125,100,195
- Padding: 2
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version

## Repositories

All private unless noted. Clone via `gh repo clone`.

- gtg922r/ai-workflow → ~/ai-workflow
- gtg922r/helping-hands → ~/helping-hands
- gtg922r/llm-council → ~/llm-council
- gtg922r/obsidian-numerals → ~/obsidian-numerals (public)
- gtg922r/obsidian-sankey → ~/obsidian-sankey (public)
- gtg922r/obsidian-entities → ~/obsidian-entities (public)
- gtg922r/Minerva → ~/Minerva
- gtg922r/iris-sync → ~/iris-sync
- gtg922r/hermes → ~/hermes
- gtg922r/hestia-timer → ~/hestia-timer

## Notes

- This is the daily driver. Hosts Obsidian plugins, web apps, sync tools, and LLM experiments.
- See `~/PYRONIC_APPS.md` for full project descriptions.
- Shelley is the primary LLM coding agent on this VM.
