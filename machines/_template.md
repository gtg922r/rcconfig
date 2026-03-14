# [Machine Name]

> One-line description of what this machine is for.

Created: [date]
Type: [VM / laptop / desktop]

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

- Title: [machine name in uppercase]
- Subtitle: [short description]
- Colors: [pick something that fits, or use the lavender default: start 200,170,220 end 125,100,195]
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version (see pyronic's banner as reference)

## Machine-Specific Shell Config

Anything here gets generated into `~/.bash_local`. The shared bashrc sources it last, so overrides win.

[Add any machine-specific aliases, environment variables, or shell tweaks here. Delete this section if none needed.]

## Repositories

[List repos to clone, with target paths. Note which are private (require gh auth).]

## Agent Integration

If this machine runs an LLM coding agent (Shelley, etc.), add the following to its root-level agent config (e.g., `~/.config/shelley/AGENTS.md`):

> This machine's environment is managed by `~/.rcconfig`. Read `~/.rcconfig/AGENTS.md` for the full contract. The short version: when you install tools, add aliases, or change the environment, update `~/.rcconfig/this_machine.md` first, then apply the change. The spec is the source of truth.

## Notes

[Anything else an agent or human should know about this machine.]
