# [VM Name]

> One-line description of what this VM is for.

Created: [date]

## Packages

Install via apt:
- bat (syntax-highlighting cat replacement; alias `bat` to `batcat`)
- emacs
- figlet (required by mkbanner)

Install nvm (latest stable), then Node.js LTS as the default.

## CLI Tools

- **GitHub CLI** — install via apt, authenticate as gtg922r (https protocol)
- **Cursor CLI** — install via the official installer, available as `agent` command

## npm Global Packages

- `@google/gemini-cli@preview` — run with `gemini`

## Banner

Generate `~/.bash_banner` using `~/.rcconfig/tools/mkbanner/mkbanner`.

- Title: [VM hostname in uppercase]
- Subtitle: [short description]
- Colors: [pick something that fits the VM's vibe, or use the lavender default: start 200,170,220 end 125,100,195]
- Include an info box below the art showing HOST, DATE, TIME, and SHELLEY version (see pyronic's banner as reference)

## VM-Specific Shell Config

Generate `~/.bash_local` with any overrides needed for this VM. The shared bashrc sources it last, so anything here wins.

[Add any VM-specific aliases, environment variables, or shell tweaks here. Delete this section if none needed.]

## Repositories

[List repos to clone, with target paths. Note which are private (require gh auth).]

## Notes

[Anything else an agent or human should know about this VM.]
