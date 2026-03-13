# rcconfig

Declarative shell configuration for cloud VMs, designed for the LLM agent era.

Clone to `~/.rcconfig` on any VM. An LLM agent can read `AGENTS.md` and fully bootstrap the machine.

## Quick Start

```bash
git clone https://github.com/gtg922r/rcconfig.git ~/.rcconfig
# Then tell your LLM agent: "Bootstrap this VM using ~/.rcconfig"
```

## Philosophy

- **Declarative over imperative** — VM specs describe *what* should exist, not *how* to install it
- **Single branch, flat fleet** — every VM's spec lives in `vm/` on main. No branch gymnastics
- **Spec is source of truth** — if the system and spec disagree, re-apply from spec
- **LLM-native** — `AGENTS.md` gives any coding agent full context to bootstrap or maintain a VM

## Structure

```
AGENTS.md           ← LLM agents start here
shell/              ← Shared bash configs (symlinked into ~)
tools/mkbanner/     ← ASCII art banner generator
vm/                 ← Per-VM declarative specs
vm/_template.md     ← Copy this for new VMs
```

## Fleet

See `vm/` for all configured VMs. Each `.md` file is a complete declarative spec.
