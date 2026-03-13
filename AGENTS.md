# rcconfig — Agent Guide

This repo is the source of truth for shell and environment configuration across all machines (VMs, laptops, desktops). It lives at `~/.rcconfig`.

For first-time setup of a new machine, see `BOOTSTRAP.md`.

## Identifying This Machine

`this_machine.md` is a symlink to this machine's spec in `machines/`. It's gitignored — every machine points to its own file. Read it to know what this machine is and how it should be configured.

## Repo Layout

```
this_machine.md      → machines/<name>.md (gitignored symlink)
machines/            One spec per machine — the fleet registry
shell/               Shared bash configs (symlinked into ~)
tools/mkbanner/      ASCII art banner generator
```

## How Configuration Works

**Shared shell configs** in `shell/` are symlinked into `~` as dotfiles. They're the same on every machine.

**Machine-specific config** is described in each machine's spec (`machines/<name>.md`). Anything that needs to differ — extra aliases, environment variables, tool-specific setup — gets generated into `~/.bash_local`, which the shared bashrc sources last. Machine-specific overrides always win.

**The banner** is generated per-machine using `tools/mkbanner/`. The machine spec describes the desired look. The output goes to `~/.bash_banner`.

## Ongoing Maintenance

When the user asks to install a tool, add an alias, change the environment, or modify any aspect of this machine's setup:

1. **Update the machine spec first** — open `this_machine.md` and describe the change
2. **Apply the change** to the live system
3. **Regenerate `~/.bash_local`** if the change involves shell overrides
4. **Commit and push**

The spec is the source of truth. If the spec and the live system disagree, re-apply from the spec.

## What Goes Where

- **In this repo:** shared shell configs, mkbanner tool, machine specs
- **Generated into `~`:** `.bash_banner`, `.bash_local` (not tracked in git)
- **Never in this repo:** secrets, auth tokens, SSH keys
