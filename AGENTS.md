# rcconfig — Agent Guide

This repo defines how VMs should be configured. It lives at `~/.rcconfig`.

## How This Works

**Shared shell configs** live in `shell/`. They get symlinked into `~` as dotfiles (e.g., `shell/bashrc` → `~/.bashrc`). These are the same across all VMs.

**VM-specific configs** live in `vm/<hostname>.md`. Each file is a complete, readable description of what should be installed and configured on that machine. If a VM needs different aliases, extra tools, or custom environment variables, that's described in its VM spec — and you generate `~/.bash_local` with whatever's needed. The shared bashrc sources `~/.bash_local` at the end, so VM-specific overrides always win.

**The banner** is generated per-VM using `tools/mkbanner/`. The VM spec describes the desired look. You generate `~/.bash_banner` with the banner art plus an info box showing host, date, time, and Shelley version. See pyronic's spec for the reference pattern.

## Bootstrap a New VM

1. Clone this repo to `~/.rcconfig`
2. Look for `vm/<hostname>.md` — if it doesn't exist, read `vm/_template.md` and create one by asking the user:
   - What's this VM for?
   - Any specific tools or packages needed?
   - Color/vibe preference for the banner?
3. Symlink shared shell configs from `shell/` into `~` (back up any existing files first)
4. Read the VM spec and make it so — install packages, set up tools, generate the banner and bash_local
5. Verify everything loads: `source ~/.bashrc`

## Ongoing Maintenance

When the user asks to change something about the VM's environment:

1. **Update the VM spec first** — it's the source of truth
2. Apply the change to the live system
3. Commit and push

If the spec and reality disagree, the spec wins.

## What Goes Where

- **In this repo:** shared shell configs, mkbanner tool, VM specs
- **Generated into `~`:** `.bash_banner`, `.bash_local` (not tracked in git)
- **Never in this repo:** secrets, auth tokens, SSH keys
