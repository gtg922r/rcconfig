# rcconfig — Agent Instructions

This repo is the source of truth for shell configuration across all VMs.
Clone it to `~/.rcconfig`. Read this file first, then act.

## Repo Structure

```
shell/          Shared shell configs — symlinked into ~
tools/mkbanner  ASCII art banner generator (figlet + gradient)
vm/             Per-VM declarative specs (the fleet registry)
vm/_template.md Starting point for new VMs
```

## Bootstrap a New VM

1. **Determine hostname:** `hostname -s`
2. **Find or create VM spec:** Look for `vm/<hostname>.md`. If it doesn't exist, copy `vm/_template.md` and ask the user:
   - What is this VM for? (one-line description)
   - Any extra packages beyond the baseline?
   - Banner color preference? (or pick one that fits the vibe)
3. **Install shared shell configs:**
   ```
   ln -sf ~/.rcconfig/shell/bashrc ~/.bashrc
   ln -sf ~/.rcconfig/shell/bash_prompt ~/.bash_prompt
   ln -sf ~/.rcconfig/shell/bash_aliases ~/.bash_aliases
   ```
4. **Install system packages** declared in the VM spec
5. **Install tools** declared in the VM spec (nvm, node, npm packages, CLI tools)
6. **Generate banner:** Use `~/.rcconfig/tools/mkbanner/mkbanner` with the colors and title from the VM spec. Write output logic to `~/.bash_banner` (see existing VM specs for the full banner template including the info box)
7. **Generate `~/.bash_local`** from `shell_extras` in the VM spec (aliases, env vars)
8. **Verify:** `source ~/.bashrc` and confirm everything loads

## Maintenance Contract

When the user asks to install a tool, change config, or modify the environment:

1. **Update the VM spec first** (`vm/<hostname>.md`) — this is the source of truth
2. **Then apply the change** to the live system
3. **Commit and push** the spec change

If the VM spec and the live system disagree, the spec wins. Re-apply from spec.

## What Lives Where

| Item | Location | In repo? |
|------|----------|----------|
| Shared shell configs | `~/.rcconfig/shell/` → symlinked to `~/` | ✅ |
| VM spec | `~/.rcconfig/vm/<hostname>.md` | ✅ |
| mkbanner tool | `~/.rcconfig/tools/mkbanner/` | ✅ |
| Generated banner | `~/.bash_banner` | ❌ (generated) |
| VM-local overrides | `~/.bash_local` | ❌ (generated) |
| Secrets / auth | managed interactively | ❌ (never) |

## Adding a New VM to the Fleet

1. Copy `vm/_template.md` → `vm/<hostname>.md`
2. Fill in the spec (see template for fields)
3. Commit to main and push
4. On the new VM: `git clone` → follow bootstrap steps above
