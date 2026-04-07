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
init.sh              Sourced from bashrc — PATH, vault env, shell helpers
bin/                 All executable scripts (single PATH entry)
vault/               Encrypted credential vault docs (see vault/README.md)
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

The spec is the source of truth. If the spec and the live system disagree, flag it to the user and ask how they'd like to proceed before making changes. Don't silently re-apply — the divergence might be intentional, or the spec might be the thing that needs updating.

## Credential Vault

The vault (`vault/`) provides an encrypted credential store using gocryptfs. See `vault/README.md` for full details.

- **`vault-open` / `vault-close`** control access to credentials
- Env vars in `~/.credentialVault/.env` are loaded automatically when the vault is open
- Symlink mappings in `~/.config/credentialVault/links.conf` connect app credential dirs
- For services that spawn bare `bash -c` shells, set `BASH_ENV=~/.rcconfig/vault/vault-bash-env.sh` via systemd drop-in

Don't store vault contents (`.env`, credentials) in this repo. Only the scripts and documentation are tracked.

## What Goes Where

- **In this repo:** shared shell configs, init.sh, vault scripts, mkbanner tool, machine specs
- **Generated into `~`:** `.bash_banner`, `.bash_local` (not tracked in git)
- **Never in this repo:** secrets, auth tokens, SSH keys, vault contents
