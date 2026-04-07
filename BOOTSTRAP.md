# Bootstrap a New Machine

Instructions for first-time setup. After bootstrap is complete, see `AGENTS.md` for ongoing maintenance.

## 1. Clone

```
git clone https://github.com/gtg922r/rcconfig.git ~/.rcconfig
```

## 2. Identify This Machine

Check if `this_machine.md` already exists as a symlink. If it does, you're set — skip to step 3.

Otherwise, check `machines/` for a spec that matches the hostname (`hostname -s`). If one exists, confirm with the user: "This looks like [name] — is that right?" If they confirm, create the symlink.

If no matching spec exists, ask the user what name to use (suggest the hostname as default). Then create a new spec — copy `machines/_template.md` to `machines/<name>.md` and ask:

- What is this machine? (VM, laptop, desktop?)
- What's it for?
- Any specific tools or packages beyond the baseline?
- Color/vibe preference for the login banner?

Then create the symlink:

```
ln -s machines/<name>.md ~/.rcconfig/this_machine.md
```

## 3. Link Shell Configs

Symlink the shared shell configs from `shell/` into `~`. Back up any existing files first.

- `shell/bashrc` → `~/.bashrc`
- `shell/bash_prompt` → `~/.bash_prompt`
- `shell/bash_aliases` → `~/.bash_aliases`

## 4. Apply the Machine Spec

Read `this_machine.md` and make it real:

- Install declared packages
- Set up declared CLI tools
- Clone declared repositories
- Generate `~/.bash_banner` using `tools/mkbanner/` with the banner settings from the spec
- Generate `~/.bash_local` with any machine-specific aliases, env vars, or overrides

## 5. Credential Vault (optional)

If this machine needs encrypted credential storage (e.g., for LLM agent access control):

```bash
sudo apt install gocryptfs    # or: brew install gocryptfs
vault-init                    # creates encrypted volume, prompts for password
```

Save the master key it displays. Then add app credentials — see `vault/README.md`.

## 6. Verify

`source ~/.bashrc` and confirm everything loads cleanly. The banner should display, the prompt should look right, and all declared tools should be available.

## 7. Commit the Spec

If a new machine spec was created in step 2, commit and push it so the fleet registry stays current.
