# Bootstrap a New Machine

Instructions for first-time setup. After bootstrap is complete, see `AGENTS.md` for ongoing maintenance.

## 1. Clone

```
git clone https://github.com/gtg922r/rcconfig.git ~/.rcconfig
```

## 2. Identify This Machine

Check `machines/` for an existing spec matching this machine. If one exists, symlink it:

```
ln -s machines/<name>.md ~/.rcconfig/this_machine.md
```

If no spec exists yet, create one. Copy `machines/_template.md` to `machines/<name>.md` and ask the user:

- What is this machine? (VM, laptop, desktop?)
- What's it for?
- Any specific tools or packages beyond the baseline?
- Color/vibe preference for the login banner?

Then create the symlink.

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

## 5. Verify

`source ~/.bashrc` and confirm everything loads cleanly. The banner should display, the prompt should look right, and all declared tools should be available.

## 6. Commit the Spec

If a new machine spec was created in step 2, commit and push it so the fleet registry stays current.
