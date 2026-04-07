# rcconfig

Shell and environment configuration for all machines — VMs, laptops, desktops. Written for humans and LLM agents to read together.

Clone to `~/.rcconfig`. An agent can read `BOOTSTRAP.md` to set up a new machine, or `AGENTS.md` to maintain an existing one.

```bash
git clone https://github.com/gtg922r/rcconfig.git ~/.rcconfig
```

## Structure

```
AGENTS.md            How agents should maintain this machine
BOOTSTRAP.md         First-time setup for a new machine
init.sh              Sourced from bashrc — PATH, vault env, shell helpers
bin/                 All executable scripts (single PATH entry)
this_machine.md      → symlink to this machine's spec (gitignored)
machines/            One file per machine — the fleet registry
shell/               Shared bash configs (symlinked into ~)
vault/               Encrypted credential vault docs
tools/mkbanner/      ASCII art banner generator
```

## Adding a New Machine

Copy `machines/_template.md` to `machines/<name>.md`, describe what you want, symlink it as `this_machine.md`, and let your agent handle the rest.
