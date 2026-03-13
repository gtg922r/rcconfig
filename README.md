# rcconfig

Shell configuration for cloud VMs, written for humans and LLM agents to read together.

Clone to `~/.rcconfig` on any VM. An agent can read `AGENTS.md` and bootstrap the machine from there.

```bash
git clone https://github.com/gtg922r/rcconfig.git ~/.rcconfig
# Then tell your agent: "Bootstrap this VM using ~/.rcconfig"
```

## Structure

```
AGENTS.md            Agent reads this first
shell/               Shared bash configs (symlinked into ~)
tools/mkbanner/      ASCII art banner generator
vm/                  One file per VM — the fleet registry
vm/_template.md      Starting point for new VMs
```

## Adding a New VM

Copy `vm/_template.md` to `vm/<hostname>.md`, describe what you want, and let your agent handle the rest.
