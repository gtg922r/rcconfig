# Credential Vault

An encrypted directory (using [gocryptfs](https://github.com/rfjakob/gocryptfs))
that stores secrets, credentials, and environment variables. Mount it to make
credentials available; unmount it to revoke access instantly.

Designed for machines where LLM agents have full filesystem access — the vault
gives you a simple on/off switch for credential access.

## Paths

| Path | Purpose |
|---|---|
| `~/.config/credentialVault/cipher/` | Encrypted data (always on disk, unreadable) |
| `~/.credentialVault/` | Decrypted mount point (only populated when open) |
| `~/.credentialVault/.env` | Environment variables (exported when vault is open) |
| `~/.config/credentialVault/links.conf` | Symlink mappings (not secret, lives outside vault) |

## Commands

```bash
vault-init       # One-time setup: initialize encryption, create mount point
vault-open       # Mount the vault (prompts for password), activate symlinks & env
vault-close      # Unmount the vault, remove symlinks, unset env vars
vault-status     # Show whether vault is open/closed, contents, links, env vars
```

## First-time setup

```bash
# 1. Install gocryptfs
sudo apt install gocryptfs    # Ubuntu/Debian
brew install gocryptfs        # macOS

# 2. Initialize the vault (sets encryption password)
vault-init

# 3. Save the master key it displays (for emergency recovery)

# 4. Add credentials and env vars to the vault
#    (vault is auto-mounted after init)
```

## Adding a new app

Two things to configure — no scripts need to change:

**1. Symlinks** — edit `~/.config/credentialVault/links.conf`:

```conf
# <dir-inside-vault>    <target-symlink-path>
gogcli-keyring          ~/.config/gogcli/keyring
my-app-secrets          ~/.config/myapp/credentials
```

When the vault opens, each vault dir is symlinked to its target path.
When it closes, symlinks are removed.

**2. Environment variables** — edit `~/.credentialVault/.env`:

```bash
export MY_APP_API_KEY="sk-..."
export SOME_OTHER_SECRET="..."
```

These are exported into every shell when the vault is open.

## How env loading works

The vault `.env` is loaded through multiple paths to cover all shell types:

| Shell type | Loads via |
|---|---|
| Interactive login (SSH) | `.bashrc` → `.env` + `init.sh` |
| Interactive non-login (tmux) | `.bashrc` → `.env` + `init.sh` |
| Non-interactive (`bash -c` reading `.bashrc`) | `.bashrc` → `.env` |
| Service-spawned (bare `bash -c`) | `BASH_ENV` → `vault-bash-env.sh` → `.env` |

For services/agents that spawn bare shells (not login, not interactive, no
`.bashrc`), add a systemd drop-in to set `BASH_ENV`:

```ini
# ~/.config/systemd/user/<service>.service.d/vault-env.conf
[Service]
Environment=BASH_ENV=/home/<user>/.rcconfig/vault/vault-bash-env.sh
```

## Security model

- **Vault open**: credentials accessible, env vars set, symlinks active
- **Vault closed**: encrypted data unreadable, symlinks removed, env vars unset
- Encryption: AES-256-GCM via gocryptfs
- Password required each time the vault is opened
- No secrets are stored in plaintext outside the encrypted volume
