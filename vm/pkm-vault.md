---
hostname: pkm-vault
description: "Personal knowledge management and Obsidian vault sync"
created: 2025-07-14

banner:
  text: PKM-VAULT
  subtitle: "knowledge management VM"
  gradient_start: "136,192,208"
  gradient_end: "56,112,148"
  padding: 2
  info_box: true

packages:
  apt:
    - bat
    - emacs
    - figlet
  nvm: latest
  node: lts
  npm_global:
    - "@google/gemini-cli@preview"

cli_tools:
  github_cli:
    install: apt
    auth_user: gtg922r
    auth_protocol: https
  cursor_cli: true

repos: []

shell_extras:
  aliases: {}
  env: {}
---

# PKM-Vault

> Personal knowledge management and Obsidian vault operations.

## Purpose

Dedicated VM for PKM workflows, Obsidian vault sync, and knowledge base tooling.

## Notes

- Created 2025-07-14
- Nord blue gradient for banner
