---
hostname: HOSTNAME
description: "Short description of this VM's purpose"
created: YYYY-MM-DD

banner:
  text: HOSTNAME
  subtitle: "description for banner subtitle"
  gradient_start: "200,170,220"
  gradient_end: "125,100,195"
  padding: 2

packages:
  apt:
    - bat
    - emacs
    - figlet
  nvm: latest
  node: lts
  npm_global: []

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

# HOSTNAME

> One-line description of what this VM is for.

## Purpose

Describe the VM's role, what projects live here, why it exists.

## Notes

- Created YYYY-MM-DD
- Any special configuration notes
