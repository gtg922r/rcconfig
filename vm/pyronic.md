---
hostname: pyronic
description: "Primary exe.dev development VM"
created: 2025-01-13

banner:
  text: PYRONIC
  subtitle: "exe.dev development VM"
  gradient_start: "200,170,220"
  gradient_end: "125,100,195"
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

repos:
  - repo: gtg922r/ai-workflow
    path: ~/ai-workflow
    private: true
  - repo: gtg922r/helping-hands
    path: ~/helping-hands
    private: true
  - repo: gtg922r/llm-council
    path: ~/llm-council
    private: true
  - repo: gtg922r/obsidian-numerals
    path: ~/obsidian-numerals
  - repo: gtg922r/obsidian-sankey
    path: ~/obsidian-sankey
  - repo: gtg922r/obsidian-entities
    path: ~/obsidian-entities
  - repo: gtg922r/Minerva
    path: ~/Minerva
    private: true
  - repo: gtg922r/iris-sync
    path: ~/iris-sync
    private: true
  - repo: gtg922r/hermes
    path: ~/hermes
    private: true
  - repo: gtg922r/hestia-timer
    path: ~/hestia-timer
    private: true

shell_extras:
  aliases: {}
  env: {}
---

# Pyronic

> Primary development VM on exe.dev. General-purpose coding, experimentation, and project hosting.

## Purpose

Main workhorse VM. Hosts multiple active projects including Obsidian plugins, web apps, sync tools, and LLM experiments. This is the daily driver.

## Active Projects

See `~/PYRONIC_APPS.md` for full descriptions of all hosted projects.

Key areas:
- **Obsidian plugins** — numerals, sankey, entities
- **Web apps** — helping-hands, llm-council, Minerva
- **Infrastructure** — iris-sync, hermes, hestia-timer
- **AI tooling** — ai-workflow

## Notes

- Created 2025-01-13
- GitHub CLI authenticated as gtg922r
- Shelley (exe.dev coding agent) is the primary LLM agent
- Banner uses lavender → purple gradient
- See `~/VMCONFIG.md` for historical config changelog
