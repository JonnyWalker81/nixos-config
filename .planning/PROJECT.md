# OrgLife — Comprehensive Org-mode Life Management System

## What This Is

A comprehensive Org-mode configuration for Doom Emacs that serves as a unified life management system. It combines full GTD task/project management, calendar/agenda views, an interconnected knowledge base (org-roam + denote), and daily journaling — all with Obsidian-inspired visual polish, fast capture workflows, and deep cross-linking between all domains. Built as part of an existing NixOS configuration managed via home-manager.

## Core Value

Everything lives in one integrated system — tasks, schedule, notes, and journal are cross-linked and accessible within two keystrokes, so nothing falls through the cracks.

## Requirements

### Validated

- ✓ Doom Emacs installed and configured via home-manager — existing
- ✓ Modular config architecture (config-*.el pattern) — existing
- ✓ org-babel with PlantUML, D2, Mermaid — existing
- ✓ Presentation export (org-reveal, ox-presenterm) — existing
- ✓ Evil-mode org/agenda keybindings — existing
- ✓ org-super-agenda package installed — existing (unconfigured)
- ✓ denote package installed — existing (unconfigured)
- ✓ org-gcal package installed — existing (unconfigured)
- ✓ elfeed RSS reader with org-based feed list — existing

### Active

- [ ] Full GTD workflow (Inbox, Next Actions, Waiting, Someday/Maybe, Contexts, Weekly Review)
- [ ] Multiple agenda views (daily, weekly, custom dashboard)
- [ ] org-roam knowledge base with backlinks
- [ ] org-roam-ui interactive knowledge graph
- [ ] denote integration alongside org-roam
- [ ] org-journal daily journaling
- [ ] Capture templates for every domain (task, note, journal, meeting, project)
- [ ] Global hotkey capture from anywhere in Emacs
- [ ] Visual polish (org-modern/org-superstar, color-coded agenda, inline images, LaTeX preview)
- [ ] Custom dashboard on Emacs startup (today's agenda, pending tasks, upcoming deadlines)
- [ ] SPC-based keybindings for all org workflows (2 keystrokes max)
- [ ] Structured org directory with GTD files + org-roam notes
- [ ] org-super-agenda configuration for grouped/prioritized agenda views

### Out of Scope

- Finance/budgeting tracking — deferred to v2, not core to initial system
- Habit tracking (org-habit) — deferred to v2, add after core GTD is solid
- Reading/media log — deferred to v2
- Contacts/CRM — deferred to v2
- Calendar sync (Google Calendar / iCal) — decide later, org-gcal already installed
- Capture from outside Emacs (org-protocol, browser) — v2, focus on in-Emacs capture first
- Mobile access — out of scope, desktop Emacs only

## Context

### Technical Environment
- NixOS system managed via Nix Flakes
- Doom Emacs configured in `users/doom.d/` directory
- Config deployed via home-manager dotfile symlinks (`~/.doom.d`)
- Custom elisp libraries in `users/elisp/` (symlinked to `~/.elisp`)
- Emacs packages managed through Doom's `packages.el`
- Modular config pattern: `config.el` bootstraps 18+ `config-*.el` files via `load!`
- Two users share the config: `cipher` and `jrothberg`

### Current Org-Mode State
- `init.el` enables org with `+pretty +attach +babel +capture +export +present` flags
- `config-org.el` has 104 lines — mostly babel/presentation config
- One capture template (Work Todo to `work.org`)
- Two basic agenda commands ("w" for work, "a" for all)
- org-super-agenda installed but not configured
- denote installed but not configured
- org-gcal installed but not configured
- No org-directory explicitly set (defaults to `~/org`)
- No org-roam, no org-journal, no GTD structure

### Existing Packages to Leverage
- `org-super-agenda` — already in packages.el, needs configuration
- `denote` — already in packages.el, needs configuration
- `org-gcal` — already in packages.el, defer configuration to v2
- `elfeed` / `elfeed-org` — already working

### Packages to Add
- `org-roam` — knowledge base with backlinks
- `org-roam-ui` — interactive web-based knowledge graph
- `org-journal` — daily journaling
- `org-modern` or `org-superstar` — visual polish
- Potentially: `org-fancy-priorities`, `org-appear`, `org-cliplink`

## Constraints

- **Tech Stack**: Must use Doom Emacs module system and `packages.el` — no straight.el or use-package outside Doom patterns
- **Config Architecture**: Follow existing `config-*.el` modular pattern — org config goes in `config-org.el` (may be split further if large)
- **Deployment**: Changes must be deployable via `sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"`
- **Compatibility**: Must not break existing org-babel, presentation export, or elfeed configurations
- **Package Management**: New Emacs packages declared in `users/doom.d/packages.el`, system packages (sqlite for org-roam) in Nix config
- **Testing Gate**: Automated OrgLife suite `tests/run-orglife-tests.sh` is mandatory for phase completion
- **Test Maintenance**: Behavioral code changes must include corresponding test additions/updates

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| org-roam for knowledge base | Provides Obsidian-like backlinks, graph visualization, per-note files | — Pending |
| Keep denote alongside org-roam | User values denote's simplicity; can coexist for different note types | — Pending |
| org-journal for daily journaling | Dedicated journal package with per-day/week/month files, better than datetree | — Pending |
| Hybrid file organization | GTD in structured files (inbox.org, projects.org, someday.org), knowledge in org-roam per-note files | — Pending |
| Full GTD methodology | User wants complete GTD: inbox, next actions, waiting, someday/maybe, contexts, weekly review | — Pending |
| org-roam-ui for knowledge graph | Interactive web-based graph visualization — the Obsidian graph experience | — Pending |

---
*Last updated: 2025-02-24 after initialization*
