# Project Research Summary

**Project:** OrgLife — Comprehensive Org-mode Life Management System
**Domain:** Personal knowledge management + GTD task management in Doom Emacs on NixOS
**Researched:** 2026-02-24
**Confidence:** HIGH

## Executive Summary

OrgLife is a comprehensive Org-mode life management system built on top of the user's existing Doom Emacs configuration running on NixOS. The research reveals a critical insight: **installation is trivial; configuration is the real work.** The Doom Emacs ecosystem already bundles most required packages via module flags (`+roam`, `+journal`, `+pretty`), and key packages like `org-super-agenda` and `denote` are already declared in the user's `packages.el`. The total installation delta is two init.el flags and two packages.el lines. The remaining 95% of effort is configuration, directory structure, and workflow establishment across 4-5 new `config-org-*.el` files following the repository's established pattern.

The recommended approach is a strict incremental build: GTD foundation first (capture → process → review cycle), then org-roam knowledge base, then journaling, then visual polish and integration. This ordering is dictated by data dependencies (everything depends on `org-directory` and GTD file structure) and by the critical pitfall that building all systems simultaneously makes debugging impossible — Doom's lazy-loading means errors surface late and cascade unpredictably. Each phase must have explicit validation gates before proceeding.

The primary risks are: (1) Doom silently overwriting capture templates and TODO states with its own defaults (mitigated by using `setq` in `after! org` blocks and verifying with `M-x org-capture`), (2) org-roam symlink resolution failures on NixOS due to home-manager's Nix store symlinks (mitigated by `file-truename` on all directory paths), and (3) org-roam/org-agenda directory overlap causing either slow agendas or invisible TODO items (mitigated by strict directory separation: `~/org/gtd/` for agenda, `~/org/roam/` for knowledge). All three risks have well-documented mitigations and are addressed in the earliest relevant phase.

## Key Findings

### Recommended Stack

The stack is almost entirely Doom Emacs module flags and existing packages. No new frameworks, no external services, no complex infrastructure. The Emacs ecosystem is mature and the packages are well-maintained (org-modern by Daniel Mendler, org-roam at v2.3.1, org-super-agenda at v1.3). The only system-level dependency is `sqlite` for org-roam's database, which should be available via NixOS's Emacs 29+ built-in sqlite3 but needs runtime verification.

**Core technologies:**
- **Doom `+roam` flag**: Installs org-roam v2.3.1 — Zettelkasten knowledge base with backlinks and sqlite graph
- **Doom `+journal` flag**: Installs org-journal — per-day journal files with carry-over and calendar integration
- **Doom `+pretty` flag** (already active): Provides org-modern + org-appear — modern visual styling
- **org-super-agenda** (already installed): Groups agenda items into semantic sections — the backbone of useful agenda views
- **org-roam-ui** (new, via packages.el): Interactive web-based knowledge graph visualization
- **denote** (already installed): Lightweight file-naming-convention notes alongside org-roam
- **sqlite** (NixOS system package): Required by org-roam's emacsql-sqlite — **verify with `(sqlite-available-p)`**

**Total changes: 2 flags in init.el, 2 lines in packages.el.** Everything else is configuration in new `config-org-*.el` files.

### Expected Features

**Must have (table stakes — system is broken without these):**
- GTD TODO states: `TODO → NEXT → WAITING → SOMEDAY | DONE → CANCELLED` (extend Doom's existing states, don't replace)
- GTD file structure: `~/org/gtd/` with inbox.org, projects.org, someday.org, tickler.org, reference.org
- 4-5 capture templates: inbox task, note, meeting, project, journal entry
- org-agenda-files pointing to `~/org/gtd/` only (not roam, not journal)
- org-super-agenda grouping: time-grid, next actions, waiting, priorities
- Refile targets: limited to GTD files at maxlevel 2, not roam
- org-modern visual polish (already active, just needs configuration)
- Color-coded TODO states and priority faces

**Should have (differentiators — what makes this great):**
- org-roam knowledge base with capture templates (default, literature, concept)
- org-roam-ui graph visualization
- org-journal with daily files, carry-over, and agenda integration
- Custom agenda views: daily review dashboard, weekly review, context-filtered views
- Cross-linking between GTD projects and roam knowledge notes via org-id
- denote for structured meeting notes / project docs

**Defer (v2+):**
- org-habit tracking (adds agenda noise, separate workflow to learn)
- org-gcal calendar sync (fragile OAuth, complex debugging)
- org-protocol browser capture
- org-clock time tracking (overhead kills flow)
- Mobile access

### Architecture Approach

The architecture follows Doom's established `config-*.el` per-concern pattern with a clear data layer separation. Configuration splits into 4-5 new files (`config-org-gtd.el`, `config-org-roam.el`, `config-org-journal.el`, `config-org-visual.el`) loaded sequentially from `config.el`. The data layer uses a hybrid model: "few big files" for GTD (one projects.org with many headings) and "many small files" for knowledge (one org-roam note per concept). Strict directory separation prevents system collision.

**Major components:**
1. **config-org-gtd.el** — Sets `org-directory`, defines TODO states, capture templates, agenda views, refile targets, org-super-agenda groups. Foundation that everything else depends on.
2. **config-org-roam.el** — org-roam directory, db-autosync, capture templates, org-roam-ui, backlinks buffer. Independent of GTD but benefits from cross-linking.
3. **config-org-journal.el** — org-journal directory, daily file type, carry-over, agenda integration. Simplest component, independent.
4. **config-org-visual.el** — org-modern config, org-appear, TODO/priority faces, UI refinements. Purely cosmetic, no data dependencies.
5. **config-org.el** (existing) — Keep as-is for babel, export, presentations. Do not modify.

**Data directory structure:**
```
~/org/
  gtd/           → GTD files (agenda scans ONLY this)
  roam/          → org-roam knowledge base (separate from agenda)
  journal/       → org-journal daily entries
  denote/        → denote notes (separate from roam)
  archive/       → archived GTD items
```

### Critical Pitfalls

1. **Doom overwrites capture templates** — Doom's `+org-init-capture-defaults-h` replaces `org-capture-templates` entirely. Use `setq` in `(after! org ...)` which runs after Doom's hooks. Verify with `M-x org-capture` showing YOUR templates, not Doom defaults. *Phase 1 blocker.*

2. **Doom overwrites TODO states** — Doom sets its own `org-todo-keywords` (TODO/PROJ/STRT/WAIT/HOLD/IDEA/DONE/KILL). Don't replace — extend by adding `NEXT` to Doom's existing sequence and providing matching faces. *Phase 1 blocker.*

3. **org-roam symlink resolution on NixOS** — org-roam doesn't resolve symlinks. home-manager deploys config via `/nix/store/` symlinks. Always use `(file-truename ...)` on `org-roam-directory` and set `find-file-visit-truename t`. Keep `~/org/` as a real directory (not Nix-managed). *Phase 2 blocker.*

4. **Building everything at once** — Interconnected systems with Doom's lazy-loading make cascading failures untraceable. Build in strict phases with validation gates. Each phase: configure → `doom sync` → test → confirm → next phase. *Meta-pitfall: the roadmap structure itself is the prevention.*

5. **org-roam/agenda directory overlap** — If org-roam-directory is inside org-agenda-files, agenda scans hundreds of note files (slow) or TODOs in roam notes are invisible to agenda (lost tasks). Keep `~/org/gtd/` in agenda-files, `~/org/roam/` for org-roam. Never overlap. *Phase 1 structure + Phase 2 validation.*

## Implications for Roadmap

Based on research, suggested phase structure:

### Phase 1: GTD Foundation
**Rationale:** Everything depends on `org-directory`, GTD file structure, TODO states, and capture templates. This is the load-bearing foundation — org-roam, journal, and agenda views all reference these primitives. Cannot be parallelized.
**Delivers:** Working GTD capture → process → review cycle. Users can capture tasks, refile from inbox, and review via basic agenda.
**Addresses:** GTD TODO states, file structure, capture templates, basic agenda, org-super-agenda grouping, refile targets, org-modern activation
**Avoids:** Pitfall 1 (capture template override), Pitfall 2 (TODO state conflict), Pitfall 5 (directory overlap), Pitfall 10 (super-agenda before basic agenda), Pitfall 15 (capture habit)
**Includes:** init.el flag changes (`+roam`, `+journal`), packages.el additions, NixOS sqlite verification, `doom sync`, directory creation

### Phase 2: Knowledge Base (org-roam)
**Rationale:** Most complex new component. Requires sqlite, careful path resolution, and database initialization. Building after GTD enables immediate cross-linking between projects and knowledge notes.
**Delivers:** org-roam knowledge base with backlinks, capture templates (default/literature/concept), org-roam-dailies for fleeting notes, org-roam-ui visualization.
**Uses:** org-roam (via `+roam` flag), org-roam-ui (via packages.el), sqlite
**Implements:** `config-org-roam.el` component
**Avoids:** Pitfall 3 (symlink resolution), Pitfall 7 (after!/use-package! timing), Pitfall 9 (denote naming conflicts), Pitfall 11 (emacsql corruption)

### Phase 3: Journaling
**Rationale:** Simpler than org-roam, benefits from both GTD and roam being available for cross-linking. Requires an explicit architectural decision about org-journal vs org-roam-dailies boundary.
**Delivers:** org-journal with daily files, TODO carry-over, agenda integration, journal capture template.
**Implements:** `config-org-journal.el` component
**Avoids:** Pitfall 8 (journal vs dailies conflict)

### Phase 4: Visual Polish & Advanced Agenda
**Rationale:** Purely cosmetic changes (org-modern tuning, faces, ellipsis) can only be meaningfully evaluated after real data exists in the system. Advanced agenda views (daily dashboard, weekly review, context filters) require all prior systems working.
**Delivers:** Polished org-modern appearance, color-coded TODO/priority faces, custom agenda dashboard, weekly review workflow, denote configuration.
**Implements:** `config-org-visual.el` component + additions to `config-org-gtd.el`
**Avoids:** Pitfall 13 (org-roam-ui blank/slow), Pitfall 14 (agenda view overload)

### Phase 5: Integration & Keybindings
**Rationale:** Cross-system integration (GTD↔roam linking, journal↔agenda, custom SPC prefix) requires all components working. This is the "glue" phase.
**Delivers:** Unified SPC keybinding tree, cross-linking workflows, startup dashboard, polished capture-to-knowledge pipeline.
**Addresses:** Cross-linking features, advanced keybindings, denote integration refinement

### Phase Ordering Rationale

- **Dependency chain:** GTD structure → org-roam → journal → visual → integration. Each phase builds on the previous. `org-directory` and `org-agenda-files` must exist before anything else works.
- **Complexity ordering:** Phase 1 is high-value/low-risk (mostly `setq` calls). Phase 2 is highest-complexity (sqlite, symlinks, database). Phase 3 is low-complexity. Phase 4-5 are polish.
- **Pitfall mitigation:** Incremental build with validation gates between phases directly prevents Pitfall 4 (building everything at once). Each phase has clear "done" criteria.
- **Habit formation:** Phase 1 should be used for a few days before Phase 2 to build the capture muscle memory (Pitfall 15). The system only works if the user actually captures into it.

### Research Flags

Phases likely needing deeper research during planning:
- **Phase 1:** Needs research on Doom's exact capture template override mechanism — verify `after! org` timing with `+org-init-capture-defaults-h`. Also verify TODO state interaction with Doom's face definitions.
- **Phase 2:** Needs research on NixOS Emacs 29+ sqlite availability — run `(sqlite-available-p)` before configuring org-roam. If `nil`, need to add sqlite build flag to Emacs derivation.

Phases with standard patterns (skip research-phase):
- **Phase 3:** org-journal is straightforward — well-documented, single config file, no edge cases beyond the dailies boundary decision.
- **Phase 4:** org-modern configuration is well-documented by the author. Face customization is standard Emacs.
- **Phase 5:** Keybinding setup is standard Doom `map!` calls.

## Confidence Assessment

| Area | Confidence | Notes |
|------|------------|-------|
| Stack | HIGH | All packages verified from GitHub repos and Doom module source. Versions confirmed. Only gap: NixOS sqlite runtime verification. |
| Features | HIGH | Feature set derived from org-mode manual, org-roam manual, and org-super-agenda README. Community patterns well-established. |
| Architecture | HIGH | Follows existing repo pattern (18 `config-*.el` files). Directory structure derived from org-roam manual recommendations. |
| Pitfalls | HIGH | Critical pitfalls verified against Doom source code, org-roam manual (symlink warning), and GitHub issues (emacsql, performance). |

**Overall confidence: HIGH**

The Org-mode ecosystem is one of the most mature in Emacs. Doom Emacs's org module is well-documented. The user's existing config already has the foundation (packages.el declarations, init.el flags, config-org.el pattern). This is not greenfield — it's extending a working system with well-understood components.

### Gaps to Address

- **NixOS sqlite availability:** Must verify `(sqlite-available-p)` returns `t` in the user's Emacs. If `nil`, org-roam will fail. Mitigation: add `pkgs.sqlite` to system packages and/or ensure Emacs is built with `--with-sqlite3`. Check early in Phase 1.
- **Doom capture template timing:** The exact hook ordering of `+org-init-capture-defaults-h` vs user's `after! org` needs empirical validation. Mitigation: test `M-x org-capture` immediately after Phase 1 config.
- **Two-user path compatibility:** Config is shared between `cipher` and `jrothberg`. All paths must use `~` not hardcoded usernames. Verify with both users.
- **org-roam-ui stability:** 123 open issues on GitHub, self-described as "alpha" by maintainers. Treat as nice-to-have visualization, not critical workflow component.

## Sources

### Primary (HIGH confidence)
- Doom Emacs org module README: `modules/lang/org/README.org` — module flags, package list, configuration patterns
- Doom Emacs org module source: `modules/lang/org/config.el` — capture defaults, TODO keywords, appearance hooks
- org-roam User Manual v2.3.1: https://www.orgroam.com/manual.html — setup, templating, symlink warning, performance
- org-roam GitHub: https://github.com/org-roam/org-roam — v2.3.1 release, issues #2474, #2547, #2550-2584
- org-super-agenda GitHub: https://github.com/alphapapa/org-super-agenda — group selectors, examples
- org-journal GitHub: https://github.com/bastibe/org-journal — setup, carry-over, agenda integration
- org-modern GitHub: https://github.com/minad/org-modern — configuration, alternatives comparison
- org-appear GitHub: https://github.com/awth13/org-appear — evil-mode integration
- Org-mode manual: https://orgmode.org/manual/ — TODO extensions, capture, agenda, refile

### Secondary (MEDIUM confidence)
- org-roam-ui GitHub: https://github.com/org-roam/org-roam-ui — Doom setup (alpha software, 123 open issues)
- Denote Manual v4.1.0: https://protesilaos.com/emacs/denote — directory isolation, file naming

### Local (HIGH confidence)
- Existing `users/doom.d/init.el` — current module flags
- Existing `users/doom.d/packages.el` — current package declarations
- Existing `users/doom.d/config-org.el` — current org configuration (104 lines, babel/export)
- Existing `users/common/packages.nix` — system packages (sqlite status)

---
*Research completed: 2026-02-24*
*Ready for roadmap: yes*
