# Roadmap: OrgLife

## Overview

OrgLife transforms the user's existing Doom Emacs setup into a comprehensive life management system by building incrementally: GTD task foundation first (everything depends on `org-directory` and file structure), then capture workflows, agenda views (basic then advanced), org-roam knowledge base, journaling + denote, visual polish, and finally cross-system integration. Each phase delivers a complete, verifiable capability and includes a `doom sync` + rebuild validation gate before proceeding. The 8-phase structure follows the dependency chain revealed by research — GTD primitives must exist before agenda views, org-roam requires sqlite verification, and visual polish can only be evaluated with real data in the system.

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [x] **Phase 1: GTD Foundation** - Establish org-directory, TODO states, file structure, refile, priorities, and effort properties
- [x] **Phase 2: Capture Workflow** - Fast capture templates for tasks, notes, projects, and meetings
- [ ] **Phase 3: Basic Agenda** - Daily and weekly agenda views with org-super-agenda grouping
- [x] **Phase 4: Advanced Agenda & GTD Reviews** - Daily review dashboard, weekly review, context filters, stuck project detection
- [x] **Phase 5: Knowledge Base** - org-roam with backlinks, capture templates, and graph visualization
- [x] **Phase 6: Journaling & Denote** - org-journal daily entries with carry-over, plus denote structured notes
- [x] **Phase 7: Visual Polish** - org-modern styling, color-coded TODO states, emphasis hiding, inline previews
- [x] **Phase 8: Integration & Dashboard** - Cross-linking between all systems, SPC keybindings, startup dashboard

## Phase Details

### Phase 1: GTD Foundation
**Goal**: User has a working GTD file structure with TODO states, priorities, effort tracking, and refile — the primitives every later phase depends on
**Depends on**: Nothing (first phase)
**Requirements**: GTD-01, GTD-02, GTD-03, GTD-04, GTD-05, GTD-06, GTD-07
**Success Criteria** (what must be TRUE):
  1. User can cycle through 6 TODO states (TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELLED) with single-key shortcuts, and state changes log timestamps in a LOGBOOK drawer
  2. `~/org/gtd/` directory exists with inbox.org, projects.org, someday.org, reference.org, and archive/ — and `org-directory` points there
  3. User can tag tasks with GTD contexts (@home, @work, @errands, @phone, @computer, @email) via fast-tag selection
  4. User can refile items from inbox.org to any GTD file with fuzzy completion (max 2 levels deep)
  5. User can assign priorities (A/B/C) with visually distinct color-coded faces and set effort/energy properties on tasks
**Plans**: 3 plans in 1 wave (sequential)

Plans:
- [x] 01-01: Infrastructure & File Scaffolding — init.el flags (+roam, +journal), config.el loader registration, config-org-gtd.el skeleton (org-directory, org-agenda-files), GTD directory auto-creation
- [x] 01-02: TODO States, Logging, Tags, Priorities & Effort — 6 TODO states with fast-keys, LOGBOOK logging, 6 GTD context tags, priority faces, t-shirt effort sizes, keyword faces
- [x] 01-03: Refile, Archive & Validation Gate — refile targets scoped to GTD files with fuzzy completion, per-source archive strategy, auto-archive function, end-to-end validation of all Phase 1 success criteria

### Phase 2: Capture Workflow
**Goal**: User can capture tasks, notes, projects, and meetings from anywhere in Emacs with minimal friction
**Depends on**: Phase 1
**Requirements**: CAP-01, CAP-02, CAP-03, CAP-04
**Success Criteria** (what must be TRUE):
  1. User can invoke capture from anywhere in Emacs via global hotkey and see a menu of 4+ template options
  2. User can capture a quick TODO or note/idea to inbox.org with a single-key template selection
  3. User can capture a new project with sub-task structure to projects.org
  4. User can capture meeting notes with attendees, date, notes, and action items
**Plans**: 2 plans in 2 waves (sequential)

Plans:
- [x] 02-01-PLAN.md — Canonical inbox task/idea templates + global DWIM capture entrypoints and legacy template cleanup
- [x] 02-02-PLAN.md — Project/meeting templates and end-to-end validation gate against Doom defaults

### Phase 3: Basic Agenda
**Goal**: User can view organized daily and weekly agenda with items grouped by priority, context, and TODO state
**Depends on**: Phase 1
**Requirements**: AGN-01, AGN-02, AGN-03
**Success Criteria** (what must be TRUE):
  1. User can view a daily agenda with time grid showing today's scheduled items and deadlines
  2. User can view a weekly overview showing the full week ahead
  3. Agenda items are grouped into meaningful sections by org-super-agenda (priority A items first, then by context/TODO state)
**Plans**: 2 plans in 2 waves (sequential)

Plans:
- [ ] 03-01-PLAN.md — Agenda module scaffold, command ownership migration, and baseline daily/weekly block commands
- [ ] 03-02-PLAN.md — org-super-agenda priority/context grouping, weekly deadline summary wiring, and validation gate

### Phase 4: Advanced Agenda & GTD Reviews
**Goal**: User can conduct daily and weekly GTD reviews with purpose-built agenda views, and filter tasks by context
**Depends on**: Phase 3
**Requirements**: AGN-04, AGN-05, AGN-06, AGN-07
**Success Criteria** (what must be TRUE):
  1. User has a "Daily Review" block agenda showing today's schedule + high-priority items + NEXT actions + WAITING items + inbox count
  2. User has a "Weekly Review" block agenda showing the week + stuck projects + all WAITING items + someday/maybe + unprocessed inbox
  3. User can view context-filtered agendas (@home only, @work only) with a single keybinding
  4. User can identify stuck projects (projects with no NEXT action defined) via the weekly review or a dedicated view
**Plans**: 2 plans in 2 waves (sequential)

Plans:
- [x] 04-01-PLAN.md — Daily Review command plus @home/@work context-filtered review views
- [x] 04-02-PLAN.md — Weekly Review command, stuck project detection, and phase validation gate

### Phase 5: Knowledge Base
**Goal**: User has an org-roam knowledge base with backlinks, fuzzy search, typed capture templates, and interactive graph visualization
**Depends on**: Phase 1 (for org-directory structure and cross-linking)
**Requirements**: KB-01, KB-02, KB-03, KB-04, KB-05, KB-06
**Success Criteria** (what must be TRUE):
  1. org-roam is configured with `~/org/roam/` directory (separate from GTD), sqlite DB syncs automatically, and `(sqlite-available-p)` returns t
  2. User can find/open existing roam notes via fuzzy search and insert links to roam notes while typing in any org buffer
  3. User can view a backlinks buffer showing all notes that link to the current note
  4. User has 3 org-roam capture templates (default note, literature note, concept note) accessible via roam capture
  5. org-roam-ui serves an interactive knowledge graph in the browser, launchable from Emacs
**Plans**: 4 plans (3 implementation + 1 gap-closure)

Plans:
- [x] 05-01-PLAN.md — org-roam core setup in config-org-roam.el (directory with file-truename workaround, db-autosync, find-file-visit-truename)
- [x] 05-02-PLAN.md — org-roam capture templates (default, literature, concept) and backlinks buffer configuration
- [x] 05-03-PLAN.md — org-roam-ui setup and validation gate (verify search, backlinks, graph all work)
- [x] 05-04-PLAN.md — gap closure for runtime-only interactive verification (graph navigation, fuzzy open, backlinks UX)

### Phase 6: Journaling & Denote
**Goal**: User can write daily journal entries with TODO carry-over and agenda integration, capture journal entries via org-capture, and use denote for structured file-naming notes
**Depends on**: Phase 1 (for org-directory), Phase 2 (for capture framework)
**Requirements**: JRN-01, JRN-02, JRN-03, JRN-04, CAP-05, KB-07
**Success Criteria** (what must be TRUE):
  1. org-journal creates per-day files in `~/org/journal/` with timestamps, and user can open today's journal with a keybinding
  2. Unfinished journal TODOs automatically carry over to the next day's entry
  3. Journal TODOs appear in org-agenda alongside GTD items, and user can search across all journal entries
  4. User can capture a journal entry via org-capture (template integrated with org-journal)
  5. denote is configured in `~/org/denote/` with its own directory, separate from org-roam, using structured file-naming convention
**Plans**: 2 plans in 2 waves (sequential)

Plans:
- [x] 06-01-PLAN.md — org-journal module bootstrap with daily entry scaffold, open-today keypath, and yesterday carry-over migration behavior
- [x] 06-02-PLAN.md — journal agenda/search/capture integration plus dedicated strict-taxonomy denote module wiring

### Phase 7: Visual Polish
**Goal**: Org buffers and agenda have modern, clean visual styling with color-coded states, hidden markup, and inline previews
**Depends on**: Phase 3 (agenda must exist to style it)
**Requirements**: VIS-01, VIS-02, VIS-03, VIS-04, VIS-05
**Success Criteria** (what must be TRUE):
  1. org-modern-mode is active in org buffers — headlines, keywords, tables, and timestamps use modern styling (not ASCII decorators)
  2. org-modern styling is applied to agenda buffers with clean grouping visuals
  3. TODO keywords are color-coded by state (NEXT=blue, WAITING=orange, SOMEDAY=grey, TODO=red, DONE=green, CANCELLED=dim)
  4. Emphasis markers (*bold*, /italic/, etc.) are hidden — only the styled text is visible
  5. Inline images display in org buffers and LaTeX fragments render as preview images
**Plans**: 2 plans in 2 waves (sequential)

Plans:
- [x] 07-01-PLAN.md — Visual module ownership + org-modern baseline for Org and agenda
- [x] 07-02-PLAN.md — Locked TODO semantics, org-appear reveal, inline image/LaTeX previews, and phase verification gate

### Phase 8: Integration & Dashboard
**Goal**: All OrgLife systems are cross-linked, accessible via consistent SPC keybindings, and a startup dashboard shows today's agenda at a glance
**Depends on**: Phase 1-7 (all systems must exist to integrate them)
**Requirements**: UX-01, UX-02, UX-03, UX-04
**Success Criteria** (what must be TRUE):
  1. User can link GTD tasks to org-roam knowledge notes using org-id (bidirectional: task references note, note backlinks to task)
  2. User can link journal entries to tasks and projects (journal entry references a project, visible in backlinks)
  3. All OrgLife workflows are accessible via SPC-based keybindings within 2 keystrokes (SPC o for org-life prefix, then single key for action)
  4. Emacs startup shows a custom dashboard with today's agenda items, pending inbox count, upcoming deadlines, and quick-access links to common actions
**Plans**: 5 plans (3 implementation + 2 gap-closure)

Plans:
- [x] 08-01-PLAN.md — Cross-link primitives and hybrid capture/manual linking for GTD↔roam and journal↔heading workflows
- [x] 08-02-PLAN.md — Canonical SPC o org-life keymap with mnemonic grouping and legacy alias preservation
- [x] 08-03-PLAN.md — Doom dashboard widgets, startup/refresh lifecycle, quick actions, and runtime UX verification gate
- [x] 08-04-PLAN.md — Gap closure: bidirectional backlink persistence/retrieval for GTD↔roam and journal↔heading links
- [x] 08-05-PLAN.md — Gap closure: strict two-keystroke SPC o contract + dashboard inbox pending count

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. GTD Foundation | 3/3 | Verified ✓ | 2026-02-25 |
| 2. Capture Workflow | 2/2 | Verified ✓ | 2026-02-26 |
| 3. Basic Agenda | 0/2 | Not started | - |
| 4. Advanced Agenda & GTD Reviews | 2/2 | Verified ✓ | 2026-02-26 |
| 5. Knowledge Base | 4/4 | Verified ✓ | 2026-02-28 |
| 6. Journaling & Denote | 2/2 | Verified ✓ | 2026-03-03 |
| 7. Visual Polish | 2/2 | Verified ✓ | 2026-03-08 |
| 8. Integration & Dashboard | 5/5 | Verified ✓ | 2026-03-08 |

---
*Roadmap created: 2026-02-24*
*Last updated: 2026-03-08 — Phase 8 verified complete (Integration & Dashboard: 5 of 5 plans done)*
