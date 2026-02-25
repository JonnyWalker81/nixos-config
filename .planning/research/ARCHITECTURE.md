# Architecture Research

**Domain:** Org-mode Life Management System in Doom Emacs
**Researched:** 2026-02-24
**Confidence:** HIGH

## System Overview

```
+===========================================================================+
|                         Doom Emacs Config Layer                            |
|  ~/.doom.d/                                                               |
+---------------------------------------------------------------------------+
|                                                                           |
|  config.el (bootstrap) -----> load! config-org-*.el modules               |
|                                                                           |
|  +----------------+  +----------------+  +----------------+               |
|  | config-org.el  |  | config-org-    |  | config-org-    |               |
|  | (core org +    |  | gtd.el         |  | roam.el        |               |
|  |  babel/export) |  | (GTD workflow) |  | (knowledge)    |               |
|  +-------+--------+  +-------+--------+  +-------+--------+               |
|          |                    |                    |                       |
|  +----------------+  +----------------+  +----------------+               |
|  | config-org-    |  | config-org-    |  | (future files) |               |
|  | journal.el     |  | visual.el      |  |                |               |
|  | (journaling)   |  | (appearance)   |  |                |               |
|  +-------+--------+  +-------+--------+  +----------------+               |
|          |                    |                    |                       |
+----------+--------------------+--------------------+-----------------------+
           |                    |                    |
           v                    v                    v
+===========================================================================+
|                          ~/org/ Data Layer                                 |
+---------------------------------------------------------------------------+
|                                                                           |
|  +-- gtd/              +-- roam/              +-- journal/                |
|  |   inbox.org          |   (per-note .org)    |   (YYYY-MM-DD.org)       |
|  |   projects.org       |   daily/             |                          |
|  |   someday.org        |                      +-- archive/               |
|  |   tickler.org        +-- denote/            |   (YYYY-*.org)           |
|  |   reference.org      |   (timestamped       |                          |
|  |                      |    notes)             |                          |
|  +-- templates/         |                      |                          |
|      (capture tpl)      +-- org-roam.db        |                          |
|                                                                           |
+---------------------------------------------------------------------------+
```

### Component Responsibilities

| Component | Responsibility | Typical Implementation |
|-----------|----------------|------------------------|
| `config-org.el` (existing) | Core org-mode: babel, export, presentations, verb HTTP. Keep as-is. | `after! org` block with babel languages, ox-presenterm, ox-reveal |
| `config-org-gtd.el` (new) | GTD workflow: TODO states, capture templates, agenda views, refile targets, org-super-agenda | `after! org` + `use-package! org-super-agenda` |
| `config-org-roam.el` (new) | Knowledge base: org-roam setup, templates, UI, org-roam-ui, denote integration | `use-package! org-roam` + `use-package! org-roam-ui` |
| `config-org-journal.el` (new) | Daily journaling: org-journal config, carry-over, templates | `use-package! org-journal` |
| `config-org-visual.el` (new) | Visual polish: org-modern, org-appear, org-superstar/bullets, prettification | `use-package! org-modern` + face customization |
| `packages.el` (existing) | Package declarations for all org-related packages | `(package! ...)` declarations |

**Rationale for splitting:** The existing `config-org.el` is 104 lines doing babel/export. Adding GTD (capture, agenda, refile), org-roam (setup, templates, UI), journal, and visual config would balloon it to 500+ lines. The repository already establishes a clear pattern of `config-*.el` per-concern (18 files currently). Following this pattern is non-negotiable.

## Recommended Directory Structure

```
~/org/                          # org-directory root
+-- gtd/                        # GTD system files (big files, few of them)
|   +-- inbox.org               # Capture destination - unsorted items
|   +-- projects.org            # Active projects with next actions
|   +-- someday.org             # Someday/maybe items
|   +-- tickler.org             # Date-triggered reminders
|   +-- reference.org           # Reference materials (non-actionable)
|   +-- work.org                # Existing file, migrate here
|
+-- roam/                       # Org-roam knowledge base (one file per note)
|   +-- 20260224131856-topic.org    # Individual notes (ID-based)
|   +-- daily/                      # org-roam-dailies (fleeting notes)
|   |   +-- 2026-02-24.org
|   +-- .org-roam.db                # SQLite cache (auto-generated)
|
+-- journal/                    # org-journal entries
|   +-- 2026-02-24.org          # Daily journal files
|   +-- ...
|
+-- denote/                     # Denote notes (timestamp-based naming)
|   +-- 20260224T131856--title__keyword1_keyword2.org
|
+-- archive/                    # Archived items from GTD
|   +-- projects-archive.org    # org-archive target
|
+-- templates/                  # Org file templates (optional)
|   +-- project-template.org
```

### Structure Rationale

- **`~/org/` as root:** Standard `org-directory`. All tools reference this. Set once, everything derives from it. Verified: org-roam manual says set `org-roam-directory`; org-journal uses `org-journal-dir`; denote uses `denote-directory`. Each can point to a subdirectory of `~/org/`.
- **`gtd/` subdirectory:** GTD files are "big files" (few files, many headings). Isolating them prevents org-roam from indexing GTD headings as knowledge nodes. Critical separation.
- **`roam/` subdirectory:** Org-roam expects its own directory (`org-roam-directory`) and scans it recursively. Keeping it separate from GTD avoids polluting the knowledge graph with task items. The `daily/` subfolder is org-roam-dailies' default.
- **`journal/` subdirectory:** org-journal uses `org-journal-dir`. Separate from roam/daily because they serve different purposes: journal is reflection/diary, dailies are fleeting capture notes.
- **`denote/` subdirectory:** Denote uses `denote-directory`. It stores notes with its own file-naming scheme (`TIMESTAMP--title__keywords.org`). Keeping it separate prevents collisions with org-roam's naming and ID-based system.
- **`archive/` subdirectory:** Standard org-archive practice. Keeps completed projects out of active GTD files while maintaining history.

### Why This Structure Avoids Conflicts

| Concern | Resolution |
|---------|------------|
| Org-roam indexing GTD files | `org-roam-directory` set to `~/org/roam/`, not `~/org/`. GTD files are outside the roam scan path. |
| Org-roam vs Denote overlap | Different directories (`roam/` vs `denote/`). Different file naming conventions. Choose one as primary knowledge base; the other supplements. **Recommendation: Use org-roam as primary.** Denote is already declared in packages.el; can serve as a lightweight alternative for specific use cases. |
| Journal vs Dailies | `journal/` for reflective daily journaling (org-journal). `roam/daily/` for fleeting capture notes (org-roam-dailies). Different tools, different purpose, different directories. |
| Agenda finding files | `org-agenda-files` should point to `"~/org/gtd/"` directory only. This keeps agenda fast and prevents roam/journal notes from cluttering agenda views. |

## Architectural Patterns

### Pattern 1: Deferred Loading via `after!` and `use-package!`

**What:** All org-mode configuration wrapped in `after! org` blocks or `use-package!` with `:after org` to ensure org loads before configuration runs.
**When to use:** Always. Doom Emacs lazy-loads org-mode; configuring it before load causes errors.
**Trade-offs:** Configuration doesn't take effect until org-mode is first opened. This is fine -- it's the Doom way.

**Example:**
```elisp
;; config-org-gtd.el
(after! org
  (setq org-directory "~/org/")
  (setq org-agenda-files '("~/org/gtd/"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
  ;; ... capture templates, refile targets, etc.
  )

(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups '(...)))
```

### Pattern 2: Centralized Variable Setting, Distributed Config

**What:** Core variables (`org-directory`, `org-agenda-files`) are set in the GTD config (loaded first). Other modules reference these variables rather than hardcoding paths.
**When to use:** When multiple config modules need the same paths.
**Trade-offs:** Creates a load-order dependency. GTD config must be loaded before roam/journal.

**Example:**
```elisp
;; config-org-gtd.el (loaded first)
(after! org
  (setq org-directory "~/org/"))

;; config-org-roam.el (loaded after)
(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (expand-file-name "roam" org-directory)))

;; config-org-journal.el (loaded after)
(use-package! org-journal
  :after org
  :config
  (setq org-journal-dir (expand-file-name "journal/" org-directory)))
```

### Pattern 3: Hybrid File Organization (Big Files + Per-Note Files)

**What:** GTD uses a "few big files" approach (projects.org contains many headings). Knowledge base uses "many small files" approach (one file per concept). This is the standard pattern in the Org ecosystem.
**When to use:** Always for this type of system. GTD benefits from refile/agenda operating on structured files. Knowledge base benefits from atomic notes for linking.
**Trade-offs:** Two mental models for file organization. Users need to know when to create a GTD entry vs. a knowledge note. Capture templates help bridge this.

## Data Flow

### GTD Capture-Process-Organize Cycle

```
[Idea/Task arrives]
        |
        v
[org-capture] --> inbox.org       # Capture: quick entry, minimal friction
        |
        v
[Manual Refile]                    # Process: review inbox, decide destination
   |         |         |
   v         v         v
projects.org  someday.org  tickler.org   reference.org   # Organize: file in GTD bucket
        |
        v
[org-agenda views]                 # Review: daily/weekly review via agenda
   |
   v
[Complete / Archive]               # Close: mark DONE, archive
   |
   v
archive/projects-archive.org
```

### Knowledge Capture Flow

```
[Idea/Concept/Learning]
        |
        +--------> [org-roam-capture]     # Create new knowledge note
        |               |
        |               v
        |          roam/TIMESTAMP-slug.org
        |               |
        |               v
        |          [org-roam-node-insert]  # Link to other notes
        |               |
        |               v
        |          [org-roam-buffer]       # View backlinks
        |
        +--------> [org-roam-dailies]     # Fleeting daily notes
                        |
                        v
                   roam/daily/YYYY-MM-DD.org
                        |
                        v
                   [Process into permanent notes]  # Manual migration
```

### Journal Reflection Flow

```
[End of day / Morning]
        |
        v
[org-journal-new-entry]
        |
        v
journal/YYYY-MM-DD.org
        |
        +------> Link to GTD projects     # "Worked on [[id:...][Project X]]"
        |
        +------> Link to roam notes       # "Learned about [[roam:Topic Y]]"
        |
        v
[Carry-over of TODOs]                     # org-journal-carryover-items
```

### Key Data Flows Between Components

1. **Capture --> GTD inbox:** `org-capture` templates write to `~/org/gtd/inbox.org`. This is the primary entry point for actionable items.
2. **Capture --> Roam:** `org-roam-capture` templates create new files in `~/org/roam/`. This is for knowledge/reference, not tasks.
3. **GTD tasks --> Roam notes:** Project headings in `projects.org` can use `[[id:...]]` links to reference org-roam knowledge notes. Backlinks in roam buffer will show these connections.
4. **Roam notes --> GTD tasks:** A roam note can contain a `TODO` heading, but agenda won't see it (roam dir not in `org-agenda-files`). Instead, create a linked task in GTD and reference the roam note. This is intentional separation.
5. **Journal --> Both:** Journal entries reference GTD items and roam notes via org links. Journal is the reflective layer, not the action layer.
6. **Agenda --> Dashboard:** `org-super-agenda` groups combine scheduled items, deadlines, and context-tagged next actions into a single coherent daily view.

## Integration Points

### GTD <-> Org-Roam: Project-Note Linking

**Pattern:** Add org-roam IDs to GTD project headings. This enables backlink discovery.

```org
;; In ~/org/gtd/projects.org:
* Project: Build Home Lab :@home:project:
  :PROPERTIES:
  :ID:       a1b2c3d4-...
  :CATEGORY: homelab
  :END:

** NEXT Research NAS options
** TODO Order parts
```

```org
;; In ~/org/roam/20260224-nas-research.org:
#+title: NAS Research
#+filetags: :homelab:hardware:

Notes about NAS options...

Related project: [[id:a1b2c3d4-...][Build Home Lab]]
```

The org-roam buffer for the project heading will show backlinks from all related knowledge notes. This is the key integration point.

### Journal <-> Agenda: Daily Review

**Pattern:** Configure org-journal to include today's agenda in new entries.

```elisp
(setq org-journal-after-header-create-hook
      '(lambda ()
         (org-journal-insert-agenda-time-range)))
```

Or use a capture template that includes `%a` (annotation) to link back to the agenda item being worked on.

### Capture Templates: The Routing Layer

Capture templates are the primary user-facing routing mechanism. They determine where data goes:

| Template Key | Description | Target File | Purpose |
|-------------|-------------|-------------|---------|
| `t` | Quick TODO | `gtd/inbox.org` | Actionable task |
| `n` | Note | org-roam capture | Knowledge/reference |
| `j` | Journal | org-journal | Reflection |
| `w` | Work TODO | `gtd/inbox.org` | Work-specific task |
| `p` | Project | `gtd/projects.org` | New project |
| `s` | Someday | `gtd/someday.org` | Future maybe |

### Internal Boundaries (Config Files)

| Boundary | Communication | Notes |
|----------|---------------|-------|
| `config-org.el` <-> `config-org-gtd.el` | Shared `org-directory` variable | GTD sets it, others read it |
| `config-org-gtd.el` <-> `config-org-roam.el` | `org-id` links between GTD and roam files | Works automatically via org's ID system |
| `config-org-gtd.el` <-> `config-org-journal.el` | Carry-over of TODO items | org-journal can carry over TODOs from GTD if journal is in agenda-files (not recommended) |
| `config-org-visual.el` <-> all others | None (visual is purely cosmetic) | No data dependencies; can be loaded in any order |
| `config-org-roam.el` <-> `config-org-journal.el` | Journal entries can link to roam notes | Via standard org `[[id:...]]` links |

## Build Order and Dependencies

### Dependency Graph

```
packages.el (declares all packages)
     |
     v
config-org.el (existing: core org, babel, export)  <-- KEEP AS-IS
     |
     v
config-org-gtd.el (GTD: sets org-directory, agenda, capture, refile)
     |
     +--------+--------+
     v        v        v
config-org-  config-org-  config-org-
roam.el      journal.el   visual.el
(knowledge)  (journaling)  (appearance)
```

### Recommended Build Order for Phases

1. **Phase 1: GTD Foundation** (`config-org-gtd.el` + `packages.el` updates)
   - Set `org-directory` to `~/org/`
   - Create GTD directory structure (`~/org/gtd/`)
   - Define TODO states (`TODO`, `NEXT`, `WAITING`, `DONE`, `CANCELLED`)
   - Basic capture templates (inbox, work)
   - Basic agenda views (daily, weekly)
   - Refile targets (projects, someday, tickler)
   - org-super-agenda grouping (already in packages.el)
   - **Why first:** Everything else depends on `org-directory` and the GTD structure. Agenda views are immediately useful.

2. **Phase 2: Knowledge Base** (`config-org-roam.el` + `packages.el` updates)
   - Set up `org-roam-directory` as `~/org/roam/`
   - Configure org-roam capture templates
   - Set up org-roam-db-autosync-mode
   - Configure org-roam-buffer display
   - Set up org-roam-dailies for fleeting notes
   - Link org-roam nodes to GTD project headings
   - **Why second:** Org-roam is the most complex component but doesn't depend on journal. Building it after GTD enables immediate cross-linking.

3. **Phase 3: Journaling** (`config-org-journal.el`)
   - Set up `org-journal-dir` as `~/org/journal/`
   - Configure journal file type (daily recommended)
   - Set up carry-over for TODO items
   - Add journal capture template
   - **Why third:** Journal is simpler than roam and benefits from both GTD and roam being available for cross-linking.

4. **Phase 4: Visual Polish** (`config-org-visual.el`)
   - org-modern for modern heading/list styling
   - org-appear for auto-showing emphasis markers
   - Custom faces for TODO states
   - Org-mode UI refinements
   - **Why last:** Purely cosmetic. No functional dependencies. Can be iterated without breaking anything.

5. **Phase 5: Integration & Dashboard** (additions to existing configs)
   - Custom agenda dashboard view combining all sources
   - Weekly review workflow
   - Advanced org-super-agenda configurations
   - Denote integration if desired
   - **Why last:** Requires all components working. This is the "glue" phase.

### Load Order in config.el

The load order in `config.el` matters. Recommended insertion:

```elisp
;; Replace the current single line:
;; (load! "config-org")
;; With:
(load! "config-org")           ;; Existing: babel, export, presentations
(load! "config-org-gtd")       ;; GTD: directory, capture, agenda, refile
(load! "config-org-roam")      ;; Knowledge base: org-roam, dailies
(load! "config-org-journal")   ;; Journaling: org-journal
(load! "config-org-visual")    ;; Visual: org-modern, appearance
```

## Anti-Patterns to Avoid

### Anti-Pattern 1: Putting Roam Directory at ~/org/ Root

**What people do:** Set `org-roam-directory` to `~/org/` and let org-roam index everything.
**Why it's wrong:** Org-roam will create nodes from GTD headings (every TODO with an ID becomes a "note"). This pollutes the knowledge graph with task items. Agenda searches slow down. The roam node-find completion becomes useless with hundreds of task nodes mixed in.
**Do this instead:** Set `org-roam-directory` to `~/org/roam/` (a subdirectory). Keep GTD files outside the roam scan path.

### Anti-Pattern 2: Adding Journal/Roam to org-agenda-files

**What people do:** Add `~/org/roam/` or `~/org/journal/` to `org-agenda-files`.
**Why it's wrong:** Agenda becomes slow as Org scans hundreds of roam files. Journal entries cluttering the agenda with old TODOs. The whole point of GTD is having a clean, focused agenda.
**Do this instead:** Only `~/org/gtd/` in `org-agenda-files`. If you need a journal TODO in the agenda, refile it to inbox.org.

### Anti-Pattern 3: One Mega config-org.el File

**What people do:** Put all org configuration (GTD + roam + journal + visual) in a single file.
**Why it's wrong:** The file grows to 500+ lines. Hard to debug. Hard to disable one component. Doesn't follow the repository's established pattern.
**Do this instead:** Split into `config-org-*.el` files matching the repository's convention. Each file is 80-150 lines and has a single concern.

### Anti-Pattern 4: Using org-roam-dailies as Journal

**What people do:** Use `org-roam-dailies-capture-today` for both fleeting notes AND daily journaling.
**Why it's wrong:** Conflates two distinct activities. Fleeting notes are raw captures to be processed. Journal entries are reflective writing. Mixing them makes the daily file a mess and makes it hard to find either type of content.
**Do this instead:** Use org-roam-dailies for quick fleeting captures. Use org-journal for structured daily reflection. Different tools, different directories, different purposes.

### Anti-Pattern 5: Over-engineering GTD File Structure

**What people do:** Create a separate .org file for every project, every area, every context.
**Why it's wrong:** Refile becomes tedious with too many targets. Agenda views become fragmented. The power of org-mode is having rich hierarchical structure WITHIN files.
**Do this instead:** Start with the canonical 5 GTD files (inbox, projects, someday, tickler, reference). Add per-area files only if projects.org grows beyond ~500 headings. Use tags for contexts (@home, @work, @phone, @computer), not separate files.

## Scaling Considerations

| Scale | Architecture Adjustments |
|-------|--------------------------|
| 0-100 notes | Single roam directory, no subdirectories. GTD files stay small. Everything instant. |
| 100-1000 notes | Consider roam subdirectories by topic. org-roam-db-autosync is fine. GTD files still manageable. |
| 1000+ notes | May need `org-roam-db-update-on-save nil` for performance. Consider archiving old GTD items aggressively. Denote's flat-file approach may be faster for some lookups. |
| Journal > 1 year | Set `org-journal-file-type` to 'monthly or 'weekly to reduce file count. Archive old journal files. |

### Scaling Priorities

1. **First bottleneck:** Agenda speed. Too many files in `org-agenda-files` = slow agenda. Fix: Keep only GTD files in agenda-files.
2. **Second bottleneck:** Org-roam db sync on large knowledge bases. Fix: Disable `org-roam-db-update-on-save`, run manual `org-roam-db-sync` periodically.

## NixOS Deployment Considerations

The existing deployment via home-manager symlinks (`home.file.".doom.d"`) means:

1. **New config files** (`config-org-gtd.el`, etc.) are automatically deployed because `recursive = true` on the `.doom.d` source.
2. **Data directories** (`~/org/`) are NOT managed by Nix. They are user data, not configuration. Do NOT put them in the Nix store.
3. **Package declarations** go in `packages.el` (already in the repo). New packages: `org-roam`, `org-roam-ui`, `org-journal`, `org-modern`, `org-appear`. Note: `denote` and `org-super-agenda` are already declared.
4. **System packages** needed: `sqlite3` (for org-roam's emacsql). This should be in the NixOS system packages or home-manager packages. Verify it's available.
5. **After deployment:** User must run `doom sync` to install new packages, then restart Emacs.

## Sources

- Org-roam User Manual v2.3.1: https://www.orgroam.com/manual.html (HIGH confidence - official docs, verified 2026-02-24)
- org-journal GitHub README: https://github.com/bastibe/org-journal (HIGH confidence - official repo)
- Denote Manual v4.1.0: https://protesilaos.com/emacs/denote (HIGH confidence - official docs by maintainer)
- org-super-agenda GitHub: https://github.com/alphapapa/org-super-agenda (HIGH confidence - official repo)
- Existing codebase analysis: `/home/cipher/nixos-config/users/doom.d/` (HIGH confidence - direct inspection)
- Doom Emacs org module: `init.el` shows `(org +pretty +attach +babel +capture +export +present)` (HIGH confidence - direct inspection)

---
*Architecture research for: Org-mode Life Management System in Doom Emacs*
*Researched: 2026-02-24*
