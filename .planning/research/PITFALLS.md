# Pitfalls Research

**Domain:** Org-mode Life Management System in Doom Emacs (NixOS)
**Researched:** 2026-02-24
**Confidence:** HIGH (verified against Doom source, org-roam manual, GitHub issues)

## Critical Pitfalls

### Pitfall 1: Doom's `org-capture-templates` Gets Silently Overwritten

**What goes wrong:**
You define custom `org-capture-templates` in `config-org.el`, but Doom's `+org-init-capture-defaults-h` function runs later and **completely replaces** `org-capture-templates` with Doom's defaults (Personal todo, Personal notes, Journal, Project-local templates). Your custom templates vanish. The current `config-org.el` uses `add-to-list` to append a single "Work Todo" template — this works only if it runs *after* Doom's defaults. But switching to `setq org-capture-templates` for a full GTD set will collide with Doom's initialization.

**Why it happens:**
Doom's org module uses `+org-init-capture-defaults-h` (hooked to org-load) which calls `setq org-capture-templates` directly — it doesn't merge, it replaces. The user's `(after! org ...)` block may run before or after this hook depending on load order. The existing `add-to-list` approach only works because it appends to whatever Doom already set.

**How to avoid:**
Use `(after! org (setq org-capture-templates '(...)))` which runs after Doom's hooks since Doom's `+org-init-capture-defaults-h` fires during org-load, and `after!` in config-org.el fires after all module config. Alternatively, use `set-org-capture-templates!` if available in your Doom version. **Test by calling `M-x org-capture` and verifying ALL your templates appear** — don't assume they're there.

**Warning signs:**
- `M-x org-capture` shows only Doom's default templates (t=Personal todo, n=Personal notes, j=Journal, p=Project templates)
- Your custom capture keys don't appear in the selection menu
- Captures go to `todo.org`, `notes.org`, `journal.org` (Doom defaults) instead of your GTD files

**Phase to address:**
Phase 1 (GTD Foundation) — must be the very first thing validated before building any capture workflows.

---

### Pitfall 2: Doom's TODO Keywords Override Your GTD States

**What goes wrong:**
You design a clean GTD workflow with states like `TODO | NEXT | WAITING | SOMEDAY | DONE | CANCELLED` but Doom's org module already sets `org-todo-keywords` with its own three sequences: `TODO/PROJ/LOOP/STRT/WAIT/HOLD/IDEA/DONE/KILL`, checkbox-style `[ ]/[-]/[?]/[X]`, and voting `OKAY/YES/NO`. Your custom states either conflict with Doom's or get overridden entirely.

**Why it happens:**
Doom's `+org-init-appearance-h` function sets `org-todo-keywords` and `org-todo-keyword-faces` directly. It also defines custom faces (`+org-todo-active`, `+org-todo-project`, `+org-todo-onhold`, `+org-todo-cancel`). If you override `org-todo-keywords` but don't provide matching face definitions, you get unstyled TODO states that look wrong in agenda.

**How to avoid:**
Don't fight Doom's states — **extend them or adopt them**. Doom's built-in states already cover most GTD needs: `TODO` = inbox/next action, `PROJ` = project, `STRT` = in progress, `WAIT` = waiting for, `HOLD` = someday/maybe, `IDEA` = someday/maybe (unconfirmed), `DONE` = done, `KILL` = cancelled. The only state Doom lacks for pure GTD is an explicit `NEXT` action marker.

Recommended approach: Add a `NEXT` state to Doom's existing sequence rather than replacing it all:
```elisp
(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"    ; Inbox / needs processing
           "NEXT(n)"    ; Next action (GTD)
           "PROJ(p)"    ; Project
           "STRT(s)"    ; In progress
           "WAIT(w)"    ; Waiting for someone
           "HOLD(h)"    ; Someday/maybe
           "IDEA(i)"    ; Unconfirmed
           "|"
           "DONE(d)"    ; Completed
           "KILL(k)")   ; Cancelled
          (sequence
           "[ ](T)"
           "[-](S)"
           "[?](W)"
           "|"
           "[X](D)")))
  ;; Also define face for NEXT
  (appendq! org-todo-keyword-faces
            '(("NEXT" . +org-todo-active))))
```

**Warning signs:**
- TODO keywords in agenda aren't color-coded
- States show plain text without face styling
- Fast-key selection (`C-c C-t`) doesn't show expected state options
- Over 8 active TODO states (classic over-engineering smell)

**Phase to address:**
Phase 1 (GTD Foundation) — define states before building agenda views that filter on them.

---

### Pitfall 3: org-roam Symlink Resolution Failure on NixOS with home-manager

**What goes wrong:**
org-roam silently fails to find or index notes, or creates duplicate database entries for the same file, because NixOS/home-manager deploys dotfiles via symlinks and org-roam's path resolution doesn't match. The org-roam manual explicitly warns: *"Org-roam does not resolve symbolic links"* and recommends `(setq find-file-visit-truename t)` or using `file-truename` on the directory path.

**Why it happens:**
home-manager creates symlinks like `~/.doom.d → /nix/store/...-home-manager-files/.doom.d`. If `org-roam-directory` points to a symlinked path, or if the org files themselves are accessed through symlinks, org-roam's SQLite database stores the symlink path while Emacs might resolve to the real path (or vice versa). This causes:
- Files indexed but not found when searching
- Duplicate entries (same file, different paths)
- Backlinks not showing up because link targets don't match DB paths

**How to avoid:**
1. Set `org-roam-directory` using `file-truename`: `(setq org-roam-directory (file-truename "~/org/roam/"))`
2. Set `(setq find-file-visit-truename t)` globally — per org-roam manual recommendation
3. Keep org data files (~/org/) as **real directories**, not symlinked through Nix. Only the *config* files should be symlinked; the *data* directory must be a real path.
4. Verify: `M-: (file-truename org-roam-directory)` should match `M-: (expand-file-name org-roam-directory)`

**Warning signs:**
- `org-roam-node-find` returns 0 nodes despite having .org files in the directory
- Backlinks panel is empty for notes you know link to each other
- `org-roam-db-sync` completes but reports 0 files processed
- `org-roam-db-diagnose-node` shows path mismatches

**Phase to address:**
Phase 2 (org-roam setup) — validate immediately after first `org-roam-db-sync`.

---

### Pitfall 4: Building the Entire System at Once Instead of Incrementally

**What goes wrong:**
You configure GTD + org-roam + org-journal + denote + org-super-agenda + org-roam-ui + visual polish + custom dashboard all in one sitting. Something breaks — maybe agenda is blank, maybe org-roam won't sync, maybe capture goes to the wrong file — and you can't isolate which of the 8+ interacting systems caused the failure. You spend hours debugging and eventually abandon the whole setup.

**Why it happens:**
Org-mode's power comes from interconnected systems, but that interconnection means failures cascade. A wrong `org-directory` setting breaks capture, agenda, and refile simultaneously. An `after!` block with a typo can silently prevent everything after it from loading. Doom's lazy-loading means errors might not surface until you first trigger a specific command, making the root cause non-obvious.

**How to avoid:**
Build in strict phases with **validation gates** between each:
1. **Phase 1:** GTD files + TODO states + capture templates only. Validate: can you capture, can you see agenda, can you refile?
2. **Phase 2:** org-roam basics (directory, db-sync, find, insert). Validate: can you create a note, can you see backlinks?
3. **Phase 3:** org-journal. Validate: does it create files where expected, does it show in agenda?
4. **Phase 4:** org-super-agenda grouping. Validate: agenda shows groups.
5. **Phase 5:** Visual polish, dashboard, integrations.

Each phase should have a `doom sync && doom doctor` check and a manual smoke test.

**Warning signs:**
- You've written 200+ lines of org config without testing any of it
- You're configuring org-super-agenda before you have working agenda views
- You're setting up org-roam-ui before org-roam-db-sync works
- Your config-org.el is growing past 150 lines without intermediate tests

**Phase to address:**
All phases — this is a meta-pitfall about project execution. The roadmap structure itself is the prevention.

---

### Pitfall 5: org-roam and org-agenda Files Don't Overlap

**What goes wrong:**
You set `org-roam-directory` to `~/org/roam/` and `org-directory` to `~/org/`. Your GTD files live in `~/org/` (inbox.org, projects.org, etc.) and your org-roam notes live in `~/org/roam/`. The agenda only scans `org-agenda-files` (which defaults to `org-directory`), so TODO items inside org-roam notes **never appear in the agenda**. You create a task while writing a note, and it falls through the cracks — the exact failure mode the system was built to prevent.

**Why it happens:**
org-agenda scans `org-agenda-files` which is typically a list of files or directories. org-roam stores notes in `org-roam-directory`. If these don't overlap, items with TODO states in org-roam notes are invisible to the agenda. Adding the entire org-roam directory to `org-agenda-files` works but causes slow agenda generation as file count grows (org-roam can have hundreds or thousands of files).

**How to avoid:**
Use a hybrid approach:
1. Keep GTD files in `~/org/gtd/` — add this to `org-agenda-files`
2. Set `org-roam-directory` to `~/org/roam/`
3. Set `org-directory` to `~/org/` (parent of both)
4. For TODOs in org-roam notes, use **refile** to move them to GTD files rather than leaving them in notes
5. OR use `org-roam`'s tag system with a custom agenda command that queries roam nodes with TODO states: `(org-agenda-files (append (list "~/org/gtd/") (org-roam-list-files)))`  — but beware performance with 1000+ roam files.

**Warning signs:**
- TODOs you created in roam notes don't appear in your agenda
- Agenda gets progressively slower as your roam directory grows
- You have "orphan" tasks scattered across dozens of note files

**Phase to address:**
Phase 1 (GTD Foundation) for directory structure decisions; Phase 2 (org-roam) for integration validation.

---

## Moderate Pitfalls

### Pitfall 6: Capture Template Bloat — Too Many Templates Cause Decision Paralysis

**What goes wrong:**
You create separate capture templates for: personal todo, work todo, project todo, meeting note, phone call, email follow-up, reading list, idea, org-roam note, org-roam literature note, org-journal entry, fleeting note, reference note, daily review... Suddenly `M-x org-capture` presents 15+ options and you spend more time choosing the template than capturing the thought. The thought escapes.

**Why it happens:**
GTD purists want to categorize at capture time. But the whole point of GTD's inbox is to capture first, process later. Over-categorization at capture time reintroduces the friction that GTD was designed to eliminate.

**How to avoid:**
Start with **maximum 5 capture templates**:
1. `t` — Quick task (goes to inbox.org, process later)
2. `n` — Quick note (goes to inbox.org or notes.org)
3. `j` — Journal entry (org-journal)
4. `r` — org-roam note (creates a new roam node)
5. `m` — Meeting notes (timestamped)

Add more templates **only when you notice a repeated friction pattern** — e.g., you keep manually adding the same metadata to meeting notes, so you create a meeting template. Never pre-create templates for workflows you haven't established yet.

**Warning signs:**
- Your org-capture menu doesn't fit on screen
- You hesitate for more than 2 seconds choosing a template
- You have templates you've never used
- You find yourself hitting "c" for a generic capture because the specific templates are too many to scan

**Phase to address:**
Phase 1 (GTD Foundation) — start minimal; Phase 5+ can add templates as workflows mature.

---

### Pitfall 7: `after!` vs `use-package!` Timing Issues in Doom Emacs

**What goes wrong:**
You configure org-roam with `(after! org-roam ...)` but some settings don't take effect, or you get errors like "Symbol's function definition is void: org-roam-db-autosync-mode". Or you use `(use-package! org-roam ...)` which conflicts with how Doom wants to manage packages.

**Why it happens:**
Doom Emacs has its own loading order and macro system that doesn't always align with standard `use-package` patterns:
- `after!` defers evaluation until after the package loads. But if the package never loads (because it's lazy-loaded and you never trigger it), your config never runs.
- `use-package!` is Doom's wrapper around `use-package` and should be preferred over raw `use-package` calls.
- Doom's org module loads org lazily via `defer-incrementally` which means many org features aren't available at config evaluation time.
- Setting `org-roam-directory` must happen before `org-roam-db-autosync-mode` is called, but if both are in an `after!` block, order depends on hook ordering.

**How to avoid:**
Follow Doom's prescribed patterns:
```elisp
;; For packages declared in packages.el:
(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-db-autosync-mode))

;; For configuring Doom's built-in org:
(after! org
  (setq org-directory "~/org/"
        org-agenda-files (list "~/org/gtd/")))
```
- Use `after! org` for org core settings
- Use `use-package! org-roam` with `:after org` for org-roam (since it depends on org)
- Put variable assignments (setq) in `:config` or `:init` appropriately
- Never use bare `(require 'org-roam)` — it defeats lazy loading

**Warning signs:**
- "Symbol's function definition is void" errors on Emacs startup
- Settings that work when you `M-x eval-buffer` but not on restart
- `doom doctor` shows warnings about package configuration
- org-roam features work only after you manually visit an org file first

**Phase to address:**
Phase 2 (org-roam) and all subsequent phases — every new package integration must follow this pattern.

---

### Pitfall 8: org-journal vs org-roam Dailies Conflict

**What goes wrong:**
You configure both `org-journal` and `org-roam-dailies` and end up with two parallel daily note systems. You capture a journal thought via org-journal, then later look for it in org-roam-dailies and can't find it (or vice versa). Your journal entries aren't linked to your knowledge base. You have files in two places: `~/org/journal/` and `~/org/roam/daily/`.

**Why it happens:**
org-journal and org-roam-dailies solve overlapping problems. Both create per-day files for timestamped entries. org-roam-dailies stores them in `org-roam-directory/daily/` and they become roam nodes (with IDs, backlinks, graph visibility). org-journal stores them separately and they're NOT roam nodes by default.

**How to avoid:**
**Choose one primary daily system and stick to it.** Recommended: Use org-roam-dailies for daily notes/fleeting thoughts (they integrate with the knowledge graph) and org-journal ONLY if you want a separate, private daily reflection that's intentionally disconnected from the knowledge base.

If using both:
- Configure org-journal to store files **outside** org-roam-directory to avoid indexing conflicts
- Decide explicitly: "fleeting work thoughts go to org-roam-dailies, personal reflections go to org-journal"
- Never cross-link between the two systems

Alternatively, configure org-journal to use a format that org-roam can index:
```elisp
(setq org-journal-dir (concat org-roam-directory "journal/")
      org-journal-date-format "%Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")
```
But this creates roam indexing overhead for journal files you may not want in the knowledge graph.

**Warning signs:**
- You have two "today's note" commands and aren't sure which to use
- Journal files accumulate in two different directories
- You search for a daily note and can't find it
- org-roam graph shows journal entries cluttering the visualization

**Phase to address:**
Phase 3 (org-journal) — must make the architectural decision before configuring.

---

### Pitfall 9: denote and org-roam File Naming Conflicts

**What goes wrong:**
denote creates files with its own naming convention: `YYYYMMDDTHHMMSS--title__keywords.org`. org-roam creates files with a different convention (default: `%<%Y%m%d%H%M%S>-${slug}.org`). When both systems share a directory, or when you try to cross-reference between them, file names clash, org-roam indexing gets confused by denote's double-dash/double-underscore conventions, and denote's regex-based search breaks on org-roam's ID-based file headers.

**Why it happens:**
denote and org-roam have fundamentally different philosophies. denote embeds metadata in the filename (no database). org-roam stores metadata in a SQLite database and uses IDs. These are complementary but require careful separation.

**How to avoid:**
**Keep denote and org-roam in separate directories.** This is the only sane approach:
```
~/org/
  gtd/          → GTD files (inbox.org, projects.org, etc.)
  roam/         → org-roam knowledge base
  denote/       → denote quick notes
  journal/      → org-journal (if separate from roam dailies)
```

Configure denote to not scan org-roam's directory:
```elisp
(setq denote-directory (expand-file-name "~/org/denote/"))
```

Do NOT add denote-directory to org-roam-directory or vice versa. If you want to link between them, use standard org `file:` links rather than `id:` links.

**Warning signs:**
- org-roam-db-sync throws errors about malformed files
- denote's `denote-link` commands can't find some files
- Files with both `:ID:` properties and denote-style filenames
- Double-indexed files appearing in both `org-roam-node-find` and `denote-open-or-create`

**Phase to address:**
Phase 1 (directory structure decisions) and Phase 3 (denote configuration).

---

### Pitfall 10: org-super-agenda Configuration Before Basic Agenda Works

**What goes wrong:**
You jump straight into configuring `org-super-agenda-groups` with complex auto-category, auto-priority, and custom matchers before verifying that the basic `org-agenda` even shows the right files and TODO items. When the grouped view is empty or wrong, you can't tell if the problem is in the super-agenda grouping rules or in the underlying agenda file/TODO configuration.

**Why it happens:**
org-super-agenda doesn't change *what* items appear in agenda — it only changes *how they're grouped and displayed*. If your `org-agenda-files` is wrong, or your TODO states don't match your filter criteria, org-super-agenda just groups "nothing" into pretty categories of "nothing."

**How to avoid:**
1. First get basic `org-agenda` working (the `a` key shows items from your GTD files)
2. Verify with `M-x org-agenda a` that you see TODO items, scheduled items, deadlines
3. Only then enable org-super-agenda and start adding groups one at a time
4. Test each group addition: does the agenda still show all items, just differently arranged?

```elisp
;; Start with this — verify it works:
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-agenda-span 'day)))
          (alltodo "")))))

;; Only then add super-agenda grouping:
(org-super-agenda-mode)
(setq org-super-agenda-groups
      '((:name "Next Actions" :todo "NEXT")
        (:name "In Progress" :todo "STRT")
        (:name "Waiting" :todo "WAIT")
        (:auto-priority t)))
```

**Warning signs:**
- org-super-agenda is enabled but agenda view looks the same as without it
- Agenda view shows groups but they're all empty
- You can't figure out why an item appears in the wrong group (it's actually a TODO state issue)

**Phase to address:**
Phase 1 (basic agenda) must work before Phase 4 (org-super-agenda polish).

---

### Pitfall 11: org-roam Database Corruption / emacsql Version Mismatch

**What goes wrong:**
After a `doom upgrade` or `doom sync`, org-roam throws emacsql errors like "wrong type argument", "database is locked", or "unable to open database file". The org-roam database becomes unusable and `org-roam-node-find` returns errors instead of nodes.

**Why it happens:**
org-roam uses emacsql for SQLite access. Doom pins package versions, and after upgrades the emacsql version might change while the database schema remains from the old version. Additionally, emacsql recently (2023-2024) switched from a custom C binary (`emacsql-sqlite`) to Emacs 29+'s built-in `sqlite` support (`emacsql-sqlite-builtin`), and this transition can cause database format issues.

On NixOS specifically: the sqlite binary needs to be available in PATH. If emacsql tries to compile its own sqlite binary, it may fail in Nix's sandboxed build environment.

**How to avoid:**
1. Ensure `sqlite` is in system packages (already done in this config: `pkgs.sqlite` in `users/common/packages.nix`) — **verified**
2. After any `doom upgrade`, run `M-x org-roam-db-sync` to rebuild the database
3. If emacsql errors occur, delete the database file and rebuild: `rm ~/.emacs.d/org-roam.db && emacs -e '(org-roam-db-sync)'`
4. Pin org-roam in `packages.el` if upgrade stability is critical: `(package! org-roam :pin "COMMIT_HASH")`
5. With Emacs 29+, prefer the built-in sqlite module — verify with `(sqlite-available-p)`

**Warning signs:**
- emacsql error messages on startup or first org-roam command
- "Database is locked" errors
- `doom doctor` warnings about emacsql or sqlite
- org-roam worked yesterday but not after a `doom upgrade`

**Phase to address:**
Phase 2 (org-roam setup) — include database rebuild as part of the setup validation.

---

## Minor Pitfalls

### Pitfall 12: Refile Chaos — Items Going to Wrong Files

**What goes wrong:**
You `org-refile` an item and it lands in a random heading in the wrong file, or the refile completion shows hundreds of headings with no way to find the right one. With org-roam potentially adding hundreds of files to the candidate pool, refile becomes unusably noisy.

**How to avoid:**
Configure `org-refile-targets` explicitly — don't rely on defaults:
```elisp
(setq org-refile-targets
      '(("~/org/gtd/projects.org" :maxlevel . 2)
        ("~/org/gtd/someday.org" :maxlevel . 1)
        ("~/org/gtd/tickler.org" :maxlevel . 1)))
```
Use `org-refile-use-outline-path 'file` (Doom already sets this) to show full paths. Do NOT add org-roam files to refile targets — refile is for GTD, not for knowledge base.

**Warning signs:**
- Refile completion shows 500+ candidates
- You can't find your target heading in the completion list
- Items end up in the wrong file after refiling

**Phase to address:**
Phase 1 (GTD Foundation).

---

### Pitfall 13: org-roam-ui Not Rendering or Extremely Slow

**What goes wrong:**
org-roam-ui opens a browser tab but shows a blank page, or the graph loads but becomes unresponsive with more than a few hundred nodes. The WebSocket connection between Emacs and the browser drops silently.

**How to avoid:**
- Ensure `websocket` package is available (dependency of org-roam-ui)
- For large graphs (500+ nodes), use org-roam-ui's filtering to show only a subgraph
- Set `(setq org-roam-ui-sync-theme t org-roam-ui-follow t org-roam-ui-update-on-save t)` for better behavior
- If blank page: check browser console for errors, verify websocket port isn't blocked
- org-roam-ui is a nice-to-have visualization — don't make it a critical part of your workflow

**Warning signs:**
- Browser tab opens but shows white/blank page
- Graph renders but is laggy/unresponsive
- Emacs hangs when opening org-roam-ui with many nodes
- WebSocket connection errors in `*Messages*`

**Phase to address:**
Phase 5 (Visual Polish) — should be one of the last things configured.

---

### Pitfall 14: Agenda View Overload — Too Many Custom Views

**What goes wrong:**
You create 10+ custom agenda commands: daily view, weekly view, work context, home context, errands context, waiting-for view, project review view, someday/maybe browser, reading list, habit tracker... Each requires remembering a different key. You end up only using 2 of them and the rest rot.

**How to avoid:**
Start with **3 agenda views maximum**:
1. `d` — Daily dashboard (today's agenda + next actions + waiting items)
2. `w` — Weekly review (all projects + all waiting + someday/maybe)
3. `t` — All TODOs (flat list for searching)

Add more only when you genuinely need them for a repeated workflow. Each view should serve a distinct GTD ritual (daily review, weekly review, processing).

**Warning signs:**
- You can't remember which key opens which agenda view
- More than 5 custom agenda commands
- You have views you haven't opened in weeks

**Phase to address:**
Phase 1 (GTD Foundation) — start with 2-3 views.

---

### Pitfall 15: Not Establishing the Capture Habit First

**What goes wrong:**
You build an elaborate system with beautiful agenda views, interconnected notes, and custom dashboards — but you never develop the muscle memory for `M-x org-capture` (or `SPC X` in Doom). The system is built but empty because nothing flows into it. After a week of not using it, the agenda is empty and feels pointless.

**How to avoid:**
The first week after setting up Phase 1 should focus exclusively on the **capture habit**:
- Bind `org-capture` to a comfortable keybinding (Doom: `SPC X` or `SPC o c`)
- Every time you think of a task, capture it — even if it's trivial
- Process inbox daily (refile items from inbox.org to their proper files)
- Only move to the next phase after you've captured 20+ items and processed them

**Warning signs:**
- Your inbox.org has 0 items after a week of using the system
- You're still writing tasks in a separate app or plain text file
- You built Phase 3+ but Phase 1 capture workflow isn't habitual

**Phase to address:**
Phase 1 (GTD Foundation) — cannot be automated, requires human behavior change.

---

### Pitfall 16: NixOS Rebuild Breaks Doom After Package Changes

**What goes wrong:**
You add `org-roam` to `packages.el`, run `sudo nixos-rebuild switch`, and then Doom is in a broken state because `doom sync` wasn't run. Or worse, you run `doom sync` but the Nix-managed Emacs binary has changed and Doom's compiled files are stale.

**How to avoid:**
The correct order after changing Doom package declarations:
1. Edit `packages.el` (in the repo: `users/doom.d/packages.el`)
2. Run `sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"` (deploys the new packages.el)
3. Run `doom sync` (installs the new Emacs packages)
4. Restart Emacs
5. Run `doom doctor` to verify

If Emacs won't start after rebuild:
```bash
doom sync --rebuild   # Force recompilation
doom purge           # Clean stale packages
```

**Warning signs:**
- Emacs shows "Package X not found" errors
- Emacs starts but with void-function errors for new packages
- `doom doctor` shows package sync warnings

**Phase to address:**
All phases — every time a new package is added.

---

## Technical Debt Patterns

Shortcuts that seem reasonable but create long-term problems.

| Shortcut | Immediate Benefit | Long-term Cost | When Acceptable |
|----------|-------------------|----------------|-----------------|
| `add-to-list 'org-capture-templates` instead of `setq` | Preserves Doom defaults | Fragile ordering, templates accumulate on reload | Never for GTD — use `setq` with complete template list |
| Putting all org config in one `(after! org)` block | Simple, one place | 300+ line block, hard to debug, can't selectively disable | Only for <50 lines total |
| Using `org-agenda-files (list org-directory)` with org-roam in org-directory | Agenda sees everything | Agenda scans thousands of roam files, becomes very slow | Never once roam has >100 files |
| Skipping `file-truename` on `org-roam-directory` | Works on most systems | Breaks silently on NixOS with symlinks, hard to debug | Never on NixOS |
| Hardcoding file paths instead of using variables | Quick to write | Breaks on multi-user setup (cipher vs jrothberg) | Never in this config (two users share it) |

## Integration Gotchas

Common mistakes when connecting org subsystems together.

| Integration | Common Mistake | Correct Approach |
|-------------|----------------|------------------|
| org-roam + org-agenda | Adding entire org-roam-directory to org-agenda-files | Keep GTD files separate; use targeted agenda commands for roam TODOs |
| org-journal + org-roam-dailies | Configuring both without deciding which is primary | Choose one for daily notes; if both, use separate directories and clear purpose for each |
| denote + org-roam | Sharing the same directory | Separate directories; link between them with `file:` links |
| org-super-agenda + custom agenda | Configuring groups before basic agenda works | Get plain agenda working first, then layer on super-agenda |
| org-roam + Doom's capture | Using org-roam-capture-templates without understanding Doom's capture defaults | Understand that org-roam capture is separate from org-capture; both can coexist with distinct keybindings |
| org-modern + org-superstar | Installing both (they conflict) | Choose one: org-modern (recommended, actively maintained) or org-superstar (simpler, heading bullets only) |

## Performance Traps

Patterns that work at small scale but fail as usage grows.

| Trap | Symptoms | Prevention | When It Breaks |
|------|----------|------------|----------------|
| All org files in `org-agenda-files` | Agenda takes 10+ seconds to load | Only add GTD files to agenda-files, not roam directory | >200 files in agenda-files |
| `org-roam-db-update-on-save t` (default) with very large org files | Noticeable save lag | Set to `nil` for files >100KB, run periodic `org-roam-db-sync` | Files >100KB with many links |
| Unlinked references in org-roam buffer | Buffer takes 30+ seconds to render | Disable unlinked references: remove `org-roam-unlinked-references-section` from `org-roam-mode-sections` | >500 roam nodes |
| `org-roam-node-find` with thousands of nodes | 3-6 second delay opening completion | This is a known org-roam issue (#2474, #2330); use `org-roam-node-find` with initial filter or `vertico` for async completion | >2000 nodes |
| org-roam-ui with full graph | Browser tab crashes or freezes | Use filtering; limit displayed nodes; treat as occasional visualization, not daily tool | >500 nodes |

## "Looks Done But Isn't" Checklist

Things that appear complete but are missing critical pieces.

- [ ] **GTD Capture:** Templates defined ≠ inbox processing workflow established — verify you actually process inbox weekly
- [ ] **org-roam setup:** `org-roam-db-sync` runs ≠ backlinks work — verify by creating two linked notes and checking the backlinks buffer
- [ ] **Agenda views:** Views defined ≠ views useful — verify by populating with real data, not just test TODOs
- [ ] **org-journal:** Journal files created ≠ journal in agenda — verify `org-journal-dir` is in `org-agenda-files` if you want journal TODOs in agenda
- [ ] **Refile targets:** Refile works ≠ refile is fast — verify completion shows expected targets, not hundreds of irrelevant headings
- [ ] **Directory structure:** Directories created ≠ all systems use them — verify `org-directory`, `org-roam-directory`, `org-journal-dir`, `denote-directory` all point to the right places
- [ ] **Keybindings:** Keys bound ≠ keys work — verify each SPC-based keybinding actually triggers the correct command
- [ ] **Two-user compatibility:** Config works for cipher ≠ config works for jrothberg — verify paths use `~` not hardcoded usernames

## Recovery Strategies

When pitfalls occur despite prevention, how to recover.

| Pitfall | Recovery Cost | Recovery Steps |
|---------|---------------|----------------|
| Capture templates overwritten by Doom | LOW | Check `org-capture-templates` value with `C-h v`; add your templates in correct `after!` block; `doom sync` |
| org-roam database corruption | LOW | Delete `org-roam.db`, run `M-x org-roam-db-sync` — data is in .org files, DB is just cache |
| Symlink path mismatch | MEDIUM | Add `file-truename` to directory settings, delete and rebuild org-roam DB, verify with `M-x org-roam-db-diagnose-node` |
| Over-engineered GTD states | LOW | Simplify `org-todo-keywords` to 6-8 states, bulk-rename old states with `org-map-entries` |
| Performance degradation from too many agenda files | MEDIUM | Separate `org-agenda-files` from `org-roam-directory`; profile with `emacs-init-time` and `benchmark-run` |
| Conflicting journal/dailies systems | MEDIUM | Pick one, migrate files from the other, update config to single system |
| Broken Doom after NixOS rebuild | LOW | `doom sync --rebuild && doom purge && doom doctor` |

## Pitfall-to-Phase Mapping

How roadmap phases should address these pitfalls.

| Pitfall | Prevention Phase | Verification |
|---------|------------------|--------------|
| Doom overwrites capture templates | Phase 1 (GTD Foundation) | `M-x org-capture` shows all custom templates |
| TODO states conflict with Doom | Phase 1 (GTD Foundation) | `C-c C-t` shows custom GTD states with correct faces |
| org-roam symlink failure | Phase 2 (org-roam) | `org-roam-node-find` returns nodes; backlinks work |
| Building everything at once | All phases | Each phase has explicit validation gates |
| Agenda doesn't see roam TODOs | Phase 1 + Phase 2 | Create TODO in roam note; verify it appears in agenda |
| Capture template bloat | Phase 1 (start minimal) | ≤5 templates initially |
| `after!` timing issues | Phase 2+ (every package) | `doom doctor` clean; features work on cold restart |
| org-journal vs org-roam-dailies | Phase 3 (org-journal) | Clear decision documented; no duplicate daily systems |
| denote/org-roam directory conflict | Phase 1 (directory structure) | Separate directories; no cross-indexing errors |
| org-super-agenda before basic agenda | Phase 4 (only after Phase 1 validated) | Basic agenda works before super-agenda enabled |
| emacsql/database corruption | Phase 2 (org-roam) | `org-roam-db-sync` completes without errors |
| Refile chaos | Phase 1 (GTD Foundation) | Refile shows ≤30 targets from GTD files only |
| org-roam-ui blank/slow | Phase 5 (Visual Polish) | Graph renders; treat as optional visualization |
| Agenda view overload | Phase 1 + Phase 4 | ≤3 views initially; each serves a distinct ritual |
| Not establishing capture habit | Phase 1 (human behavior) | 20+ real items captured and processed in first week |
| NixOS rebuild breaks Doom | All phases | `doom sync && doom doctor` after every rebuild |

## Sources

- Doom Emacs org module source: `modules/lang/org/config.el` — verified Doom's capture template defaults, TODO keywords, appearance hooks, refile settings (HIGH confidence)
- Org-roam User Manual v2.3.1-devel: https://www.orgroam.com/manual.html — verified symlink warning, `file-truename` recommendation, `org-roam-db-autosync-mode`, SQLite caching, performance section (HIGH confidence)
- org-roam GitHub Issue #2474: Master Issue for Making Org-Roam Faster — verified performance concerns with large databases, node-find completion slowness (HIGH confidence)
- org-roam GitHub Issue #2547: org-roam-db-sync fails with "too many open files" (5400+ files) — verified scale limits (MEDIUM confidence, specific to macOS but pattern relevant)
- org-roam GitHub Issues #2550, #2551, #2582, #2584: Capture template regressions — verified capture system is actively changing and fragile (HIGH confidence)
- Existing codebase analysis: `users/doom.d/config-org.el`, `users/doom.d/packages.el`, `users/doom.d/init.el`, `users/common/packages.nix` — verified current state, sqlite availability, existing capture templates, Doom org module flags (HIGH confidence)
- org-roam manual section on dailies/extensions — verified org-roam-dailies feature overlap with org-journal (HIGH confidence)

---
*Pitfalls research for: Org-mode Life Management in Doom Emacs (NixOS)*
*Researched: 2026-02-24*
