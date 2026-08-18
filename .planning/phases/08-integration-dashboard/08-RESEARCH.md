# Phase 8: Integration & Dashboard - Research

**Researched:** 2026-03-07
**Domain:** Doom Emacs OrgLife integration (org-id cross-linking, keymap unification, startup dashboard)
**Confidence:** HIGH

## Summary

Phase 8 should be implemented by extending the existing Org modules already split in this repo (`config-org-gtd.el`, `config-org-agenda.el`, `config-org-roam.el`, `config-org-journal.el`) and by adding one integration module that owns cross-link commands, unified `SPC o` map, and dashboard widgets. This aligns with locked decisions: no new standalone subsystem, only integration/access patterns.

For cross-linking, the standard approach is to use Org-native ID links (`id:...`) plus explicit properties for companion metadata. Org supports both visible links and stable ID-backed targets (`org-store-link`, `org-id-get-create`, `org-entry-put`), and Org-roam explicitly treats ID links as canonical node links while still caching other links for external use. That directly supports "visible in source + stable metadata + useful backlinks".

For the dashboard, Doom's own dashboard is the correct host: it already appears on startup via `initial-buffer-choice` and is designed to be extended through `+doom-dashboard-functions`. Use this instead of introducing a parallel dashboard package. Build rich list blocks from existing agenda/data functions, keep a manual refresh command, and use context-aware open behavior via `display-buffer` actions.

**Primary recommendation:** Implement Phase 8 with one new `config-org-integration.el` that centralizes ID-link workflows, unified `SPC o`/aliases, and Doom-dashboard widgets backed by existing agenda/review command ownership.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| Org mode (`org`) | Doom pin `89df5bf...` (`release_9.8`) | IDs, links, capture, agenda data/query primitives | Canonical source of truth for GTD/journal tasks and link semantics |
| org-id (built into Org) | Org 9.8 surface | Stable identity via `ID` properties and `id:` links | Required for move-safe links and cross-file heading identity |
| org-roam | Doom pin `7cd906b...` | Backlinks/reflinks graph over Org IDs and links | Existing knowledge graph layer already configured in repo |
| Doom `:ui doom-dashboard` | Doom core module | Startup screen and widget pipeline (`+doom-dashboard-functions`) | Already enabled in `init.el`; supports startup auto-show and custom blocks |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| org-journal | Doom pin `831ecfd...` | Daily journal files and capture locations | Journal-task/project linking and history search |
| org-super-agenda | repo package (Doom/MELPA managed) | Grouped actionable lists in agenda flows | Reuse current review/dashboard sections instead of new query engine |
| Emacs window display API (`display-buffer`) | GNU Emacs current | Context-aware open behavior by action/category | Implement "reuse vs split" heuristics per action type |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| Doom dashboard widgets | Separate startup package (`dashboard.el`, custom tabulated UI) | Duplicates startup plumbing and diverges from existing Doom behavior |
| Org ID + property APIs | Handwritten text parsing for links/properties | Fragile with drawers/inheritance/multivalue properties |
| Existing agenda commands | New standalone query stack for this phase | Violates "integrate, don't duplicate" and increases maintenance |

**Installation:**
```bash
# No new package required for the phase baseline.
# If config changes touch Doom packages, run:
doom sync
```

## Architecture Patterns

### Recommended Project Structure
```
users/doom.d/
├── config-org-gtd.el          # capture templates and GTD file ownership
├── config-org-agenda.el       # existing agenda/review command ownership
├── config-org-roam.el         # roam node/backlink configuration
├── config-org-journal.el      # journal location/capture helper ownership
└── config-org-integration.el  # NEW: Phase 8 cross-links, SPC o unification, dashboard
```

### Pattern 1: Dual-Link Representation (Visible Link + Metadata)
**What:** Insert visible `[[id:...][Title]]` links in body text and mirror structured metadata in properties.
**When to use:** GTD task->roam links and journal->task/project links.
**Example:**
```elisp
;; Source: Org property API + Org-roam ID link guidance
(defun org-life-link-to-node-at-point (node-id node-title)
  (insert (format "[[id:%s][%s]]" node-id node-title))
  (org-entry-put nil "ORGLIFE_LINKED_NODE_ID" node-id)
  (org-entry-put nil "ORGLIFE_LINKED_NODE_TITLE" node-title))
```

### Pattern 2: Heading-Scoped Journal Links by Default
**What:** Create IDs on specific target headings (`task`/`project`) and link journal entries to those IDs, not only date headers.
**When to use:** Journal capture/link commands.
**Example:**
```elisp
;; Source: org-roam node definition + org-id-get-create usage
(defun org-life-link-current-heading-into-journal ()
  (interactive)
  (org-id-get-create)
  (let* ((id (org-entry-get nil "ID"))
         (title (org-get-heading t t t t)))
    (org-insert-link nil (concat "id:" id) title)))
```

### Pattern 3: Hybrid Link Creation (Capture Prompt + Manual Command)
**What:** Ask for linking during high-value capture flows, and provide always-available manual linking commands.
**When to use:** GTD capture templates and journal capture command paths.
**Example:**
```elisp
;; Source: Org capture template expansion + current repo capture ownership
(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "~/org/gtd/inbox.org" "Tasks")
         "* TODO %^{Task}\n%U\n%(org-life-capture-link-prompt)\n")))
```

### Pattern 4: Unified `SPC o` with Alias Preservation
**What:** Create one canonical domain-first `SPC o` tree, keep legacy keypaths as command aliases.
**When to use:** All org-life navigation entrypoints.
**Example:**
```elisp
;; Source: Doom map! patterns used in current repo
(map! :leader
      (:prefix ("o" . "org-life")
       (:prefix ("a" . "agenda") "d" (cmd! (org-agenda nil "d")))
       (:prefix ("g" . "gtd")    "i" #'my/org-gtd-open-inbox)
       (:prefix ("j" . "journal") "t" #'org-life-journal-open-today)
       (:prefix ("r" . "roam")   "f" #'org-life-roam-node-find)))

;; Preserve old entrypoint alias
(map! :leader :desc "Legacy inbox dashboard alias" "o a i" (cmd! (org-agenda nil "I")))
```

### Pattern 5: Doom Dashboard as Operational List Surface
**What:** Render agenda/inbox/deadline/quick-action blocks by extending `+doom-dashboard-functions`.
**When to use:** Startup dashboard requirement (auto-show + refresh on open + manual refresh command).
**Example:**
```elisp
;; Source: Doom dashboard module (`+doom-dashboard-functions`, `+doom-dashboard-reload`)
(after! doom-dashboard
  (defun org-life-dashboard-widget ()
    (insert "\nToday\n")
    (insert (org-life-dashboard-today-lines)))

  (add-to-list '+doom-dashboard-functions #'org-life-dashboard-widget t)
  (map! :leader :desc "Refresh OrgLife dashboard" "o d r" #'+doom-dashboard-reload))
```

### Pattern 6: Context-Aware Open Heuristics via `display-buffer`
**What:** Route opening behavior by item type using `display-buffer` action alists.
**When to use:** Dashboard item activation and quick actions.
**Example:**
```elisp
;; Source: Emacs display-buffer action model
(display-buffer buffer
                '((display-buffer-reuse-window display-buffer-pop-up-window)
                  (reusable-frames . visible)
                  (inhibit-same-window . nil)))
```

### Anti-Patterns to Avoid
- **Building a second dashboard framework:** Use Doom dashboard hooks already active at startup.
- **Storing only free-form link text:** Always keep machine-usable ID metadata in properties.
- **Replacing agenda ownership in `config-org-agenda.el`:** Extend existing commands/data, do not fork command definitions.
- **Hard-coding strict 2-key depth everywhere:** Locked decision allows deeper keys for mnemonic clarity.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Stable cross-file task identity | Custom UUID parsing/file-scoped anchors | `org-id-get-create`, `id:` links | Org already handles uniqueness, persistence, and movement across files |
| Property storage/readback | Manual drawer string edits | `org-entry-put`, `org-entry-get` | Avoids malformed drawers/inheritance bugs |
| Startup dashboard lifecycle | Ad-hoc startup hooks and bespoke buffer mode | Doom `+doom-dashboard-functions` + `+doom-dashboard-reload` | Startup + redraw + fallback-buffer behavior already implemented |
| Agenda-like rollups (deadlines, inbox lists) | New parser over Org files | Existing `org-agenda`/custom command ecosystem | Reuses proven filtering/scheduling semantics and existing phase ownership |
| Window routing heuristics | `split-window-*` branches everywhere | `display-buffer` actions/alists | Centralized, composable, and user-configurable display policy |

**Key insight:** This phase is an integration phase; leverage Org/Doom extension points and existing repo ownership boundaries instead of introducing new infrastructure.

## Common Pitfalls

### Pitfall 1: ID links created inconsistently
**What goes wrong:** Some entries get plain text/roam links without stable IDs.
**Why it happens:** Relying only on manual link insertion and skipping `org-id` creation.
**How to avoid:** Ensure linking commands call `org-id-get-create` for heading targets and persist metadata properties.
**Warning signs:** Backlinks exist for some tasks but break after refiles/moves.

### Pitfall 2: Dashboard becomes stale after edits
**What goes wrong:** Startup dashboard shows outdated agenda/deadline counts.
**Why it happens:** Widget data computed once without calling dashboard reload or agenda rebuild path.
**How to avoid:** Trigger refresh on dashboard open and expose explicit `SPC o d r` refresh command.
**Warning signs:** Manual agenda view differs from dashboard block contents.

### Pitfall 3: Keymap fragmentation under `SPC o`
**What goes wrong:** New canonical map exists, but users still depend on old scattered keypaths.
**Why it happens:** Migration removes legacy bindings instead of aliasing.
**How to avoid:** Keep legacy keypaths as aliases while documenting canonical paths.
**Warning signs:** Commands are reachable only via old paths or only via new paths, not both.

### Pitfall 4: Journal links point only to day files
**What goes wrong:** Backlinks are too coarse; task/project context is lost.
**Why it happens:** Capture links to journal-day heading without prompting heading-level target.
**How to avoid:** Default journal linking command to heading-level targets and create IDs there.
**Warning signs:** Roam backlinks show journal pages but not actionable task/project headings.

### Pitfall 5: Context-aware open behavior feels random
**What goes wrong:** Some actions hijack current window unexpectedly; others over-split.
**Why it happens:** No explicit display policy by action type.
**How to avoid:** Define deterministic heuristics (agenda/review in current window, roam find in other window, dashboard quick actions preserving dashboard when appropriate).
**Warning signs:** Frequent manual window cleanup after dashboard interactions.

## Code Examples

Verified patterns from official sources:

### Store/Insert stable Org links
```elisp
;; Source: https://orgmode.org/manual/Handling-Links.html
(org-store-link)
(org-insert-link)
```

### Work with properties using Org API
```elisp
;; Source: https://orgmode.org/manual/Using-the-Property-API.html
(org-entry-put nil "ORGLIFE_LINKED_NODE_ID" "7b77f...")
(org-entry-get nil "ORGLIFE_LINKED_NODE_ID")
```

### Org-roam links should be ID links
```elisp
;; Source: https://www.orgroam.com/manual.html (Links between Nodes)
;; Org-roam computes node links from standard Org id: links.
(org-id-get-create)
(org-insert-link nil (concat "id:" (org-entry-get nil "ID")) "Target")
```

### Extend Doom startup dashboard
```elisp
;; Source: https://raw.githubusercontent.com/doomemacs/doomemacs/master/modules/ui/doom-dashboard/config.el
(add-to-list '+doom-dashboard-functions #'my-dashboard-widget t)
(+doom-dashboard-reload)
```

### Context-aware display behavior
```elisp
;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window.html
(display-buffer some-buffer
                '((display-buffer-reuse-window display-buffer-pop-up-window)
                  (inhibit-same-window . nil)))
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| File/heading text links without stable IDs | `org-id` + `id:` links as canonical cross-file identity | Org ID workflow is now standard in Org + org-roam docs | Links survive refiles/moves and feed roam backlinks reliably |
| Standalone splash/dashboard packages | Doom dashboard widget extension via `+doom-dashboard-functions` | Doom dashboard module stabilized in modern Doom | Startup lifecycle is already solved; add domain widgets only |
| Ad-hoc split logic per command | `display-buffer` action model and alists | Emacs display-buffer model is the current canonical API | Consistent, testable window behavior by action type |

**Deprecated/outdated:**
- Building custom parsers to read/write property drawers directly; replace with Org property API.

## Implementation Blueprint

Recommended planner split:

1. **08-01 Cross-link primitives and metadata schema**
   - Add integration helpers for task<->roam and journal<->task/project links.
   - Enforce dual representation: visible inline link + property metadata schema.
   - Add capture prompt hooks for key flows and manual commands for anytime linking.

2. **08-02 Unified `SPC o` map with legacy aliases**
   - Define canonical domain-oriented `SPC o` tree with mnemonic assignments.
   - Preserve existing `SPC o r`, `SPC o j t`, and other legacy keypaths as aliases.
   - Keep agenda/review command ownership in existing agenda module.

3. **08-03 Startup dashboard widgets and interaction model**
   - Implement dashboard blocks in order: today agenda, inbox, deadlines (14 days), quick actions (Core 4).
   - Add auto-refresh-on-open and manual refresh command.
   - Add deterministic context-aware open heuristics using `display-buffer`.
   - Show explicit guidance text for empty sections.

## Verification Checklist

Planner-reusable checks:

```bash
# Validate config loads cleanly
doom doctor
```

In Emacs (`M-:` / `M-x`):

```elisp
;; ID and link primitives
(featurep 'org-id)

;; Dashboard extension active
(boundp '+doom-dashboard-functions)

;; Required key entrypoints present
(where-is-internal #'org-life-roam-node-find)
(where-is-internal #'my/org-capture-dwim)

;; Manual refresh available
(where-is-internal #'+doom-dashboard-reload)
```

Manual acceptance tests:
- Create/choose GTD task and link to a roam node: inline `id:` link is visible and companion properties are set.
- Link a journal entry to a specific project/task heading: roam backlinks show heading-level context.
- Run `SPC o` and confirm canonical tree discoverability; legacy keypaths still work as aliases.
- Restart Emacs: dashboard opens automatically with first block = today's agenda.
- Confirm dashboard shows rich list blocks for inbox + deadlines (14d) and quick actions for capture, daily review, weekly review, roam find.
- Trigger dashboard refresh command and verify updated data after changing TODO/deadline state.

## Open Questions

1. **Exact metadata key names for dual-link schema**
   - What we know: Org property API supports stable companion metadata cleanly.
   - What's unclear: Final naming convention (`ORGLIFE_*` vs shorter keys) preferred by maintainer.
   - Recommendation: Use a single namespaced prefix (`ORGLIFE_`) now; keep one migration helper if renamed later.

2. **Dashboard item count thresholds per block**
   - What we know: Locked decision requires rich lists and guidance for empty sections.
   - What's unclear: Preferred max visible items before truncation (e.g., 7/10/15).
   - Recommendation: Start with 10 items per block + "+N more" tail indicator.

## Sources

### Primary (HIGH confidence)
- https://raw.githubusercontent.com/doomemacs/doomemacs/master/modules/lang/org/packages.el - Org/Org-roam/Org-journal pins and versions used by Doom module.
- https://raw.githubusercontent.com/doomemacs/doomemacs/master/modules/ui/doom-dashboard/config.el - Dashboard startup behavior, widget extension points, reload lifecycle.
- https://orgmode.org/manual/Handling-Links.html - `org-store-link`, `org-insert-link`, and ID-link behavior.
- https://orgmode.org/manual/Using-the-Property-API.html - Official property read/write APIs (`org-entry-put/get`).
- https://orgmode.org/manual/Capture-templates.html - Capture template extension points for prompt-driven linking.
- https://orgmode.org/manual/Agenda-Commands.html - Agenda open/refresh interaction semantics.
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window.html - `display-buffer` action model for context-aware open heuristics.
- https://www.orgroam.com/manual.html - Node definition, ID-link semantics, backlinks/reflinks sections.
- https://raw.githubusercontent.com/bastibe/org-journal/master/README.org - Journal capture/find-file behavior and integration notes.

### Secondary (MEDIUM confidence)
- https://docs.doomemacs.org/latest/modules/ui/doom-dashboard/ - Module-level dashboard extension guidance.

### Tertiary (LOW confidence)
- None

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - based on upstream Doom package pins and official Org/Org-roam docs.
- Architecture: HIGH - constrained by locked decisions and validated against repo module ownership.
- Pitfalls: HIGH - derived from official APIs and current repo integration boundaries.

**Research date:** 2026-03-07
**Valid until:** 2026-04-06 (30 days; moderate change velocity)
