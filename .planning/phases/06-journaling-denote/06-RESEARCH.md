# Phase 6: Journaling & Denote - Research

**Researched:** 2026-03-01
**Domain:** Emacs Org journaling (org-journal) + structured note naming (denote)
**Confidence:** HIGH

## Summary

This research focused on implementing the locked decisions for Phase 6 using the existing Doom Emacs + Org setup in this repository, specifically `org-journal` for daily journals and `denote` for structured, separate note files. The key concerns were: daily file behavior, timestamped entries, carry-over semantics from yesterday, agenda/search visibility, org-capture integration, and strict Denote naming/taxonomy in a dedicated directory.

The standard implementation is to keep journaling on `org-journal` with `org-journal-file-type` as `daily`, use a custom `org-journal-file-header` for a lightweight daily scaffold (including a dedicated tasks section + end-of-day reflection prompt), and configure carry-over via `org-journal-carryover-items` plus a custom `org-journal-handle-old-carryover-fn` to mark migrated source tasks instead of deleting them. For agenda integration, do not rely only on `org-journal-enable-agenda-integration` (current/future only); include `~/org/journal/` in agenda scope explicitly and isolate journal sections in custom agenda commands.

For Denote, keep `denote-directory` fixed to `~/org/denote/`, enforce strict keyword vocabulary with `denote-infer-keywords` set to `nil`, and use `denote-known-keywords` as the controlled taxonomy. Keep Denote and org-roam separated by directory and command ownership; optional linking from journal entries to Denote should be explicit and manual.

**Primary recommendation:** Implement `org-journal` as the daily workflow engine and `denote` as a strict, separate naming system, with explicit agenda scoping and a custom carry-over marker function to preserve migration traceability.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| `org-journal` | 2.2.0 (latest tag in upstream repo) | Per-day journal files, timestamped entries, carry-over, journal search, calendar integration | Purpose-built for Org daily journaling; built-in carry-over/search/capture location patterns |
| `denote` | 4.1.3 (latest tag), manual 4.1.0 stable docs | Structured filename-based note creation and retrieval | Mature GNU ELPA package with strong conventions and no DB requirement |
| `org` (Emacs Org mode) | Existing Doom/Org stack | Agenda, TODO states, capture templates, search | Required substrate for both org-journal and denote workflows |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| `org-capture` | Org built-in | Fast capture into journal locations | CAP-05 journal capture path and keybinding integration |
| `org-super-agenda` | existing repo package | Sectioned agenda views | Keep journal tasks visible but separated from GTD blocks |
| Doom `:lang org +journal +roam +capture` | existing repo module choice | Package/module integration in Doom | Preserve existing module strategy and avoid ad-hoc package wiring |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| `org-journal` for daily entries | plain Org files + custom functions | More custom code, no built-in carry-over/search semantics |
| Denote core workflow | `denote-journal` package | Adds Denote-based journaling layer; out of phase scope (journal already locked to `~/org/journal/`) |
| Denote for networked KB | org-roam | Different model (DB/backlinks graph). Locked decision is role separation, not replacement |

**Installation:**
```emacs-lisp
;; Doom modules already include org-journal via +journal.
;; Ensure Denote is declared (already present in this repo):
(package! denote)
```

## Architecture Patterns

### Recommended Project Structure
```
users/doom.d/
├── config-org-gtd.el       # org-directory, TODO states, capture base templates
├── config-org-agenda.el    # agenda ownership and custom command sections
├── config-org-roam.el      # roam-only behavior (keep separate from denote)
├── config-org-journal.el   # org-journal setup, carry-over, journal capture, keybinds
└── config-org-denote.el    # denote directory, taxonomy, keybinds
```

### Pattern 1: Daily Journal with Lightweight Scaffold
**What:** Use `org-journal` daily files with minimal header, timestamped entries, explicit tasks section, and end-of-day prompt.
**When to use:** Every `org-journal-new-entry` in `~/org/journal/`.
**Example:**
```emacs-lisp
;; Source: https://raw.githubusercontent.com/bastibe/org-journal/master/README.org
(setq org-journal-dir "~/org/journal/"
      org-journal-file-type 'daily
      org-journal-time-format "%R "
      org-journal-time-prefix "** ")

(setq org-journal-file-header
      (lambda (_time)
        (concat "#+title: Daily Journal\n"
                "* Tasks\n"
                "* Notes\n"
                "* End-of-day Reflection\n"
                "- What moved forward today?\n"
                "- What is still open?\n")))
```

### Pattern 2: Carry Over from Yesterday with Migration Marker
**What:** Set carry-over matcher for all open states and replace default deletion of source entries with explicit migration marking.
**When to use:** Automatically when opening today's journal (`org-journal-new-entry`).
**Example:**
```emacs-lisp
;; Source: https://raw.githubusercontent.com/bastibe/org-journal/master/README.org
;; and org-journal source variable docs in org-journal.el
(setq org-journal-carryover-items
      "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\"")

(defun my/org-journal-mark-old-carryover-as-migrated (old-carryover)
  (save-excursion
    (dolist (entry (reverse old-carryover))
      (save-restriction
        (narrow-to-region (car entry) (cadr entry))
        (goto-char (point-min))
        ;; preserve source task and mark migration traceability
        (org-set-tags ":migrated:")))))

;; Use new variable name in recent org-journal (old alias still exists)
(setq org-journal-handle-old-carryover-fn
      #'my/org-journal-mark-old-carryover-as-migrated)
```

### Pattern 3: Agenda + Search Split for Journal vs GTD
**What:** Include journal files in agenda scope but isolate them in dedicated agenda sections/commands.
**When to use:** Daily/weekly review commands in `config-org-agenda.el`.
**Example:**
```emacs-lisp
;; Source: https://raw.githubusercontent.com/bastibe/org-journal/master/README.org
;; org-journal's built-in integration only adds current+future files,
;; so include journal path explicitly for all open journal tasks.
(setq org-agenda-files '("~/org/gtd/" "~/org/journal/"))

;; journal text search over all history
;; C-u M-x org-journal-search => entire journal history
```

### Pattern 4: Strict Denote Taxonomy in Separate Directory
**What:** Use controlled keywords from day one in `~/org/denote/`, disable inferred keywords for strict naming.
**When to use:** All note creation via `denote` commands.
**Example:**
```emacs-lisp
;; Source: https://raw.githubusercontent.com/protesilaos/denote/main/README.org
(setq denote-directory (expand-file-name "~/org/denote/"))
(setq denote-known-keywords
      '("reference" "project" "support" "decision" "people" "meeting"))
(setq denote-infer-keywords nil) ; strict vocabulary
(setq denote-sort-keywords t)
(setq denote-prompts '(title keywords))
```

### Anti-Patterns to Avoid
- **Using `org-journal-enable-agenda-integration` as sole agenda source:** It tracks current/future journal files, not full history; explicitly scope agenda files for JRN-03.
- **Deleting old carry-over items:** Violates the locked requirement for migration traceability.
- **Merging Denote with org-roam directories:** Breaks locked directory/role separation and creates retrieval ambiguity.
- **Free-form Denote keywords with inference enabled:** Drifts taxonomy and weakens strict naming conventions.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Daily journal file lifecycle | Custom date-file generator | `org-journal-new-entry` + `org-journal-file-format` | Handles file creation, date logic, timestamps, and extension behavior |
| TODO carry-over engine | Manual file parsing/moving headings | `org-journal--carryover` via `org-journal-carryover-items` + custom handler fn | Built-in carry-over parser supports org match syntax and parent subtree handling |
| Journal capture target resolution | Custom "today file" locator | `org-journal-find-location` capture pattern | Officially documented integration point for org-capture |
| Denote filename grammar | Hand-built slug/timestamp naming | `denote` commands + `denote-prompts` + keyword controls | Avoids naming drift and keeps links/search conventions intact |

**Key insight:** Both `org-journal` and `denote` already solve the hard edge cases (date boundaries, filename parsing, keyword handling, carry-over mechanics). Custom replacements add maintenance risk without functional gain.

## Common Pitfalls

### Pitfall 1: Carry-over function variable mismatch
**What goes wrong:** Config sets `org-journal-handle-old-carryover` while current code documents `org-journal-handle-old-carryover-fn` (obsolete alias exists).
**Why it happens:** README examples may use older variable names; source has deprecation aliases.
**How to avoid:** Configure `*-fn` variable names directly in new code.
**Warning signs:** Byte-compile warnings or inconsistent behavior after upgrades.

### Pitfall 2: Journal TODOs missing from agenda history
**What goes wrong:** Older/open journal tasks disappear from agenda except current/future files.
**Why it happens:** `org-journal-enable-agenda-integration` intentionally limits scope.
**How to avoid:** Add `~/org/journal/` explicitly to `org-agenda-files` and keep a dedicated journal section.
**Warning signs:** Journal TODO from last week not visible in daily/weekly commands.

### Pitfall 3: Capture inserts duplicate headings/timestamps
**What goes wrong:** Using `org-journal-new-entry` without prefix in capture location inserts extra heading before template text.
**Why it happens:** Capture and org-journal both create headings unless told otherwise.
**How to avoid:** Use documented location helper calling `(org-journal-new-entry t)`.
**Warning signs:** Repeated timestamp headings after capture.

### Pitfall 4: Denote taxonomy drift
**What goes wrong:** Keyword set becomes inconsistent (`project`, `projects`, `proj-support`, etc.).
**Why it happens:** Keyword inference left enabled in strict-taxonomy workflow.
**How to avoid:** Set `denote-infer-keywords` to `nil`, define `denote-known-keywords`, and keep review discipline.
**Warning signs:** Similar concepts split across near-duplicate keywords.

### Pitfall 5: Cross-contaminating org-roam and denote workflows
**What goes wrong:** Notes are created in wrong directory or linked with wrong tooling assumptions.
**Why it happens:** Both packages manage notes but with different organizing principles.
**How to avoid:** Keep keybindings, creation commands, and directories separate by role.
**Warning signs:** Denote commands run in roam paths or vice versa.

## Code Examples

Verified patterns from official sources:

### Org-journal capture integration (today's journal)
```emacs-lisp
;; Source: https://raw.githubusercontent.com/bastibe/org-journal/master/README.org
(defun org-journal-find-location ()
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(add-to-list 'org-capture-templates
             '("j" "Journal entry" plain
               (function org-journal-find-location)
               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
               :jump-to-captured t))
```

### Journal full-history search
```emacs-lisp
;; Source: https://raw.githubusercontent.com/bastibe/org-journal/master/README.org
;; Built-in command: C-u M-x org-journal-search
;; Prefix argument searches the whole journal history.
```

### Denote strict keyword vocabulary
```emacs-lisp
;; Source: https://raw.githubusercontent.com/protesilaos/denote/main/README.org
(setq denote-infer-keywords nil)
(setq denote-known-keywords '("reference" "project" "support" "decision"))
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| `org-journal-handle-old-carryover` | `org-journal-handle-old-carryover-fn` | org-journal 2.3.0 alias transition | Prefer `*-fn` variable names in new config |
| `org-journal-find-file` | `org-journal-find-file-fn` | org-journal 2.3.0 alias transition | Use explicit `*-fn` to reduce future deprecation cleanup |
| Free-form keyword growth in Denote | Controlled vocabulary (`denote-known-keywords`, `denote-infer-keywords=nil`) | Documented in Denote 4.x manual | Enables strict taxonomy from day one |

**Deprecated/outdated:**
- Older org-journal README snippets using non-`-fn` variables: still work via alias, but not preferred for new configuration.

## Open Questions

1. **Migration marker format for carried items**
   - What we know: Source tasks must remain and be marked as migrated.
   - What's unclear: Preferred marker semantics (`:migrated:` tag vs TODO state like `MIGRATED` vs property).
   - Recommendation: Start with `:migrated:` tag for minimal TODO-keyword impact; revisit only if reporting needs change.

2. **Exact Denote taxonomy set for this user**
   - What we know: Strict naming/taxonomy is locked; primitives are discretionary.
   - What's unclear: Final canonical keyword list breadth (minimal vs extensive).
   - Recommendation: Begin with a small stable core (`reference`, `project`, `support`, `decision`, `people`, `meeting`) and expand only when repeated need appears.

## Sources

### Primary (HIGH confidence)
- `org-journal` README (official): https://raw.githubusercontent.com/bastibe/org-journal/master/README.org - setup, carry-over, agenda integration limits, capture integration.
- `org-journal` source (official): https://raw.githubusercontent.com/bastibe/org-journal/master/org-journal.el - current variable names, defaults, carry-over internals, package version header.
- `denote` manual/README (official): https://raw.githubusercontent.com/protesilaos/denote/main/README.org - directory, prompts, controlled keywords, file naming scheme.
- `denote` official manual page: https://protesilaos.com/emacs/denote - stable documentation and version metadata.

### Secondary (MEDIUM confidence)
- Git tag listing (`git ls-remote --tags`) for upstream version recency checks:
  - https://github.com/bastibe/org-journal
  - https://github.com/protesilaos/denote

### Tertiary (LOW confidence)
- None.

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - official docs + upstream tags + repo context agree.
- Architecture: HIGH - directly derived from locked decisions and official integration patterns.
- Pitfalls: HIGH - sourced from official docs/source behavior and current repo structure.

**Research date:** 2026-03-01
**Valid until:** 2026-03-31
