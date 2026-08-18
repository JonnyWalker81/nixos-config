# Phase 2: Capture Workflow - Research

**Researched:** 2026-02-25
**Domain:** Org capture workflow design in Doom Emacs GTD setup
**Confidence:** HIGH

## Summary

Phase 2 should be implemented with native `org-capture` only, using explicit template keys and target locations inside `~/org/gtd/`. The locked decisions map directly to Org capture primitives: `org-capture` key-based template selection, `file+headline` targets for template-specific inbox sections, structured multi-line templates for projects/meetings, and `:immediate-finish` for low-friction quick capture.

The standard implementation pattern is: define/override capture templates in `after! org`, expose one global command for context-aware default capture, and keep a menu path available for full template selection. Org already restores prior window context after finalizing capture, so the "return immediately" behavior should use defaults rather than custom window-management code.

For the delegated decision, meetings should go to a dedicated `~/org/gtd/meetings.org` file (top-level meeting entries with subsections and TODO action sub-items). This keeps `inbox.org` focused on triage while preserving agenda visibility, because `org-agenda-files` currently includes the whole `~/org/gtd/` directory.

**Primary recommendation:** Implement a `my/org-capture-dwim` wrapper that routes to a context-aware default template key via `(org-capture nil KEY)`, with a fallback path to the full menu, and define all templates in a single `after! org` `setq` block.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| GNU Emacs | 29.x (repo runtime) | Host editor/runtime | Required platform for Org capture workflows |
| Org mode (`org-capture`) | Built-in with Emacs 29 | Capture engine, templates, targets, finalize flow | Official, feature-complete capture system with template expansion and context support |
| Doom Emacs config (`after! org`) | Doom private config macros | Load-order-safe override point for Org settings | Needed in this repo to prevent Doom module defaults from overriding custom templates |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| `org-protocol` | Org built-in component | External/app-driven capture entrypoint | Use only if global OS/browser capture is added later (out of scope for this phase) |
| `org-agenda` integration | Org built-in | Makes TODO action items from capture visible in agenda | Always on; captures in `~/org/gtd/*.org` are automatically in scope |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| Native `org-capture` templates | Custom elisp buffer insertion functions | Loses built-in finalize/refile hooks, prompt syntax, and standardized capture flow |
| Dedicated meeting file (`meetings.org`) | Store meetings under `inbox.org` or `projects.org` | Dedicated file improves retrieval and avoids inbox/project clutter; alternatives reduce file count but mix concerns |

**Installation:**
```bash
# No new package required for Phase 2 core capture
# Uses built-in Org + existing Doom setup
```

## Architecture Patterns

### Recommended Project Structure
```
users/doom.d/
├── config-org-gtd.el        # GTD core + capture templates in after! org
└── config-org.el            # Keep non-GTD org settings separate

~/org/gtd/
├── inbox.org                # Tasks + ideas capture sections
├── projects.org             # Top-level project entries
└── meetings.org             # Dedicated meeting entries (Phase 2 recommendation)
```

### Pattern 1: Single Source of Truth Templates
**What:** Define `org-capture-templates` with `setq` in one `after! org` block (do not incrementally `add-to-list` scattered templates).
**When to use:** Always in this repo (Phase 1 already established Doom load-order risk).
**Example:**
```elisp
;; Source: https://orgmode.org/manual/Capture-templates.html
(after! org
  (setq org-capture-templates
        '(("t" "Inbox Task" entry
           (file+headline "~/org/gtd/inbox.org" "Tasks")
           "* TODO %^{Task} :%^{Context|@home|@work|@errands|@phone|@computer|@email}:\n%U\n%a"
           :empty-lines 1)
          ("i" "Idea" entry
           (file+headline "~/org/gtd/inbox.org" "Ideas")
           "* %U %?"
           :empty-lines 1))))
```

### Pattern 2: Context-Aware Default Entry Command
**What:** A wrapper command chooses a default template key and calls `org-capture` directly with that key.
**When to use:** For "capture starts with context-aware default" and quick single-key behavior.
**Example:**
```elisp
;; Source: https://orgmode.org/manual/Capture-templates.html
(defun my/org-capture-dwim ()
  (interactive)
  (let ((key (if (derived-mode-p 'org-mode) "t" "i")))
    (org-capture nil key)))

(global-set-key (kbd "C-c c") #'my/org-capture-dwim)
```

### Pattern 3: Structured Heading Templates for Projects and Meetings
**What:** Use multi-section `entry` templates; collect required prompts inline; keep actions as TODO children for meetings.
**When to use:** CAP-03 and CAP-04 templates.
**Example:**
```elisp
;; Source: https://orgmode.org/manual/Template-elements.html
("p" "Project" entry
 (file "~/org/gtd/projects.org")
 "* TODO %^{Project Name}\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Outcome\n** Notes\n** Next Actions\n"
 :empty-lines 1)

("m" "Meeting" entry
 (file "~/org/gtd/meetings.org")
 "* %^{Meeting Title} %^t\n:PROPERTIES:\n:ATTENDEES: %^{Attendees (optional)|}\n:CREATED: %U\n:END:\n** Notes\n%?\n** Action Items\n*** TODO \n"
 :empty-lines 1)
```

### Anti-Patterns to Avoid
- **Scattered template mutation:** Multiple `add-to-list` calls across files produce duplicate/unstable menus; use one canonical `setq`.
- **Custom window restore logic:** `org-capture-finalize` already returns to previous window config; avoid hand-rolled restore functions.
- **Immediate-finish on templates needing rich edits:** `:immediate-finish` is only for minimal/no-edit templates.
- **Mixing meeting capture into inbox by default:** Increases inbox processing load and weakens meeting retrieval.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Prompting for capture fields | Custom minibuffer prompt orchestration | Org template escapes (`%^{PROMPT}`, `%^t`, `%^g`, `%\\N`) | Built-in expansion handles ordering, defaults, and reuse robustly |
| Capture destination routing | Manual file parsing/insertion functions | Official targets (`file`, `file+headline`, `file+olp+datetree`) | Handles subtree creation rules and insertion semantics reliably |
| Post-capture context restore | Custom window-state save/restore | `org-capture-finalize` default behavior | Official flow already restores previous window configuration |
| Template-specific lifecycle hooks | Ad hoc wrapper chains everywhere | Template properties (`:hook`, `:before-finalize`, `:after-finalize`) | Keeps behavior local to template and maintainable |

**Key insight:** Org capture already solves the hard parts (targeting, prompting, finalize flow, hooks); custom replacements increase failure modes without adding value for Phase 2.

## Common Pitfalls

### Pitfall 1: Doom Load Order Overwrites Templates
**What goes wrong:** Custom templates disappear or default Doom templates reappear.
**Why it happens:** Template assignment occurs before Doom org module finishes setup.
**How to avoid:** Define `org-capture-templates` inside `after! org` and use one `setq`.
**Warning signs:** `M-x org-capture` menu does not match configured keys/descriptions after restart.

### Pitfall 2: Violating "quick capture" with over-prompting
**What goes wrong:** Quick capture feels as slow as full entry.
**Why it happens:** Required prompts added to all templates instead of mixed depth by template.
**How to avoid:** Keep task template limited to title + one context tag; keep idea template timestamp-first and mostly immediate.
**Warning signs:** More than 2-3 required prompts for quick templates.

### Pitfall 3: Incorrect target hierarchy assumptions
**What goes wrong:** Entries land at file top or wrong section, or fail when heading/file missing.
**Why it happens:** Using wrong target type (`file` vs `file+headline`) or missing bootstrap headings/files.
**How to avoid:** Use explicit targets per template and bootstrap `Tasks`/`Ideas` headings (and `meetings.org`) during setup.
**Warning signs:** Captures create unexpected structure or prompt for location unexpectedly.

### Pitfall 4: Action items not visible in agenda
**What goes wrong:** Meeting TODO sub-items are captured but not shown in agenda workflow.
**Why it happens:** Meeting file outside `org-agenda-files` scope.
**How to avoid:** Keep meetings file under `~/org/gtd/` (already covered by directory agenda scope).
**Warning signs:** TODO appears in file but not in agenda searches.

## Code Examples

Verified patterns from official sources:

### Global Capture Entry + Direct Template Key
```elisp
;; Source: https://orgmode.org/manual/Activation.html
;; Source: https://orgmode.org/manual/Capture-templates.html
(global-set-key (kbd "C-c c") #'org-capture)

(define-key global-map (kbd "C-c x")
  (lambda () (interactive) (org-capture nil "x")))
```

### Template with Target Headline and Return-to-Flow Finalize
```elisp
;; Source: https://orgmode.org/manual/Capture-templates.html
;; Source: https://orgmode.org/manual/Using-capture.html
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))
;; C-c C-c finalizes and returns to prior window configuration.
```

### Prompt and Expansion Controls
```elisp
;; Source: https://orgmode.org/manual/Template-expansion.html
(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline "~/org/gtd/inbox.org" "Ideas")
         "* %U %^{Title}\n%?\nFrom: %a\nTag: %^{Context|@home|@work|@computer}")))
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Legacy date/week tree target variants | Unified `file+olp+datetree` with `:tree-type` and `:time-prompt` properties | Org manual marks old variants deprecated (current manual) | New templates should use unified target syntax only |
| Manual context switching before capture | Direct key-specific capture `(org-capture nil "k")` and context mapping via `org-capture-templates-contexts` | Established in current Org capture docs | Enables true DWIM quick capture entrypoints |

**Deprecated/outdated:**
- Older non-`file+olp+datetree` date-tree target forms: deprecated in Org manual footnote under template elements.

## Open Questions

1. **Final hotkey choice for global "capture DWIM" in this Doom config**
   - What we know: Org recommends global bindings (`C-c c` typical), and Phase requires from-anywhere capture.
   - What's unclear: Whether this user has a conflicting custom binding for `C-c c` in day-to-day workflow.
   - Recommendation: Default to `C-c c` for DWIM and expose full menu on secondary binding (e.g., `C-c C`) if conflict appears during validation.

## Sources

### Primary (HIGH confidence)
- https://orgmode.org/manual/Capture.html - Capture overview and workflow intent.
- https://orgmode.org/manual/Setting-up-capture.html - Setup and global key guidance.
- https://orgmode.org/manual/Using-capture.html - Finalize behavior, prefix behavior, and insertion modes.
- https://orgmode.org/manual/Capture-templates.html - Template shape and direct key invocation (`org-capture nil KEY`).
- https://orgmode.org/manual/Template-elements.html - Targets, template properties (`:immediate-finish`, hooks, etc.), deprecation notes.
- https://orgmode.org/manual/Template-expansion.html - Prompt and expansion escapes.
- https://orgmode.org/manual/Templates-in-contexts.html - Context restrictions/remapping via `org-capture-templates-contexts`.
- https://orgmode.org/manual/Agenda-Files.html - Directory entries in `org-agenda-files` include `.org` files.
- `users/doom.d/config-org-gtd.el` - Existing GTD directory scope and `after! org` pattern in this repo.
- `users/doom.d/config-org.el` - Existing capture template mutation (`add-to-list`) and current org config layout.

### Secondary (MEDIUM confidence)
- None.

### Tertiary (LOW confidence)
- None.

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - Based on official Org manual + direct repo configuration evidence.
- Architecture: HIGH - Patterns map directly to documented Org capture mechanisms and existing repo conventions.
- Pitfalls: HIGH - Derived from official behavior plus observed local config risks (load-order and scattered template mutation).

**Research date:** 2026-02-25
**Valid until:** 2026-03-27 (30 days)
