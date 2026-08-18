# Phase 3: Basic Agenda - Research

**Researched:** 2026-02-25
**Domain:** Org agenda daily/weekly workflows with org-super-agenda in Doom Emacs
**Confidence:** HIGH

## Summary

Phase 3 should be implemented as one Doom-safe custom command architecture in Org: a daily command and a weekly command, each built as a block agenda where the first block is `agenda` (timeline/deadlines) and subsequent blocks provide grouped unscheduled actionable work. This matches Org's intended model for custom agenda views (`org-agenda-custom-commands`) and directly supports the locked decisions for daily/weekly hybrid views.

For grouping, `org-super-agenda` should be used as a display/grouping layer only, not as a collector. Collection must come from agenda blocks (`agenda`, `tags-todo`, `todo`, `alltodo`), then grouping should enforce priority-first, context-second sections, with WAITING/SOMEDAY parked at the bottom and an explicit Uncategorized section. Overdue inclusion should rely on Org agenda behavior for scheduled/deadline carry-forward plus explicit weekly deadline summary filtering.

Best-practice defaults for this phase: daily scope should be one day (`org-agenda-span 'day`) while still including overdue items (Org already carries overdue deadline/scheduled warnings into today), and top grouped section should be "Priority A actionable" (after timeline) so the planning flow is: hard landscape first, then highest-impact unscheduled actions by context.

**Primary recommendation:** Create `users/doom.d/config-org-agenda.el`, loaded after `config-org-gtd.el`, and define all Phase 3 agenda commands/groups inside `after! org` plus `after! org-agenda`/`after! org-super-agenda`, with explicit Doom leader bindings to open daily (`A d`) and weekly (`A w`) views.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| GNU Emacs | 29.x (repo runtime) | Editor/runtime | Required host for Org agenda workflows |
| Org mode (`org-agenda`) | Built-in with Emacs 29 | Daily/weekly agenda engine, custom commands, sorting/filtering | Official agenda system; all phase requirements map to native features |
| Doom macros (`after!`, `map!`) | Doom core | Load-order-safe configuration and keybinding | Prevents override/race issues in this repo's Doom setup |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| `org-super-agenda` | Doom/MELPA pinned (`v1.3` latest tag; active `1.4-pre` changes) | Grouping sections by priority/context/TODO state | Use for AGN-03 grouping in both daily/weekly command blocks |
| `evil-org-agenda` | Already present in repo modules | Vim-style keys in agenda buffers | Keep enabled for navigation consistency, no custom rewrite needed |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| `org-agenda` + `org-super-agenda` block agenda | Custom elisp agenda buffer post-processing | Reinvents collection/sorting/grouping and is fragile across Org updates |
| Priority/context groups in `org-super-agenda` | Multiple separate `tags-todo` blocks only | Harder to maintain ordering/tie-breakers and duplicates logic in many places |

**Installation:**
```bash
# Package already declared in this repo:
# users/doom.d/packages.el -> (package! org-super-agenda)

~/.emacs.d/bin/doom sync
```

## Architecture Patterns

### Recommended Project Structure
```
users/doom.d/
├── config-org-gtd.el        # Existing GTD primitives (files, TODO states, tags)
├── config-org.el            # Existing generic org config
├── config-org-agenda.el     # NEW: Phase 3 agenda commands + super-agenda groups
└── config.el                # Load order; ensure config-org-agenda loads after gtd
```

### Pattern 1: Doom-Safe Agenda Command Definition
**What:** Define all `org-agenda-custom-commands` and agenda variables in `after! org` (or `after! org-agenda`) to avoid Doom/module override issues.
**When to use:** Always for this repo; existing GTD config already follows this pattern.
**Example:**
```elisp
;; Source: https://raw.githubusercontent.com/doomemacs/doomemacs/master/lisp/doom-lib.el (after! docstring)
;; Source: https://orgmode.org/manual/Custom-Agenda-Views.html
(after! org
  (setq org-agenda-start-on-weekday 1) ; Monday
  (setq org-agenda-custom-commands
        '(("d" "Daily plan"
           ((agenda "" ((org-agenda-span 'day)))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\""
                       ((org-agenda-overriding-header "Actionable (unscheduled)"))))))))
```

### Pattern 2: Block Agenda for Hybrid Daily/Weekly Views
**What:** Use one custom command per view, each with multiple blocks: timeline first (`agenda`), then summary/group blocks (`tags-todo`/`todo`).
**When to use:** Daily split layout and weekly hybrid overview.
**Example:**
```elisp
;; Source: https://orgmode.org/manual/Block-agenda.html
;; Source: https://orgmode.org/manual/Setting-options.html
("w" "Weekly plan"
 ((agenda "" ((org-agenda-span 'week)
               (org-agenda-start-on-weekday 1)))
  (tags-todo "DEADLINE<=\"<+7d>\""
             ((org-agenda-overriding-header "Weekly deadline summary")))
  (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\""
             ((org-agenda-overriding-header "Unscheduled actionable"))))
 ((org-agenda-prefix-format " %(priority) %-12:c %?-12t %e ")))
```

### Pattern 3: org-super-agenda Group Consumption Order
**What:** Define groups in strict match order and use `:order` for display stability; first matching group consumes the item.
**When to use:** Priority-first then context tie-breaking, plus WAITING/SOMEDAY parking sections.
**Example:**
```elisp
;; Source: https://raw.githubusercontent.com/alphapapa/org-super-agenda/master/README.org
(after! org-super-agenda
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "P1 - Priority A" :priority "A" :order 0)
          (:name "P2 - Priority B @work" :and (:priority "B" :tag "@work") :order 1)
          (:name "P2 - Priority B @home" :and (:priority "B" :tag "@home") :order 2)
          (:name "Uncategorized" :and (:not (:tag ("@home" "@work" "@errands" "@phone" "@computer" "@email")))
                                :order 90)
          (:name "WAITING" :todo "WAITING" :order 98)
          (:name "SOMEDAY" :todo "SOMEDAY" :order 99))))
```

### Anti-Patterns to Avoid
- **`let`-binding `org-super-agenda-groups` for persistent commands:** refresh (`g`) drops groups; use global `setq` or command settings.
- **Using org-super-agenda to "collect" missing tasks:** it only groups collected agenda items; collection must be fixed in command blocks.
- **Scattering agenda command mutation across multiple files:** creates hard-to-debug key/command drift.
- **Defining many custom predicates before baseline agenda works:** validate plain agenda blocks first, then layer grouping.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Daily/weekly assembly | Custom buffer renderer | `org-agenda-custom-commands` block agenda | Native support for multi-block agenda with per-block settings |
| Priority/context grouping | Manual item parsing/reordering | `org-super-agenda-groups` selectors (`:priority`, `:tag`, `:todo`, `:and`, `:order`) | Handles grouping semantics and header rendering consistently |
| Overdue handling | Custom overdue calculators | Org deadline/scheduled carry-forward in agenda + deadline match block | Native urgency/overdue logic already maintained by Org |
| Metadata-rich display | Handcrafted display strings per item | `org-agenda-prefix-format` and built-in columns (`%c`, `%t`, `%e`, etc.) | More stable and easier to tune per command |

**Key insight:** Keep collection in Org agenda blocks and grouping in org-super-agenda; blending these responsibilities into custom elisp makes behavior brittle and hard to verify.

## Common Pitfalls

### Pitfall 1: Doom Load Order Overrides Agenda Config
**What goes wrong:** Custom commands or keys disappear after restart/reload.
**Why it happens:** Agenda settings defined before Org/Doom module finalization.
**How to avoid:** Put agenda variables in `after! org` and super-agenda mode/group config in `after! org-super-agenda`.
**Warning signs:** `org-agenda` dispatcher does not show custom keys `d`/`w`.

### Pitfall 2: Grouping Rules Conflict with First-Match Consumption
**What goes wrong:** Items land in broad groups instead of specific context groups.
**Why it happens:** Broad selectors (e.g. `:priority "B"`) placed before narrow `:and` context selectors.
**How to avoid:** Define groups from most specific to most general; use `:order` for display, not for matching precedence.
**Warning signs:** Context sections unexpectedly empty while higher-level priority section is overloaded.

### Pitfall 3: Missing Weekly Deadline Summary Coverage
**What goes wrong:** Deadlines appear in day timeline but not in dedicated weekly summary (or vice versa).
**Why it happens:** Weekly summary block query/filter mismatched with weekly span.
**How to avoid:** Add explicit deadline summary block in the weekly command and verify each expected deadline appears in both places.
**Warning signs:** Known deadline visible in agenda days but absent from "Weekly deadline summary" section.

### Pitfall 4: DONE/CANCELLED Noise in Planning Views
**What goes wrong:** Daily/weekly views become cluttered with completed states.
**Why it happens:** Logbook/log mode and TODO queries include closed states.
**How to avoid:** Keep default planning views focused on open states; use log mode toggle when needed for audit.
**Warning signs:** Many DONE/CANCELLED entries in primary daily planning command.

### Pitfall 5: Keybinding Collisions in Doom
**What goes wrong:** New agenda commands are defined but hard to invoke consistently.
**Why it happens:** Binding only global keys or only leader keys without repo conventions.
**How to avoid:** Add Doom leader bindings via `map!` and keep command keys in `org-agenda-custom-commands`.
**Warning signs:** Users must remember ad-hoc `M-x` names to open daily/weekly views.

## Code Examples

Verified patterns from official sources:

### Daily/Weekly Command Skeleton (Block Agenda)
```elisp
;; Source: https://orgmode.org/manual/Block-agenda.html
;; Source: https://orgmode.org/manual/Setting-options.html
(setq org-agenda-custom-commands
      '(("d" "Daily"
         ((agenda "" ((org-agenda-span 'day))))
         ((org-agenda-prefix-format " %(priority) %-12:c %?-12t %e ")))
        ("w" "Weekly"
         ((agenda "" ((org-agenda-span 'week)
                       (org-agenda-start-on-weekday 1)))))))
```

### Persistent org-super-agenda Groups
```elisp
;; Source: https://raw.githubusercontent.com/alphapapa/org-super-agenda/master/README.org
(org-super-agenda-mode)
(setq org-super-agenda-groups
      '((:name "Priority A" :priority "A")
        (:name "WAITING" :todo "WAITING" :order 98)
        (:name "SOMEDAY" :todo "SOMEDAY" :order 99)))
```

### Doom-Safe Deferred Config and Keybinding
```elisp
;; Source: https://raw.githubusercontent.com/doomemacs/doomemacs/master/lisp/doom-lib.el (after! macro)
;; Source: https://raw.githubusercontent.com/doomemacs/doomemacs/master/docs/faq.org (map! guidance)
(after! org
  (map! :leader
        (:prefix ("o A" . "agenda")
         :desc "Daily agenda"  "d" (cmd! (org-agenda nil "d"))
         :desc "Weekly agenda" "w" (cmd! (org-agenda nil "w")))))
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Multi-command agenda without grouping (separate tag/todo views) | Block agenda + org-super-agenda grouping in one view | org-super-agenda mainstream usage (2018+) | Better single-screen planning workflow for daily/weekly use |
| Temporary grouping via local `let` around command calls | Persistent global `org-super-agenda-groups` (`setq`) | Documented in org-super-agenda usage notes | Grouping survives agenda refresh and day/week navigation |
| Relying on default weekly-only `a` agenda command | Dedicated custom daily+weekly command keys | Standard Org custom views pattern | Predictable GTD-focused entry points for AGN-01/02/03 |

**Deprecated/outdated:**
- Treating `org-super-agenda` as a query engine: outdated mental model; it is a grouping layer only.

## Best-Practice Defaults for Locked Decisions

### Daily Scope Default
- **Recommendation:** `org-agenda-span 'day` (today view) with overdue always included via Org carry-forward behavior and explicit overdue-aware blocks.
- **Rationale:** Org's daily agenda already surfaces overdue scheduled/deadline pressure in today's planning context, which is the standard daily triage model.
- **Implementation note:** Keep timeline to one day; add unscheduled actionable block(s) in same command for inline planning flow.

### Top Section Pattern
- **Recommendation:** In grouped task area, first section is `Priority A actionable` (after timeline block).
- **Rationale:** Matches locked priority-first grouping, aligns with Org urgency model (priority contributes strongly to sort urgency), and supports decisive daily planning.
- **Implementation note:** Put specific context splits immediately after each priority tier to satisfy context tie-break behavior.

## Concrete Implementation Guidance

### File-Level Plan
- Add `users/doom.d/config-org-agenda.el` for all Phase 3 agenda commands, super-agenda groups, and agenda keybindings.
- Update `users/doom.d/config.el` to load `config-org-agenda` immediately after `config-org-gtd`.
- Keep `users/doom.d/config-org-gtd.el` as source of TODO states/tags/agenda file scope; do not duplicate those settings in the new file.

### Verification Commands
```bash
# Sync Doom packages/autoloads after editing agenda config
~/.emacs.d/bin/doom sync

# Restart Emacs, then run these interactively:
# M-x org-agenda RET d
# M-x org-agenda RET w
```

### Acceptance Checks (Planner-Ready)
- **AGN-01 daily:** command `d` shows one-day timeline, includes overdue, and displays unscheduled actionable groups in same view.
- **AGN-02 weekly:** command `w` starts Monday, shows week timeline, includes overdue, includes unscheduled grouped tasks, and has a dedicated weekly deadline summary section.
- **AGN-03 grouping:** groups are priority-first then context; WAITING/SOMEDAY appear at bottom parking sections; items lacking context/priority appear in explicit Uncategorized section.
- **Metadata density:** agenda prefix displays priority/category/time/effort (or equivalent rich metadata) by default.
- **DONE/CANCELLED default:** closed states are hidden from planning blocks by default (can still be inspected via agenda log mode when needed).

## Open Questions

1. **Exact naming and key prefix for agenda entrypoints in this repo**
   - What we know: Repo uses `map!` heavily with descriptive leader prefixes, but no existing Org agenda prefix convention is established.
   - What's unclear: Preferred final leader path (`o A`, `a`, or another existing mnemonic).
   - Recommendation: Default to `SPC o A d` and `SPC o A w` to avoid collisions and keep Org-related discoverability.

## Sources

### Primary (HIGH confidence)
- https://orgmode.org/manual/Weekly_002fdaily-agenda.html - Daily/weekly span behavior and week start configuration.
- https://orgmode.org/manual/Block-agenda.html - Multi-block agenda architecture.
- https://orgmode.org/manual/Setting-options.html - Per-command and per-block agenda options.
- https://orgmode.org/manual/Sorting-of-agenda-items.html - Urgency/sorting model and priority influence.
- https://orgmode.org/manual/Presentation-and-Sorting.html - Prefix metadata customization.
- https://orgmode.org/manual/Deadlines-and-Scheduling.html - Overdue/deadline/scheduled carry-forward behavior.
- https://raw.githubusercontent.com/alphapapa/org-super-agenda/master/README.org - Group selector semantics, global scope requirement, first-match behavior.
- https://raw.githubusercontent.com/doomemacs/doomemacs/master/lisp/doom-lib.el - `after!` behavior and constraints.
- `users/doom.d/config-org-gtd.el` - Existing locked baseline (`after! org`, TODO states, tags, agenda file scope).
- `users/doom.d/config-org.el` - Existing custom agenda usage in repo.
- `users/doom.d/config.el` - Current Doom file loading order.
- `users/doom.d/packages.el` - `org-super-agenda` package declaration.

### Secondary (MEDIUM confidence)
- https://github.com/alphapapa/org-super-agenda/releases - Tagged release currency (`v1.3`, Sep 2023) vs ongoing README/changelog updates.

### Tertiary (LOW confidence)
- None.

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - Directly supported by Org manual, org-super-agenda README, Doom source docstrings, and existing repo config.
- Architecture: HIGH - Block agenda + grouped display is explicitly documented and maps cleanly to locked decisions.
- Pitfalls: HIGH - Confirmed by official behavior plus concrete risks visible in current repo structure/load order.

**Research date:** 2026-02-25
**Valid until:** 2026-03-27 (30 days)
