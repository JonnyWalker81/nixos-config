# Phase 9: Agenda Runtime Hardening & Flow Recovery - Research

**Researched:** 2026-03-09
**Domain:** Doom Emacs Org agenda runtime safety (org-super-agenda binding, command-path hardening, E2E agenda flow recovery)
**Confidence:** HIGH

## Summary

Phase 9 is a reliability and integration-hardening phase, not a feature-expansion phase. The current repo already has complete agenda/review command coverage (`d`, `w`, `r`, `R`) and dashboard quick actions, but the milestone audit documents a runtime blocker: agenda command execution can hit `Symbol's value as variable is void: org-super-agenda-groups` and fail before rendering. This is the single failure mode that keeps AGN-01/02/03 open and cascades into Phase 4/8 flows.

Official Org docs confirm that `org-agenda-custom-commands` settings are variable/value pairs evaluated while building command views, and org-super-agenda docs explicitly require either global `setq` for persistent grouping or deliberate `let` binding for per-call grouping. Combined with current codebase evidence, the reliable implementation pattern is: make grouping data always bound, make package activation explicit at invocation paths, and keep an ungrouped fallback so agenda never crashes.

Research also shows the automated tests do not yet model the failure condition accurately: test macros make `after!` execute immediately and stubs define `org-super-agenda-mode`, which bypasses real load-order timing. Phase 9 planning should therefore include targeted regression tests that intentionally unbind/withhold `org-super-agenda-groups` at execution time and verify graceful agenda opening plus restored dashboard/flow paths.

**Primary recommendation:** Implement a runtime-safe agenda command path that guarantees `org-super-agenda-groups` is always bound (or nil fallback), explicitly prepares org-super-agenda before `org-agenda` dispatch, and adds regression tests that reproduce the historical unbound-variable failure.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| Org agenda (`org-agenda`) | Org 9.x (Doom-managed) | Custom command execution (`org-agenda-custom-commands`) and timeline views | Native, canonical agenda engine already used by all existing commands |
| `org-super-agenda` | `1.4-pre` (source header) / `1.3` latest tagged release | Grouping agenda items by priority/context/TODO | Required for AGN-03 and already declared in `users/doom.d/packages.el` |
| Doom config macros (`after!`, `use-package!`) | Doom current | Deterministic deferred config and package wiring | Existing repo convention and required for load-order-safe behavior |
| ERT (`ert`) | Emacs built-in | Regression tests for runtime safety and flow recovery | Existing test harness (`tests/emacs/orglife-config-tests.el`) already uses ERT |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| `config-org-agenda.el` command ownership | Repo-local module | Single source of truth for `d/w/r/R/I/H/W` agenda commands | All agenda hardening changes stay here |
| `config-org-integration.el` wrappers and dashboard actions | Repo-local module | Dashboard quick actions and canonical agenda wrapper commands | Validate and harden end-to-end flow recovery |
| Milestone verification docs | 2026-03 audit set | Canonical list of broken requirements/flows to close | Define test/verification scope for Phase 9 |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| Runtime-safe fallback when grouping unavailable | Hard fail if org-super-agenda missing/unbound | Breaks AGN-01/02 and downstream flows again; unacceptable for reliability phase |
| Explicit command-path prep helper before `org-agenda` | Rely only on `after! org-super-agenda` side effects | Timing-dependent; does not guarantee execution-time safety |
| Regression simulation of unbound variable path | Coverage that only checks definitions/load | Misses real invocation failure mode documented in audit |

**Installation:**
```bash
# No new package is required for baseline Phase 9.
# Ensure declared packages and autoloads are synchronized:
doom sync
```

## Architecture Patterns

### Recommended Project Structure
```
users/doom.d/
├── config-org-agenda.el       # agenda command definitions + grouping source + runtime guards
├── config-org-integration.el  # dashboard quick actions + agenda wrapper functions
└── packages.el                # org-super-agenda package declaration

tests/emacs/
└── orglife-config-tests.el    # runtime regression tests (including unbound/load-timing scenarios)
```

### Pattern 1: Runtime-Safe Group Resolver
**What:** Use a dedicated resolver that returns a valid group list or `nil` fallback, never a void symbol.
**When to use:** Any place command options currently reference `org-super-agenda-groups` directly.
**Example:**
```elisp
;; Source: org-super-agenda usage docs + repo runtime blocker analysis
(defvar org-life-agenda-default-super-groups
  '((:name "Priority A actionable" :and (:todo ("TODO" "NEXT") :priority "A") :order 0)
    (:name "WAITING (parked)" :todo "WAITING" :order 98)
    (:name "SOMEDAY (parked)" :todo "SOMEDAY" :order 99)))

(defun org-life-agenda-super-groups-safe ()
  "Return safe super-agenda groups, or nil when unavailable."
  (if (boundp 'org-super-agenda-groups)
      (or org-super-agenda-groups org-life-agenda-default-super-groups)
    nil))
```

### Pattern 2: Explicit Pre-Dispatch Preparation
**What:** Prepare grouping runtime (best-effort `require`, mode enable, variable bind) immediately before agenda dispatch.
**When to use:** All entrypoints that open agenda views (`d/w/r/R` and dashboard quick actions).
**Example:**
```elisp
;; Source: Doom getting_started.org + org-super-agenda README usage contract
(defun org-life-agenda--prepare-runtime ()
  "Prepare agenda grouping runtime without hard failure."
  (when (require 'org-super-agenda nil t)
    (org-super-agenda-mode 1)
    (unless (bound-and-true-p org-super-agenda-groups)
      (setq org-super-agenda-groups org-life-agenda-default-super-groups))))

(defun org-life-agenda-open-safe (key)
  "Open agenda command KEY with runtime hardening."
  (org-life-agenda--prepare-runtime)
  (org-agenda nil key))
```

### Pattern 3: Command Definitions Use Stable Value Sources
**What:** In `org-agenda-custom-commands`, set `org-super-agenda-groups` from a stable helper/value, not self-referential raw symbol dereference.
**When to use:** Every command block and command-level settings list that currently sets `(org-super-agenda-groups org-super-agenda-groups)`.
**Example:**
```elisp
;; Source: Org manual (setting options for custom commands)
(tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
           ((org-agenda-overriding-header "Unscheduled actionable")
            (org-super-agenda-groups (org-life-agenda-super-groups-safe))))
```

### Pattern 4: Regression Test Must Reproduce Historical Failure
**What:** Add a test that intentionally unbinds grouping variable and executes the agenda command path.
**When to use:** Phase 9 regression gate and future anti-regression checks.
**Example:**
```elisp
;; Source: milestone audit gap "load-timing/unbound-variable invocation path"
(ert-deftest orglife-agenda-path-survives-unbound-super-groups ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-agenda.el")
   (makunbound 'org-super-agenda-groups)
   (should-not (should-error (org-life-agenda-open-safe "d") :type 'void-variable))))
```

### Anti-Patterns to Avoid
- **Direct self-reference in command options:** `(org-super-agenda-groups org-super-agenda-groups)` without guard can reintroduce the crash.
- **Assuming `after!` alone is sufficient:** deferred config does not guarantee execution-time binding under all paths.
- **Testing only static config presence:** must execute command paths and assert no runtime errors.
- **Fixing only `d`/`w` but not `r`/`R`/dashboard calls:** leaves integration flows partially broken.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Agenda view assembly | Custom buffer renderer | `org-agenda-custom-commands` block agenda | Org already supports per-block and per-command options safely |
| Grouping engine | Manual regrouping of agenda strings | `org-super-agenda` selectors/groups | Mature grouping semantics and known integration points |
| Package load sequencing | Ad-hoc `load-file` chains | Doom `after!`/`use-package!` + guarded `require` | Fits Doom lifecycle and avoids brittle order assumptions |
| Runtime safety checks | One-off message logging | ERT regression tests that execute commands | Prevents silent reintroduction of known crash path |

**Key insight:** The right fix is wiring and guardrails around existing Org/Doom primitives, not a new agenda subsystem.

## Common Pitfalls

### Pitfall 1: Unbound variable dereference at command execution
**What goes wrong:** Agenda command throws `void-variable` before rendering.
**Why it happens:** Command options dereference `org-super-agenda-groups` without guaranteed binding.
**How to avoid:** Always use a safe resolver/default and pre-dispatch runtime prep.
**Warning signs:** `d/w/r/R` fail immediately; no agenda buffer is produced.

### Pitfall 2: Over-trusting test stubs for load-order safety
**What goes wrong:** Tests pass while real runtime still fails.
**Why it happens:** Test harness defines `after!` as immediate and stubs `org-super-agenda-mode`, masking timing issues.
**How to avoid:** Add tests that explicitly `makunbound` target vars and execute actual command wrappers.
**Warning signs:** Config assertions green, interactive invocation red.

### Pitfall 3: Fixing only agenda file, not dashboard command paths
**What goes wrong:** Manual `org-agenda` works, dashboard quick actions still break.
**Why it happens:** Dashboard action functions call `org-agenda` directly and bypass new prep helper.
**How to avoid:** Route all agenda opens through hardened wrapper functions.
**Warning signs:** `SPC o d/w` works, dashboard buttons fail.

### Pitfall 4: Group definitions drift across commands
**What goes wrong:** Daily/weekly/review render with inconsistent grouping behavior.
**Why it happens:** Multiple duplicated group lists and one-off edits.
**How to avoid:** Keep one canonical group definition variable and reuse it everywhere.
**Warning signs:** Sections differ by command without intentional reason.

## Code Examples

Verified patterns from official sources:

### Set per-command custom agenda options
```elisp
;; Source: https://orgmode.org/manual/Setting-options.html
(setq org-agenda-custom-commands
      '(("w" todo "WAITING"
         ((org-agenda-sorting-strategy '(priority-down))))))
```

### org-super-agenda persistent global groups contract
```elisp
;; Source: https://raw.githubusercontent.com/alphapapa/org-super-agenda/master/README.org
(org-super-agenda-mode)
(setq org-super-agenda-groups
      '((:name "Important" :priority "A")
        (:name "WAITING" :todo "WAITING")))
```

### Doom deferred configuration pattern
```elisp
;; Source: https://raw.githubusercontent.com/doomemacs/doomemacs/master/docs/getting_started.org
(after! org-agenda
  (setq org-agenda-start-on-weekday 1))
```

### Dashboard quick action dispatch path (repo-local)
```elisp
;; Source: users/doom.d/config-org-integration.el
(defun org-life-dashboard-action-weekly-review ()
  (interactive)
  (org-agenda nil "R"))
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Direct command option dereference of `org-super-agenda-groups` | Safe resolver + pre-dispatch prep + fallback nil/default groups | Required by 2026-03 milestone audit findings | Eliminates crash class; agenda opens even when grouping runtime is unavailable |
| Static config-presence tests only | Execution-path regression tests for unbound/load-timing failures | Required for Phase 9 gap closure | Detects real runtime regressions before they ship |
| Mixed direct `org-agenda` calls across modules | Canonical hardened wrappers reused by integration/dashboard | Phase 9 planning target | Restores E2E flows consistently |

**Deprecated/outdated:**
- Relying on `after! org-super-agenda` alone as proof that execution-time command paths are safe.

## Open Questions

1. **Fallback UX when org-super-agenda is unavailable**
   - What we know: AGN-01/02 require agenda to open reliably; crash is unacceptable.
   - What's unclear: Should fallback render ungrouped silently or display an explicit warning message in minibuffer/header?
   - Recommendation: Open ungrouped (no hard fail) and emit one concise warning message.

2. **Scope of hardened wrappers**
   - What we know: Success criteria explicitly name `d`, `w`, `R`, and dashboard quick actions; `r` is flow-critical for capture->review.
   - What's unclear: Whether to include context views (`H/W`) and inbox dashboard (`I`) in Phase 9 hardening pass.
   - Recommendation: Harden all agenda keys in one pass to avoid partial regressions.

## Sources

### Primary (HIGH confidence)
- `users/doom.d/config-org-agenda.el` - current command definitions and direct group-variable references.
- `users/doom.d/config-org-integration.el` - dashboard quick actions and agenda wrapper call paths.
- `tests/emacs/orglife-config-tests.el` - current test harness behavior and gap in unbound-variable simulation.
- `.planning/v1-MILESTONE-AUDIT.md` - authoritative list of broken requirements, integrations, and flows.
- https://orgmode.org/manual/Custom-Agenda-Views.html - custom agenda architecture.
- https://orgmode.org/manual/Setting-options.html - per-command/per-block agenda option semantics.
- https://orgmode.org/manual/Weekly_002fdaily-agenda.html - daily/weekly agenda behavior and span semantics.
- https://raw.githubusercontent.com/alphapapa/org-super-agenda/master/README.org - official usage contract for global groups and persistence.
- https://raw.githubusercontent.com/alphapapa/org-super-agenda/master/org-super-agenda.el - mode and variable definitions (`org-super-agenda-mode`, `org-super-agenda-groups`).

### Secondary (MEDIUM confidence)
- https://raw.githubusercontent.com/doomemacs/doomemacs/master/docs/getting_started.org - Doom macro/config conventions (`after!`, `use-package!`, module structure).

### Tertiary (LOW confidence)
- None.

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - based on repo code + official Org/org-super-agenda docs.
- Architecture: HIGH - directly derived from documented failure mode and current command ownership.
- Pitfalls: HIGH - grounded in audit findings and observable test/runtime mismatch.

**Research date:** 2026-03-09
**Valid until:** 2026-04-08 (30 days; moderate change velocity)
