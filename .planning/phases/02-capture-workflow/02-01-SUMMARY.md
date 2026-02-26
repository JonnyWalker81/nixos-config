---
phase: 02-capture-workflow
plan: 01
subsystem: org
tags: [doom-emacs, org-mode, org-capture, gtd, elisp]

# Dependency graph
requires:
  - phase: 01-gtd-foundation
    provides: GTD file layout, tags, and after! org override-safe baseline
provides:
  - Canonical org-capture template set (t/i/p/m) in config-org-gtd.el
  - DWIM capture command with explicit global hotkeys for DWIM and full menu
  - Quick idea template auto-finalization with default Org return-to-context flow
  - Removal of legacy capture-template mutation in config-org.el
affects: [02-02, 03-01, 04-01, 06-02, 08-02]

# Tech tracking
tech-stack:
  added: []
  patterns: [single-source capture template setq in after! org, explicit DWIM key selection wrapper]

key-files:
  created: []
  modified: [users/doom.d/config-org-gtd.el, users/doom.d/config-org.el]

key-decisions:
  - "Locked Phase 2 capture key contract to t/i/p/m with no alternate keys"
  - "DWIM route is context-driven by current file (meetings->m, projects->p, default->t)"
  - "Only idea capture uses :immediate-finish t; richer templates stay interactive"

patterns-established:
  - "Capture templates are centralized in config-org-gtd.el via one org-capture-templates setq"
  - "CAP-01 entrypoints are explicit global-set-key bindings for DWIM and full menu"

# Metrics
duration: 1min
completed: 2026-02-26
---

# Phase 2 Plan 1: Canonical inbox capture and hotkey entry Summary

**Canonical org-capture templates with DWIM routing, explicit CAP-01 global hotkeys, and low-interruption idea finalize behavior.**

## Performance

- **Duration:** 1 min
- **Started:** 2026-02-26T04:01:16Z
- **Completed:** 2026-02-26T04:03:01Z
- **Tasks:** 3
- **Files modified:** 2

## Accomplishments
- Added a single canonical `org-capture-templates` definition in `config-org-gtd.el` with locked mnemonic keys `t/i/p/m`.
- Added `my/org-capture-dwim-key` + `my/org-capture-dwim` and explicit global bindings: `C-c c` (DWIM) and `C-c C` (full menu).
- Removed legacy capture template mutation from `config-org.el` to prevent load-order drift and duplication.
- Enabled low-interruption capture by applying `:immediate-finish t` only to the idea template.

## Task Commits

Each task was committed atomically:

1. **Task 1: Define canonical mnemonic template keys and CAP-01 global hotkeys** - `d983990` (feat)
2. **Task 2: Remove conflicting legacy capture template mutation** - `30d5650` (fix)
3. **Task 3: Implement and verify low-interruption quick capture finalize behavior** - `122b88c` (feat)

## Files Created/Modified
- `users/doom.d/config-org-gtd.el` - Added canonical capture templates, DWIM key selector/command, global hotkeys, and quick idea immediate-finish behavior.
- `users/doom.d/config-org.el` - Removed legacy `add-to-list 'org-capture-templates` mutation.

## Decisions Made
- Locked the Phase 2 capture mnemonic key contract to `t` task, `i` idea, `p` project, `m` meeting.
- Kept capture entrypoints explicit and separate: `C-c c` for DWIM and `C-c C` for full capture menu.
- Applied `:immediate-finish t` only to quick idea capture to keep richer templates editable.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] `doom` CLI unavailable in executor environment**
- **Found during:** Task 1 and Task 3 verification
- **Issue:** `doom sync` could not run because `doom` is not available on PATH in this environment.
- **Fix:** Verified capture behavior with static `rg` checks, `check-parens`, and batch Emacs runtime assertions for DWIM key routing/invocation.
- **Files modified:** None
- **Verification:** Batch Emacs checks passed for DWIM routing (`m/p/t`) and `(org-capture nil (my/org-capture-dwim-key))` behavior.
- **Committed in:** N/A (verification-only workaround)

---

**Total deviations:** 1 auto-fixed (1 blocking)
**Impact on plan:** Core CAP-01/CAP-02 behavior is implemented and verified in code/runtime emulation; no scope creep.

## Authentication Gates

None.

## Issues Encountered
- `doom sync` could not be executed in this shell because `doom` command is not installed/available on PATH.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness
- `02-02-PLAN.md` can build on the locked template contract and explicit entrypoints already in place.
- No functional blockers in code; optional environment follow-up is to expose `doom` binary in PATH for direct `doom sync` checks.

---
*Phase: 02-capture-workflow*
*Completed: 2026-02-26*
