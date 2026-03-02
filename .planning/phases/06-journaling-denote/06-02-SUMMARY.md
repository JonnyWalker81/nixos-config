---
phase: 06-journaling-denote
plan: 02
subsystem: workflow
tags: [doom-emacs, org-agenda, org-journal, org-capture, denote]

# Dependency graph
requires:
  - phase: 06-01
    provides: org-journal module ownership, open-today keypath, yesterday-only carry-over
provides:
  - Journal-specific agenda sections that pull TODOs from full journal history
  - CAP-05 journal capture template integrated into canonical capture templates
  - Dedicated denote module with strict keyword taxonomy in ~/org/denote/
affects: [07-visual-polish, 08-integration-dashboard]

# Tech tracking
tech-stack:
  added: []
  patterns:
    - Explicit journal agenda scoping via per-block `org-agenda-files`
    - Canonical org-capture template extension with journal location helper
    - Separate ownership of denote vs org-roam directories and keypaths

key-files:
  created:
    - users/doom.d/config-org-denote.el
  modified:
    - users/doom.d/config-org-agenda.el
    - users/doom.d/config-org-journal.el
    - users/doom.d/config-org-gtd.el
    - users/doom.d/config.el

key-decisions:
  - "Journal TODO visibility is added as dedicated agenda blocks scoped to ~/org/journal/, keeping GTD sections isolated."
  - "Journal capture uses a helper location function calling (org-journal-new-entry t) to avoid duplicate heading artifacts."
  - "Denote is loaded from a dedicated module with strict known keywords and keyword inference disabled."

patterns-established:
  - "Journal/GTD separation: shared agenda views with dedicated source-scoped sections"
  - "Strict note-system separation: denote in ~/org/denote/, org-roam in ~/org/roam/"

# Metrics
duration: 3 min
completed: 2026-03-02
---

# Phase 6 Plan 2: Journaling + Denote Integration Summary

**Journal TODOs are now visible in dedicated agenda sections with full-history search/capture support, and Denote is configured as a strict, separate note system in `~/org/denote/`.**

## Performance

- **Duration:** 3 min
- **Started:** 2026-03-02T19:34:07Z
- **Completed:** 2026-03-02T19:37:41Z
- **Tasks:** 3/3
- **Files modified:** 5

## Accomplishments
- Added explicit journal agenda scoping and dedicated Journal sections to daily, weekly, and review agenda commands.
- Added full-history journal search binding and CAP-05 capture integration using a journal location helper.
- Created and wired a dedicated `config-org-denote.el` module with strict keyword taxonomy and inference disabled.

## Task Commits

Each task was committed atomically:

1. **Task 1: Add journal agenda visibility and full-history search defaults** - `d55812b` (feat)
2. **Task 2: Extend canonical capture templates with CAP-05 journal capture** - `2ca56db` (feat)
3. **Task 3: Create dedicated denote module with strict taxonomy and separation** - `aeff46b` (feat)

_Note: plan metadata commit is added after SUMMARY/STATE updates._

## Files Created/Modified
- `users/doom.d/config-org-agenda.el` - Added journal-scoped agenda sections in daily/weekly/review commands.
- `users/doom.d/config-org-journal.el` - Added explicit journal agenda scope var, full-history search command, and capture location helper.
- `users/doom.d/config-org-gtd.el` - Added CAP-05 `org-capture` journal template using journal helper function.
- `users/doom.d/config-org-denote.el` - Added dedicated denote ownership, strict taxonomy, and note keybindings.
- `users/doom.d/config.el` - Added deterministic module load for `config-org-denote`.

## Decisions Made
- Kept GTD agenda defaults intact while explicitly injecting journal history via dedicated journal sections per command.
- Used `org-journal-new-entry` with prefix argument in capture location helper to prevent duplicate heading/timestamp artifacts.
- Bound denote commands under `SPC o n` to keep denote operations separate from existing org-roam keypaths.

## Deviations from Plan

None - plan executed exactly as written.

## Issues Encountered

None.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness

Phase 6 plan outcomes are now complete end-to-end for journaling + denote behavior and are ready for downstream visual polish/integration work.
No blockers identified.

---
*Phase: 06-journaling-denote*
*Completed: 2026-03-02*
