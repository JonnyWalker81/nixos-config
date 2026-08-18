---
phase: 08-integration-dashboard
plan: 01
subsystem: integration
tags: [doom-emacs, org-mode, org-id, org-roam, org-capture]

# Dependency graph
requires:
  - phase: 01-gtd-foundation
    provides: Stable GTD file structure and heading/task ownership for linking targets
  - phase: 05-knowledge-base
    provides: org-roam node APIs and ID-backed roam link semantics
  - phase: 06-journaling-denote
    provides: Journal capture location and journal workflow ownership boundaries
  - phase: 07-visual-polish
    provides: Deterministic org module load ordering used for integration insertion point
provides:
  - Dedicated integration owner module for stable org-id link creation and ORGLIFE metadata persistence
  - Manual GTD->roam and journal->heading commands that create visible id links plus machine-queryable properties
  - Capture-template prompt hooks for optional task/journal cross-link insertion
affects: [08-02, 08-03, UX-03, UX-04]

# Tech tracking
tech-stack:
  added: []
  patterns:
    [Dual representation links (visible id link + ORGLIFE_LINK_* metadata) owned by config-org-integration.el]

key-files:
  created: [users/doom.d/config-org-integration.el, .planning/phases/08-integration-dashboard/08-01-SUMMARY.md]
  modified: [users/doom.d/config.el, users/doom.d/config-org-gtd.el]

key-decisions:
  - "Use a single ORGLIFE_LINK_* schema (`KIND`, `TARGET_ID`, `TARGET_TITLE`, `TARGET_FILE`, `TARGET_TYPE`) stored via Org property APIs for stable, parse-safe metadata."
  - "Journal linking targets concrete GTD TODO headings collected from `~/org/gtd/` and never day-file-only journal headers."

patterns-established:
  - "Integration behavior is centralized in one owner module and consumed from capture templates through `%(org-life-integration-capture-link-prompt ...)`."
  - "All heading links must call `org-id-get-create` on targets before link insertion so IDs survive refiles/moves."

# Metrics
duration: 3 min
completed: 2026-03-08
---

# Phase 8 Plan 1: Cross-Link Foundation Summary

**Phase 8 now has a canonical integration module that creates stable org-id task/journal cross-links with inline visibility and ORGLIFE metadata, plus capture-time prompt hooks for hybrid linking.**

## Performance

- **Duration:** 3 min
- **Started:** 2026-03-08T04:56:50Z
- **Completed:** 2026-03-08T04:59:53Z
- **Tasks:** 3
- **Files modified:** 3

## Accomplishments
- Created `users/doom.d/config-org-integration.el` as the Phase 8 owner for linking primitives, including `org-life-link-task-to-roam`, `org-life-link-journal-to-heading`, and `org-life-integration-capture-link-prompt`.
- Added deterministic integration loading in `users/doom.d/config.el` between denote and visual modules to preserve existing org subsystem ownership boundaries.
- Wired canonical task and journal capture templates in `users/doom.d/config-org-gtd.el` to optional link prompts via `%(org-life-integration-capture-link-prompt ...)` while retaining existing key contracts (`t/i/p/m/j`).

## Task Commits

Each task was committed atomically:

1. **Task 1: Create integration module with org-id link primitives and metadata schema** - `92f7b4d` (feat)
2. **Task 2: Wire integration module into Doom load chain** - `7a153ac` (feat)
3. **Task 3: Add capture-flow link prompts for hybrid link creation** - `3d4b0e5` (feat)

## Verification Evidence
- `emacs --batch -Q --eval "(with-temp-buffer (insert-file-contents \"users/doom.d/config-org-integration.el\") (check-parens))"` passed.
- `rg "org-id-get-create|org-entry-put|ORGLIFE_LINK_" users/doom.d/config-org-integration.el` matched required primitives and metadata schema.
- `rg "org-life-integration-capture-link-prompt|%\(org-life-" users/doom.d/config-org-gtd.el users/doom.d/config-org-integration.el` confirmed capture-template wiring.
- `~/.emacs.default/bin/doom sync` completed successfully.
- `tests/run-orglife-tests.sh` passed (11/11).
- Batch runtime verification script exercised task->roam and journal->heading link flows and confirmed inline `id:` links plus `ORGLIFE_LINK_TARGET_ID` metadata insertion.

## Files Created/Modified
- `users/doom.d/config-org-integration.el` - Added reusable org-id link commands, GTD heading targeting helper, and capture prompt API.
- `users/doom.d/config.el` - Added deterministic integration module loader before visual module initialization.
- `users/doom.d/config-org-gtd.el` - Added capture-time optional linking hooks for task and journal templates.
- `.planning/phases/08-integration-dashboard/08-01-SUMMARY.md` - Execution summary for this plan.

## Decisions Made
- Adopted one namespaced ORGLIFE property schema for all Phase 8 link metadata writes and reads.
- Enforced heading-level journal targets by selecting TODO headings from GTD files and creating IDs on those headings before link insertion.
- Kept hybrid link UX: capture-time optional prompts plus separate manual commands available post-capture.

## Deviations from Plan

None - plan executed exactly as written.

## Authentication Gates

None.

## Issues Encountered

None.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness
- Integration foundations for UX-01 and UX-02 are in place with one canonical API surface for downstream keymap/dashboard work.
- Ready for `08-02-PLAN.md` to layer unified `SPC o` navigation on top of the established integration module.

---
*Phase: 08-integration-dashboard*
*Completed: 2026-03-08*
