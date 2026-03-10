---
phase: 09-agenda-runtime-hardening-flow-recovery
plan: 01
subsystem: ui
tags: [doom-emacs, org-agenda, org-super-agenda, dashboard, elisp]

# Dependency graph
requires:
  - phase: 03-basic-agenda
    provides: "Daily/weekly agenda commands and org-super-agenda grouping baseline"
  - phase: 04-advanced-agenda-gtd-reviews
    provides: "Review and context agenda commands that share the agenda runtime path"
  - phase: 08-integration-dashboard
    provides: "Dashboard quick actions and canonical SPC o agenda wrapper entrypoints"
provides:
  - "Runtime-safe org-super-agenda preparation and fallback group resolution for agenda command settings"
  - "Canonical daily/weekly/review/inbox/context agenda wrappers that always prepare runtime before dispatch"
  - "Dashboard and integration agenda entrypoints routed onto the same hardened agenda path"
affects: [09-02-regression-verification, v1-milestone-closure]

# Tech tracking
tech-stack:
  added: []
  patterns: [Guarded org-super-agenda runtime preparation before org-agenda dispatch, shared agenda wrapper path across dashboard and canonical keybindings, noninteractive-safe standalone config loading for batch verification]

key-files:
  created: [.planning/phases/09-agenda-runtime-hardening-flow-recovery/09-01-SUMMARY.md]
  modified: [users/doom.d/config-org-agenda.el, users/doom.d/config-org-integration.el, tests/emacs/orglife-config-tests.el]

key-decisions:
  - "Kept one canonical org-life-agenda-default-super-groups variable and resolved command settings through org-life-agenda-super-groups-safe instead of raw self-reference."
  - "Moved all agenda entrypoints onto org-life-agenda-dispatch so runtime preparation happens consistently before org-agenda."
  - "Made standalone batch loading tolerant of missing Doom/dashboard runtime so verification commands can exercise these modules outside full Doom startup."

patterns-established:
  - "Agenda runtime safety lives in config-org-agenda.el and is reused by dashboard and SPC o integrations instead of duplicating direct org-agenda calls."
  - "Batch verification paths should degrade safely when Doom-only helpers or dashboard state are absent."

# Metrics
duration: 11 min
completed: 2026-03-10
---

# Phase 9 Plan 1: Agenda Runtime Hardening Summary

**Agenda commands now prepare org-super-agenda safely at runtime, fall back to canonical grouping when bindings are missing, and route dashboard plus SPC o entrypoints through the same hardened dispatch path.**

## Performance

- **Duration:** 11 min
- **Started:** 2026-03-10T17:47:20Z
- **Completed:** 2026-03-10T17:58:35Z
- **Tasks:** 3
- **Files modified:** 3

## Accomplishments
- Added one canonical `org-life-agenda-default-super-groups` source plus `org-life-agenda-super-groups-safe` and `org-life-agenda-prepare-runtime` in `users/doom.d/config-org-agenda.el`.
- Replaced command-level raw `org-super-agenda-groups` dereferences with guarded resolver output so `d`, `w`, `r`, `R`, `H`, and `W` no longer depend on an already-bound variable.
- Added canonical wrapper functions for all agenda opens and pointed agenda keybindings at those wrappers.
- Routed dashboard quick actions and integration-owned agenda entrypoints through the shared hardened wrapper path in `users/doom.d/config-org-integration.el`.
- Extended automated coverage in `tests/emacs/orglife-config-tests.el` for unbound group recovery, wrapper availability, dashboard dispatch, and batch-safe integration startup.

## Task Commits

Each task was committed atomically:

1. **Task 1: Add runtime-safe super-agenda preparation and group resolution** - `70bb85d` (fix)
2. **Task 2: Route all agenda key entrypoints through hardened dispatch wrappers** - `41c5a15` (feat)
3. **Task 3: Harden dashboard quick actions and integration wrappers to reuse agenda safety path** - `b3e7357` (feat)

_Note: plan metadata commit is added after SUMMARY/STATE updates._

## Validation Evidence
- `emacs --batch -Q --eval "(progn (load-file \"users/doom.d/config-org-agenda.el\") (princ \"agenda-load-ok\"))"` succeeded.
- `emacs --batch -Q --eval "(progn (load-file \"users/doom.d/config-org-agenda.el\") (mapc (lambda (fn) (unless (fboundp fn) (error \"missing %s\" fn))) '(org-life-agenda-daily-planning org-life-agenda-weekly-planning org-life-agenda-daily-review org-life-agenda-weekly-review org-life-agenda-inbox-dashboard org-life-agenda-context-home org-life-agenda-context-work)) (princ \"agenda-wrappers-ok\"))"` succeeded.
- `emacs --batch -Q --eval "(progn (load-file \"users/doom.d/config-org-agenda.el\") (load-file \"users/doom.d/config-org-integration.el\") (princ \"integration-load-ok\"))"` succeeded.
- `tests/run-orglife-tests.sh` passed with 21/21 tests green.

## Files Created/Modified
- `users/doom.d/config-org-agenda.el` - added safe group resolution, runtime preparation, standalone batch-safe guards, and canonical agenda wrapper commands.
- `users/doom.d/config-org-integration.el` - routed dashboard review actions and canonical agenda references through agenda-module wrappers and hardened noninteractive dashboard startup behavior.
- `tests/emacs/orglife-config-tests.el` - added regression coverage for unbound super-group recovery and updated dashboard tests to assert wrapper-based dispatch.
- `.planning/phases/09-agenda-runtime-hardening-flow-recovery/09-01-SUMMARY.md` - execution summary and verification record for Phase 9 Plan 1.

## Decisions Made
- Centralized fallback grouping in `org-life-agenda-default-super-groups` so runtime repair and persistent `after! org-super-agenda` setup share the same source of truth.
- Used `org-life-agenda-dispatch` as the only agenda-opening path for canonical commands to keep preparation logic out of dashboard and keybinding call sites.
- Kept batch verification first-class by guarding missing Doom/dashboard runtime pieces instead of requiring a full interactive Doom session just to load these modules.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 3 - Blocking] Added standalone Doom macro fallbacks for batch verification**
- **Found during:** Task 1 (Add runtime-safe super-agenda preparation and group resolution)
- **Issue:** The required `emacs --batch -Q` verification crashed before loading the agenda module because `after!`/`map!` and `org-super-agenda-mode` were unavailable outside Doom startup.
- **Fix:** Added guarded fallback macro definitions and protected the eager `org-super-agenda-mode` call so standalone batch loads can reach the hardened agenda code.
- **Files modified:** `users/doom.d/config-org-agenda.el`
- **Verification:** Standalone `load-file` verification now prints `agenda-load-ok`.
- **Committed in:** `70bb85d` (Task 1 commit)

**2. [Rule 3 - Blocking] Made integration dashboard startup safe in noninteractive verification**
- **Found during:** Task 3 (Harden dashboard quick actions and integration wrappers to reuse agenda safety path)
- **Issue:** The required batch load for `config-org-integration.el` failed because Doom dashboard state was absent and `initial-buffer-choice` invoked dashboard opening during `emacs --batch -Q` shutdown.
- **Fix:** Added safe dashboard defaults plus a noninteractive initial-buffer helper so batch verification can load the integration module without a live Doom dashboard.
- **Files modified:** `users/doom.d/config-org-integration.el`, `tests/emacs/orglife-config-tests.el`
- **Verification:** Combined agenda + integration batch load now prints `integration-load-ok`, and the OrgLife suite stays green.
- **Committed in:** `b3e7357` (Task 3 commit)

---

**Total deviations:** 2 auto-fixed (2 blocking issues)
**Impact on plan:** Both fixes were required to satisfy the plan's exact verification commands and keep hardening work testable outside full Doom startup. No scope creep.

## Issues Encountered

None.

## Authentication Gates

None.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness
- Agenda runtime hardening is in place for manual agenda use, review flows, and dashboard quick actions.
- Ready for `09-02-PLAN.md` regression additions and end-to-end re-verification against the milestone audit gaps.

---
*Phase: 09-agenda-runtime-hardening-flow-recovery*
*Completed: 2026-03-10*
