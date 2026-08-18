---
phase: 04-advanced-agenda-gtd-reviews
plan: 02
subsystem: ui
tags: [doom-emacs, org-agenda, gtd, weekly-review, stuck-projects]

# Dependency graph
requires:
  - phase: 04-01
    provides: Daily review command + context-isolated review command framework and shared review prefix formatting
provides:
  - Dedicated Weekly Review command with timeline, inbox triage, stuck projects, WAITING, and SOMEDAY sections
  - Explicit GTD stuck-project detection scoped to projects.org and driven by NEXT-child presence
  - Validation evidence for AGN-04 through AGN-07 command behavior and no-regression key access
affects: [05-knowledge-base, 08-integration-keybindings-dashboard]

# Tech tracking
tech-stack:
  added: []
  patterns: [Dedicated review command keys separated from planning keys, projects.org-scoped stuck detection with subtree NEXT checks]

key-files:
  created: [.planning/phases/04-advanced-agenda-gtd-reviews/04-02-SUMMARY.md]
  modified: [users/doom.d/config-org-agenda.el]

key-decisions:
  - "Weekly Review uses dedicated key R so existing weekly planning key w remains unchanged."
  - "Weekly GTD flow is ordered as timeline, inbox triage, stuck projects, WAITING commitments, then SOMEDAY parking."
  - "Stuck detection remains org-native aware (org-stuck-projects definition) but enforces correctness with explicit subtree NEXT checks."

patterns-established:
  - "Review command sections are numbered and map directly to GTD review steps for repeatable weekly processing."
  - "Stuck-project views are constrained to projects.org to avoid noise from non-project GTD files."

# Metrics
duration: 5 min
completed: 2026-02-26
---

# Phase 4 Plan 2: Weekly Review + Stuck Projects Summary

**Weekly GTD review now runs from one dedicated command that surfaces stalled projects, inbox triage, and parked commitments in a single repeatable flow.**

## Performance

- **Duration:** 5 min
- **Started:** 2026-02-26T22:32:54Z
- **Completed:** 2026-02-26T22:38:08Z
- **Tasks:** 3
- **Files modified:** 1

## Accomplishments
- Added a dedicated `R` Weekly Review command in `users/doom.d/config-org-agenda.el` without changing existing planning command `w`.
- Added Weekly Review sections for week timeline, inbox triage, stuck projects, WAITING commitments, and SOMEDAY/MAYBE parking with GTD-aligned section headers.
- Added explicit GTD stuck-project helpers that treat a level-1 project as stuck when it has no `NEXT` child.
- Added leader key access under `SPC o A R` for direct Weekly Review entrypoint.
- Executed AGN-04 to AGN-07 validation checks including stuck-project before/after reproduction and context view isolation.

## Task Commits

Each task was committed atomically:

1. **Task 1: Implement Weekly Review block agenda with GTD review sections** - `fd72739` (feat)
2. **Task 2: Wire robust stuck-project detection for GTD project files** - `8883d81` (feat)
3. **Task 3: Execute validation gate for AGN-04 through AGN-07** - `c38392d` (fix)

## Validation Evidence
- `emacs --batch -Q --eval "(with-temp-buffer (insert-file-contents \"users/doom.d/config-org-agenda.el\") (check-parens))"` exited successfully.
- `~/.emacs.default/bin/doom sync` completed successfully.
- Runtime validation script confirmed:
  - `daily_key=r`, `weekly_key=R`, `context_keys=H,W`
  - `daily_sections_ok=yes`
  - `weekly_sections_ok=yes`
  - `commands_present=yes`
  - `home_context_isolated=yes`
  - `work_context_isolated=yes`
  - `stuck_before_next=yes`
  - `stuck_after_next=no` after adding a `NEXT` child to the seeded stuck project

## Files Created/Modified
- `users/doom.d/config-org-agenda.el` - Added Weekly Review command, leader binding, projects.org-scoped stuck detection variables, and robust stuck filtering helpers.
- `.planning/phases/04-advanced-agenda-gtd-reviews/04-02-SUMMARY.md` - Execution summary and validation record for AGN-04 through AGN-07.

## Decisions Made
- Preserved Phase 3 planning command behavior (`w`) and added Weekly Review on separate command key `R`.
- Kept weekly review ordering explicit to mirror GTD weekly processing (triage inbox before stalled/parked review).
- Used explicit subtree NEXT checks for stuck detection reliability while retaining org-native stuck-project configuration metadata for future reuse.

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Fixed stuck project review false-positives during runtime validation**
- **Found during:** Task 3 (validation gate)
- **Issue:** Weekly stuck-project section continued to show a project after a `NEXT` child was added, which violated AGN-07 expectations.
- **Fix:** Added `my/org-gtd-project-has-next-child-p` and `my/org-agenda-skip-non-stuck-gtd-projects` so Weekly Review only keeps truly stuck top-level projects.
- **Files modified:** `users/doom.d/config-org-agenda.el`
- **Verification:** Runtime validation now shows `stuck_before_next=yes` and `stuck_after_next=no`.
- **Committed in:** `c38392d` (Task 3 commit)

---

**Total deviations:** 1 auto-fixed (1 bug fix)
**Impact on plan:** Auto-fix was required to satisfy AGN-07 correctness without changing scope.

## Authentication Gates

None.

## Issues Encountered

None.

## User Setup Required

None - no external service configuration required.

## Next Phase Readiness
- Phase 4 is complete with both daily and weekly GTD review flows validated.
- No blockers were introduced; existing Phase 5 sqlite/symlink concerns remain the known carry-forward items.

---
*Phase: 04-advanced-agenda-gtd-reviews*
*Completed: 2026-02-26*
