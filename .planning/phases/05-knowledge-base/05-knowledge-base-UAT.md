---
phase: 05-knowledge-base
plan: 04
artifact: uat
status: needs_retest
updated: 2026-02-27T00:20:00Z
tests:
  - id: UAT-KB-06-graph
    status: failed
  - id: UAT-KB-02-find-open
    status: pending
  - id: UAT-KB-04-backlinks
    status: pending
---

# Phase 5 Knowledge Base Runtime UAT

This checklist captures interactive runtime verification that cannot be proven by static inspection.

## Test: UAT-KB-06-graph

- requirement: KB-06
- status: failed
- priority: blocking
- commands: `SPC o r g`, `SPC o r l`
- preconditions:
  - Emacs is running with active Doom profile.
  - `~/org/roam/` has at least two linked notes.
  - Browser can open the org-roam-ui graph page.
- steps:
  1. Open a roam note in Emacs.
  2. Run `SPC o r g`.
  3. Confirm org-roam-ui graph opens in a browser tab/window.
  4. Run `SPC o r l` from Emacs on the current note.
  5. Confirm graph focus updates to current note neighborhood.
  6. Click a different node in the graph.
- expected:
  - Graph launches without command errors.
  - Local graph command updates neighborhood focus.
  - Clicking a node opens the mapped roam note in Emacs.
- evidence:
  - executed_by: "user"
  - executed_at: "2026-02-27"
  - notes: "`SPC o r` opened Doom's default \"Open a REPL for\" prompt instead of org-roam submenu. Screenshot evidence captured in ~/Pictures."

## Test: UAT-KB-02-find-open

- requirement: KB-02
- status: pending
- priority: blocking
- command: `SPC o r f`
- preconditions:
  - Emacs is running with active Doom profile.
  - At least one existing roam note is available in the DB.
- steps:
  1. Run `SPC o r f`.
  2. In minibuffer completion, select an existing roam note.
  3. Confirm selection.
- expected:
  - Node finder opens with fuzzy completion candidates.
  - Selected existing note opens directly in Emacs.
- evidence:
  - executed_by: ""
  - executed_at: ""
  - notes: ""

## Test: UAT-KB-04-backlinks

- requirement: KB-04
- status: pending
- priority: blocking
- command: `org-roam-buffer-toggle`
- preconditions:
  - Emacs is running with active Doom profile.
  - Linked note dataset exists where current note has at least one inbound link.
- steps:
  1. Open a roam note known to have inbound links.
  2. Run `org-roam-buffer-toggle`.
  3. Review backlinks section in roam buffer.
- expected:
  - Backlinks section is visible.
  - Inbound links appear with context snippet content.
  - Ordering reflects recency intent (newer links first).
- evidence:
  - executed_by: ""
  - executed_at: ""
  - notes: ""

## Execution Outcome

- overall_status: blocked
- blocker: "Leader conflict at `SPC o r` prevented required org-roam commands (`f/g/l/u`) from being reachable."
- follow_up: "Rebound org-roam submenu to `SPC o r` (while keeping `SPC n r` compatibility). User re-test required for approval."
