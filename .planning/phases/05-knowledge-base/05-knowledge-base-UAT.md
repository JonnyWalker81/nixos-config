---
phase: 05-knowledge-base
plan: 04
artifact: uat
status: needs_retest
updated: 2026-02-27T02:05:00Z
tests:
  - id: UAT-KB-06-graph
    status: pending
  - id: UAT-KB-02-find-open
    status: pending
  - id: UAT-KB-04-backlinks
    status: pending
---

# Phase 5 Knowledge Base Runtime UAT

This checklist captures interactive runtime verification that cannot be proven by static inspection.

## Runtime Command Path (User-Approved Deviation)

- Canonical keypath for this phase acceptance is `SPC n r`.
- `SPC o r` is currently reserved by Doom REPL in this setup and is not a runtime acceptance gate for Phase 05-04.
- Validation scope is unchanged: graph, find/open, and backlinks behavior must still pass.

## Test: UAT-KB-06-graph

- requirement: KB-06
- status: pending
- priority: blocking
- commands: `SPC n r g`, `SPC n r l`
- preconditions:
  - Emacs is running with active Doom profile.
  - `~/org/roam/` has at least two linked notes.
  - Browser can open the org-roam-ui graph page.
- steps:
  1. Open a roam note in Emacs.
  2. Run `SPC n r g`.
  3. Confirm org-roam-ui graph opens in a browser tab/window.
  4. Run `SPC n r l` from Emacs on the current note.
  5. Confirm graph focus updates to current note neighborhood.
  6. Click a different node in the graph.
- expected:
  - Graph launches without command errors.
  - Local graph command updates neighborhood focus.
  - Clicking a node opens the mapped roam note in Emacs.
- evidence:
  - executed_by: "user"
  - executed_at: "2026-02-27"
  - notes: "User-approved deviation: Phase 05-04 acceptance now validates graph flow via `SPC n r g/l` instead of `SPC o r g/l` so REPL ownership of `SPC o r` does not block completion."

## Test: UAT-KB-02-find-open

- requirement: KB-02
- status: pending
- priority: blocking
- command: `SPC n r f`
- preconditions:
  - Emacs is running with active Doom profile.
  - At least one existing roam note is available in the DB.
- steps:
  1. Run `SPC n r f`.
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
- blocker: "None (retest pending on user-approved canonical keypath `SPC n r`)."
- follow_up: "Retest required using `SPC n r f/g/l` and `org-roam-buffer-toggle`; record pass/fail evidence below."
