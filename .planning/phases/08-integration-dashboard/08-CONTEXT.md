# Phase 8: Integration & Dashboard - Context

**Gathered:** 2026-03-08
**Status:** Ready for planning

<domain>
## Phase Boundary

Integrate existing OrgLife systems (GTD, agenda, org-roam, journal) so they are cross-linked with org-id, reachable through a consistent `SPC o` keybinding tree, and surfaced through a startup dashboard showing daily operational visibility (agenda, inbox, deadlines, and quick actions). This phase defines integration and access patterns only; new standalone capabilities remain out of scope.

</domain>

<decisions>
## Implementation Decisions

### Cross-linking behavior
- GTD task-to-roam links should use both visible inline links and stable ID-oriented metadata where useful.
- Link visibility should be optimized both in source text (tasks/journal entries) and in org-roam backlink views.
- Journal linking should default to linking specific task/project headings rather than only day-level journal headings.
- Link creation should be hybrid: prompts in key capture flows plus manual linking commands available anytime.

### SPC o key map shape
- `SPC o` should use a mixed structure: primarily domain-oriented grouping with select global shortcuts where helpful.
- Key depth may be flexible when it improves mnemonic clarity; strict 2-keystroke enforcement is not required for every action.
- Existing legacy keypaths should be preserved as aliases while `SPC o` becomes the unified map.
- Individual key choices should prioritize strong mnemonics.

### Dashboard content and order
- The first dashboard block should be today's agenda.
- Dashboard blocks should default to rich lists (not only counts).
- Upcoming deadlines should use a 14-day horizon.
- Always include quick actions for the Core 4: capture, daily review, weekly review, and roam find.

### Dashboard interaction flow
- Dashboard should appear automatically on every Emacs startup.
- Data should refresh automatically when opening the dashboard, with an explicit manual refresh action also available.
- Item open behavior should be context-aware (window handling varies by action type).
- Empty sections should show guidance text rather than being hidden.

### Claude's Discretion
- Exact ID/property schema for storing companion metadata alongside inline links.
- Exact branch structure and letter assignments for mixed `SPC o` grouping while preserving mnemonic intent.
- Exact formatting and item count thresholds for "rich list" dashboard blocks.
- Heuristics for context-aware open behavior (when to reuse current window vs split).

</decisions>

<specifics>
## Specific Ideas

- Dashboard should feel operational and actionable at startup, with concrete lists rather than abstract metrics.
- Deadline visibility should support medium-horizon planning (two-week lookahead).
- Navigation should preserve discoverability through mnemonic-first key choices.

</specifics>

<deferred>
## Deferred Ideas

None - discussion stayed within phase scope.

</deferred>

---

*Phase: 08-integration-dashboard*
*Context gathered: 2026-03-08*
