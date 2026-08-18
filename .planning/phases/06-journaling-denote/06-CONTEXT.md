# Phase 6: Journaling & Denote - Context

**Gathered:** 2026-02-27
**Status:** Ready for planning

<domain>
## Phase Boundary

Enable daily journaling in `~/org/journal/` with per-day entries, TODO carry-over, agenda/search visibility, org-capture integration for journal entries, and denote notes in a separate `~/org/denote/` directory using structured naming. This phase does not add new capabilities beyond journaling + denote setup and their expected workflows.

</domain>

<decisions>
## Implementation Decisions

### Journal entry shape
- Default daily format is minimal freeform rather than a heavy scaffold.
- Entries should be timestamped for each new entry within the day file.
- Each day should include a dedicated tasks section.
- Reflection cadence should use an end-of-day prompt.

### Carry-over behavior
- Carry over all open states (not just TODO/NEXT).
- Carry-over runs automatically when opening today's journal.
- Carry only from yesterday, not from older backlog days.
- Original carried tasks remain in prior day and are marked as migrated.

### Agenda and search view
- All open journal TODOs should appear in agenda by default.
- Journal tasks should appear in a separate journal section (not mixed into GTD sections).
- Journal search should default to all journal history.
- Search should treat tasks/decisions and reflections equally (no bias).

### Denote note conventions
- Use a strict naming/taxonomy convention from the start.
- Prioritize denote for reference notes and project support notes.
- Keep denote and org-roam roles clearly separated.
- Journal-to-denote linking is optional and done only when useful.

### Claude's Discretion
- Exact wording of end-of-day reflection prompt.
- Visual formatting/details of the dedicated daily tasks section.
- Exact taxonomy primitives and keyword set that satisfy the strict convention.

</decisions>

<specifics>
## Specific Ideas

- Daily journaling should stay lightweight in structure while still preserving timestamped entry granularity.
- Carry-over behavior should preserve traceability in prior-day logs via explicit migration markers.
- Agenda should keep journal work visible but clearly separated from core GTD blocks.

</specifics>

<deferred>
## Deferred Ideas

None - discussion stayed within phase scope.

</deferred>

---

*Phase: 06-journaling-denote*
*Context gathered: 2026-02-27*
