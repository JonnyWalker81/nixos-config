# Phase 3: Basic Agenda - Context

**Gathered:** 2026-02-25
**Status:** Ready for planning

<domain>
## Phase Boundary

Deliver daily and weekly Org agenda views that show scheduled items and deadlines, and add org-super-agenda grouping so items are organized by meaningful sections (priority, context, TODO state). This phase clarifies view behavior and grouping rules only; it does not add new capabilities beyond Basic Agenda.

</domain>

<decisions>
## Implementation Decisions

### Daily View Shape
- Daily agenda should use a balanced split between calendar timeline and grouped task sections.
- Unscheduled actionable tasks should be inline-mixed into the daily flow (not isolated to another command).
- Daily item density should be detailed planning level (rich metadata visible by default).
- Date window default is intentionally research-driven: choose the most common, best-practice daily scope for Org/GTD users (today-only vs today+overdue vs rolling model).

### Weekly Overview Style
- Weekly default should be a hybrid: day-by-day timeline plus summary sections.
- Week should start on Monday.
- Deadlines should appear both in each day and in a dedicated weekly deadline summary.
- Weekly density should be detailed planning level.

### Grouping Rules
- Primary grouping order should be priority then context.
- WAITING and SOMEDAY items should remain visible in bottom parking sections.
- When an item could fit multiple groups, prefer context match as the tie-breaker.
- Top grouped section choice is intentionally research-driven: select the most common, best-practice first section pattern in Org/GTD agenda workflows.

### Inclusion Boundaries
- Overdue items should always be included by default in both daily and weekly views.
- Weekly view should include unscheduled tasks by default, as grouped sections.
- Items without priority/context should appear in an explicit Uncategorized section.

### Claude's Discretion
- Default handling for completed states (DONE/CANCELLED) in daily/weekly views is open.

</decisions>

<specifics>
## Specific Ideas

- Preference throughout is planning-oriented, high-information views (detailed metadata, not compact scan).
- For two decision points (daily window and top section ordering), user explicitly wants a best-practices/popularity-informed choice from research.

</specifics>

<deferred>
## Deferred Ideas

None - discussion stayed within phase scope.

</deferred>

---

*Phase: 03-basic-agenda*
*Context gathered: 2026-02-25*
