# Phase 5: Knowledge Base - Context

**Gathered:** 2026-02-26
**Status:** Ready for planning

<domain>
## Phase Boundary

Deliver an org-roam knowledge base in `~/org/roam/` with fuzzy note find/open, link insertion from org buffers, backlinks visibility, three capture templates (default, literature, concept), and org-roam-ui graph launch from Emacs.

This phase clarifies how those capabilities should behave in day-to-day use; it does not add new knowledge features.

</domain>

<decisions>
## Implementation Decisions

### Note identity rules
- Filename style should default to date+slug (timestamped uniqueness plus readable slug).
- Aliases should be used often (`ROAM_ALIASES`) to improve discoverability across alternate phrasing.
- Title-change filename policy should follow researched best practice and popular org-roam usage.
- Duplicate/similar-title behavior should follow researched best practice and popular org-roam usage.

### Capture template shape
- Default note template should be minimal (quick capture first).
- Literature template should stay flexible at capture time (no strict mandatory fields beyond normal capture essentials).
- Concept template should be definition-first.
- Metadata should be rich by default.

### Linking and backlinks flow
- Link insertion should balance create and link equally (not biased only to existing notes).
- If no good match appears, flow should offer immediate create from current query.
- Backlinks should use a mixed presentation (compact list with contextual visibility).
- With many backlinks, ordering should prioritize newest first.

### Graph interaction style
- Graph should be a frequent companion during note-taking, not occasional-only.
- Default graph scope should start from the current note neighborhood.
- Visual preference favors readable labels over dense node packing.
- Graph should support both exploration and navigation equally in regular workflow.

### Claude's Discretion
- Exact policy selection for title-change filename handling after reviewing org-roam best practices.
- Exact policy selection for duplicate/similar-title handling after reviewing org-roam best practices.
- Concrete field keys and ordering used to realize "rich metadata" within each template type.

</decisions>

<specifics>
## Specific Ideas

- User explicitly wants best-practice and popular-approach alignment for rename and duplicate title behaviors.
- The graph should feel usable as an always-available workspace companion, not just a novelty visualization.

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope.

</deferred>

---

*Phase: 05-knowledge-base*
*Context gathered: 2026-02-26*
