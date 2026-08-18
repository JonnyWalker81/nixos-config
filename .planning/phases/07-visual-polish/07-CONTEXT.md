# Phase 7: Visual Polish - Context

**Gathered:** 2026-03-05
**Status:** Ready for planning

<domain>
## Phase Boundary

Deliver visual polish for existing OrgLife workflows: modern Org buffer and agenda styling, roadmap-defined TODO state colors, hidden emphasis markup while editing, and inline image/LaTeX previews. This phase clarifies presentation and interaction defaults only; it does not add new workflows or capabilities.

</domain>

<decisions>
## Implementation Decisions

### Visual style density
- Overall Org buffer visual weight: clean and minimal.
- Headlines should be hierarchy-first, with clear level differentiation.
- Bullets and tables should use subtle modern styling, not high-contrast decorative treatment.
- Timestamps should use muted pill-style presentation.

### Agenda look & grouping
- Agenda should be compact but readable.
- Group headers should be quiet labels (subtle framing rather than bold section bars).
- Priority A sections should appear clearly first with mild emphasis.
- DONE/CANCELLED entries should be de-emphasized when shown.

### TODO color language
- Keep roadmap color semantics exactly as specified: NEXT=blue, WAITING=orange, SOMEDAY=grey, TODO=red, DONE=green, CANCELLED=dim.
- Use moderate saturation (clear distinction without neon intensity).
- NEXT should be the most visually prominent open state.
- Priority faces should be a subtle overlay; TODO state color remains primary.

### Markup & preview behavior
- Emphasis markers should reappear only at the cursor/edit location.
- Reveal/hide behavior should feel quick and smooth (brief anti-flicker timing).
- Inline images should display automatically when opening Org buffers.
- LaTeX fragments should preview automatically in the current Org buffer.

### Claude's Discretion
- Exact face attributes (specific hex values, font weights, and spacing increments) as long as they preserve the locked visual intent above.
- Exact timing values for reveal/hide behavior as long as it feels quick and smooth.
- Exact defaults for preview refresh hooks/commands, provided behavior matches auto-display decisions.

</decisions>

<specifics>
## Specific Ideas

- Preference is consistently "clean/minimal with clear hierarchy," not decorative or heavy.
- Agenda should stay scan-friendly through compact spacing and subtle visual framing.

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope.

</deferred>

---

*Phase: 07-visual-polish*
*Context gathered: 2026-03-05*
