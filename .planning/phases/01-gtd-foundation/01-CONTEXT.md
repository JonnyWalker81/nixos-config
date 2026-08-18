# Phase 1: GTD Foundation - Context

**Gathered:** 2026-02-24
**Status:** Ready for planning

<domain>
## Phase Boundary

Establish the core GTD primitives in Doom Emacs: org-directory, file structure (inbox.org, projects.org, someday.org, reference.org, archive/), 6 TODO states with logging, GTD context tags, refile with fuzzy completion, priorities with color-coded faces, and effort/energy properties. This is the foundation every later phase depends on — no capture templates, no agenda views, no visual polish.

</domain>

<decisions>
## Implementation Decisions

### TODO State Workflow
- 6 states in sequence: `TODO NEXT WAITING SOMEDAY | DONE CANCELLED`
- 4 active states (left of pipe), 2 closed states (right of pipe)
- Transitions are unrestricted — any state can move to any other state
- State changes log timestamps in LOGBOOK drawer
- WAITING transitions prompt for a note (who/what are you waiting on); all other transitions log timestamp only, no note prompt
- Fast selection keys: t=TODO, n=NEXT, w=WAITING, s=SOMEDAY, d=DONE, c=CANCELLED
- All state config must use `after! org` blocks to survive Doom Emacs overrides

### File Structure & Organization
- `~/org/gtd/` directory with: inbox.org, projects.org, someday.org, reference.org, archive/
- Projects in projects.org are flat — each project is a top-level heading with sub-tasks underneath (no area-of-life grouping)
- Each GTD file gets minimal boilerplate: `#+title` and a brief comment explaining its purpose
- Claude's Discretion: org-directory setting (~/org/ vs ~/org/gtd/) — pick what works best across all 8 phases
- Claude's Discretion: Archive strategy (single file vs per-source) — pick what works best with org-archive

### Context Tags & Tagging Conventions
- 6 GTD contexts (expanded from roadmap's 5): @home, @work, @errands, @phone, @computer, @email
- Context tags are NOT mutually exclusive — a task can have multiple contexts (e.g., @work + @computer)
- Tags inherit from parent headings — tag a project @work and all sub-tasks inherit it
- Fast-tag selection interface for quick tagging
- Claude's Discretion: Whether to add any additional non-context tag categories (e.g., energy level tags) — decide based on what's practical without overcomplicating the system

### Refile & Archive Behavior
- Refile targets include ALL GTD files: inbox.org, projects.org, someday.org, reference.org
- Max refile depth: 2 levels
- Refile completion shows file path hierarchy (e.g., "projects.org/Project A") for clarity
- Fuzzy completion for refile target selection
- Archiving: DONE/CANCELLED items auto-archive after 30 days
- Manual archiving also available for immediate cleanup

### Effort & Priority Properties
- Effort uses t-shirt sizes: XS, S, M, L, XL
- Priorities A/B/C with visually distinct color-coded faces (color choices are Claude's discretion)
- Effort is set via property, selectable from the t-shirt size list

### Claude's Discretion
- org-directory path choice (~/org/ vs ~/org/gtd/)
- Archive file strategy (single vs per-source)
- Additional tag categories beyond contexts (if any)
- Priority face colors
- Exact auto-archive implementation approach (org-crypt, custom function, etc.)
- LOGBOOK drawer formatting details

</decisions>

<specifics>
## Specific Ideas

No specific requirements — open to standard approaches. User wants a clean, classic GTD setup that follows established org-mode conventions within Doom Emacs.

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope.

</deferred>

---

*Phase: 01-gtd-foundation*
*Context gathered: 2026-02-24*
