# Phase 2: Capture Workflow - Context

**Gathered:** 2026-02-25
**Status:** Ready for planning

<domain>
## Phase Boundary

Define capture behavior and template content so the user can capture tasks, notes, projects, and meetings from anywhere in Emacs with minimal friction, using Phase 2 scope only.

</domain>

<decisions>
## Implementation Decisions

### Capture menu flow
- Capture starts with a context-aware default template instead of always forcing the full menu first.
- Template choices should prioritize mnemonic wording for speed and recognition.
- Quick capture should mostly auto-save with minimal interruption.
- After save, return immediately to prior context by default.

### Quick task/note capture
- Inbox task capture requires task title plus one context tag at capture time.
- Note/idea capture should be a timestamped idea entry.
- Prompt depth is mixed by template (different prompt behavior for task vs note).
- Inbox entries should go to template-specific sections in `inbox.org`.

### Project template shape
- New projects are captured as a heading with sections (not a bare heading).
- Do not insert starter actions at creation time; actions are added later.
- Capture project name plus deadline during project capture.
- New projects are stored as top-level entries in `projects.org`.

### Meeting template shape
- Meeting capture uses a heading with subsections.
- Attendees are optional at capture time.
- Action items use a mixed model: TODO sub-items with optional inbox linkage.

### Claude's Discretion
- Choose final destination placement for meeting entries (user delegated this decision).

</decisions>

<specifics>
## Specific Ideas

- Prioritize minimal friction and fast return-to-flow behavior after capture.

</specifics>

<deferred>
## Deferred Ideas

None - discussion stayed within phase scope.

</deferred>

---

*Phase: 02-capture-workflow*
*Context gathered: 2026-02-25*
