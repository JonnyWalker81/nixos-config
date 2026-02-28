# Requirements: OrgLife

**Defined:** 2025-02-24
**Core Value:** Everything lives in one integrated system — tasks, schedule, notes, and journal are cross-linked and accessible within two keystrokes, so nothing falls through the cracks.

## v1 Requirements

Requirements for initial release. Each maps to roadmap phases.

### GTD Foundation

- [x] **GTD-01**: User has 6 TODO states (TODO, NEXT, WAITING, SOMEDAY | DONE, CANCELLED) with fast-access keys
- [x] **GTD-02**: State changes log timestamps into a LOGBOOK drawer
- [x] **GTD-03**: User can tag tasks with GTD contexts (@home, @work, @errands, @phone, @computer)
- [x] **GTD-04**: org-directory set with structured GTD files (inbox.org, projects.org, someday.org, reference.org, archive)
- [x] **GTD-05**: User can refile items from inbox to proper GTD files with fuzzy completion
- [x] **GTD-06**: User can assign priorities (A/B/C) with color-coded faces
- [x] **GTD-07**: User can set effort/energy properties on tasks for time-available filtering

### Capture

- [x] **CAP-01**: User can capture a quick TODO to inbox.org from anywhere in Emacs via global hotkey
- [x] **CAP-02**: User can capture a note/idea to inbox.org
- [x] **CAP-03**: User can capture a new project with sub-tasks to projects.org
- [x] **CAP-04**: User can capture meeting notes with attendees, date, notes, and action items
- [ ] **CAP-05**: User can capture a journal entry via org-capture integrated with org-journal

### Agenda

- [ ] **AGN-01**: User can view daily agenda with time grid showing today's schedule
- [ ] **AGN-02**: User can view weekly overview showing the week ahead
- [ ] **AGN-03**: Agenda items are grouped by org-super-agenda (priority, context, TODO state)
- [x] **AGN-04**: User has a GTD "Daily Review" block agenda (today + high priority + next actions + waiting + inbox count)
- [x] **AGN-05**: User has a "Weekly Review" block agenda (week + stuck projects + waiting + someday + unprocessed)
- [x] **AGN-06**: User can view context-filtered views (@home, @work tasks only)
- [x] **AGN-07**: User can detect stuck projects (projects with no NEXT action)

### Knowledge Base

- [x] **KB-01**: org-roam configured with separate roam directory, sqlite DB, and autosync
- [x] **KB-02**: User can find and open existing roam notes via fuzzy search
- [x] **KB-03**: User can insert links to roam notes while typing anywhere in org
- [x] **KB-04**: User can view backlinks buffer showing what links to current note
- [x] **KB-05**: User has org-roam capture templates (default, literature, concept notes)
- [x] **KB-06**: org-roam-ui serves interactive knowledge graph in browser
- [ ] **KB-07**: denote configured in separate directory for structured file-naming notes

### Journaling

- [ ] **JRN-01**: org-journal configured with per-day files and timestamps
- [ ] **JRN-02**: Unfinished journal TODOs auto-carry-over to next day's entry
- [ ] **JRN-03**: Journal TODOs appear in org-agenda
- [ ] **JRN-04**: User can search across all journal entries

### Visual

- [ ] **VIS-01**: org-modern-mode active for headlines, keywords, tables, timestamps
- [ ] **VIS-02**: org-modern styling applied to agenda buffer
- [ ] **VIS-03**: TODO keywords color-coded by state (NEXT=blue, WAITING=orange, etc.)
- [ ] **VIS-04**: Emphasis markers hidden for clean appearance
- [ ] **VIS-05**: Inline images and LaTeX fragments render in org buffers

### Integration & UX

- [ ] **UX-01**: User can link GTD tasks to org-roam knowledge notes
- [ ] **UX-02**: User can link journal entries to tasks and projects
- [ ] **UX-03**: Custom SPC-based keybindings for all org-life workflows (2 keystrokes max)
- [ ] **UX-04**: Custom startup dashboard showing today's agenda, pending tasks, and upcoming deadlines

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### Habits

- **HAB-01**: User can track daily habits with org-habit
- **HAB-02**: Habit consistency graphs visible in agenda

### Calendar Sync

- **CAL-01**: User can sync Google Calendar events to org-agenda via org-gcal
- **CAL-02**: User can view external calendar events alongside org schedule

### Extended Capture

- **EXT-01**: User can capture from browser via org-protocol
- **EXT-02**: User can capture web article links with org-cliplink

### Time Tracking

- **CLK-01**: User can clock in/out on tasks with org-clock
- **CLK-02**: User can generate time reports

### Finance

- **FIN-01**: User can track expenses in org tables or ledger
- **FIN-02**: User can view budget summaries

### Reading Log

- **READ-01**: User can track books with status (to-read, reading, finished)
- **READ-02**: User can capture article highlights and annotations

### Contacts

- **CRM-01**: User can maintain contact records with notes
- **CRM-02**: User can set follow-up reminders for contacts

## Out of Scope

Explicitly excluded. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| Over-engineering TODO states (>7 states) | Creates decision fatigue, slows processing. 6 states covers full GTD. |
| Auto-scheduling all tasks | Agenda becomes wall of overdue red. Use SCHEDULED sparingly. |
| 20+ capture templates | Can't remember keys. Start with 5, add only when friction appears. |
| Syncing org files via Dropbox/Syncthing | Merge conflicts are catastrophic. Binary sqlite DB can't merge. |
| Custom org-agenda sorting functions | org-super-agenda grouping handles 95% of cases with less complexity. |
| org-roam for every thought | Database bloat, meaningless graph. Use for durable concepts only. |
| Complex weekly review automation | Fragile elisp. The review IS the thinking, not the output. |
| Mobile app / access | Web-first desktop Emacs only. Out of scope entirely. |

## Traceability

Which phases cover which requirements. Updated during roadmap creation.

| Requirement | Phase | Status |
|-------------|-------|--------|
| GTD-01 | Phase 1: GTD Foundation | Complete |
| GTD-02 | Phase 1: GTD Foundation | Complete |
| GTD-03 | Phase 1: GTD Foundation | Complete |
| GTD-04 | Phase 1: GTD Foundation | Complete |
| GTD-05 | Phase 1: GTD Foundation | Complete |
| GTD-06 | Phase 1: GTD Foundation | Complete |
| GTD-07 | Phase 1: GTD Foundation | Complete |
| CAP-01 | Phase 2: Capture Workflow | Complete |
| CAP-02 | Phase 2: Capture Workflow | Complete |
| CAP-03 | Phase 2: Capture Workflow | Complete |
| CAP-04 | Phase 2: Capture Workflow | Complete |
| CAP-05 | Phase 6: Journaling & Denote | Pending |
| AGN-01 | Phase 3: Basic Agenda | Pending |
| AGN-02 | Phase 3: Basic Agenda | Pending |
| AGN-03 | Phase 3: Basic Agenda | Pending |
| AGN-04 | Phase 4: Advanced Agenda & GTD Reviews | Complete |
| AGN-05 | Phase 4: Advanced Agenda & GTD Reviews | Complete |
| AGN-06 | Phase 4: Advanced Agenda & GTD Reviews | Complete |
| AGN-07 | Phase 4: Advanced Agenda & GTD Reviews | Complete |
| KB-01 | Phase 5: Knowledge Base | Complete |
| KB-02 | Phase 5: Knowledge Base | Complete |
| KB-03 | Phase 5: Knowledge Base | Complete |
| KB-04 | Phase 5: Knowledge Base | Complete |
| KB-05 | Phase 5: Knowledge Base | Complete |
| KB-06 | Phase 5: Knowledge Base | Complete |
| KB-07 | Phase 6: Journaling & Denote | Pending |
| JRN-01 | Phase 6: Journaling & Denote | Pending |
| JRN-02 | Phase 6: Journaling & Denote | Pending |
| JRN-03 | Phase 6: Journaling & Denote | Pending |
| JRN-04 | Phase 6: Journaling & Denote | Pending |
| VIS-01 | Phase 7: Visual Polish | Pending |
| VIS-02 | Phase 7: Visual Polish | Pending |
| VIS-03 | Phase 7: Visual Polish | Pending |
| VIS-04 | Phase 7: Visual Polish | Pending |
| VIS-05 | Phase 7: Visual Polish | Pending |
| UX-01 | Phase 8: Integration & Dashboard | Pending |
| UX-02 | Phase 8: Integration & Dashboard | Pending |
| UX-03 | Phase 8: Integration & Dashboard | Pending |
| UX-04 | Phase 8: Integration & Dashboard | Pending |

**Coverage:**
- v1 requirements: 39 total
- Mapped to phases: 39
- Unmapped: 0
- Coverage: 100% ✓

**Note:** Original count said 36 but actual enumeration yields 39 requirements (7+5+7+7+4+5+4). All 39 mapped.

---
*Requirements defined: 2025-02-24*
*Last updated: 2026-02-28 — Phase 5 requirements (KB-01 through KB-06) marked Complete*
