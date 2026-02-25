# Feature Research: Org-mode Life Management System

**Domain:** Personal knowledge management + GTD task management in Doom Emacs
**Researched:** 2026-02-24
**Confidence:** HIGH (org-mode manual, org-roam manual, org-super-agenda README, org-journal README, org-modern README)

## Feature Landscape

### Table Stakes (Users Expect These)

Features that any "comprehensive org-mode life management system" must have. Missing these and the system feels half-built, not worth switching to.

#### 1. GTD TODO States and Workflow

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Custom TODO keyword sequence | GTD requires states beyond TODO/DONE | LOW | `org-todo-keywords` with `(sequence "TODO" "NEXT" "WAITING" "SOMEDAY" "|" "DONE" "CANCELLED")` |
| Fast-access keys for states | Switching states must be instant (single keypress) | LOW | Use `(sequence "TODO(t)" "NEXT(n)" ...)` syntax for fast access via `C-c C-t` |
| Timestamp on state changes | Must know when things changed for weekly review | LOW | `org-log-into-drawer t`, `org-treat-insert-todo-heading-as-state-change t` |
| Priority levels (A/B/C) | Triage is fundamental to GTD | LOW | Built-in, just needs `org-priority-faces` for color coding |
| Tags for GTD contexts | `@home`, `@work`, `@errands`, `@phone`, `@computer` | LOW | Standard org tags, set via `org-tag-alist` |
| Properties for effort/energy | Effort estimates for time-available filtering | LOW | `org-global-properties` with `Effort_ALL` values |

**Confidence:** HIGH - Verified from org-mode manual (TODO Extensions, Workflow states, Tags sections).

**Standard GTD TODO states from community consensus:**
```
(sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "WAITING(w@/!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELLED(c@)")
```

The `@` means prompt for a note on entry, `!` means log timestamp. The `|` separates active states from done states. This is the most common pattern across GTD org-mode setups.

**Standard context tags:**
```elisp
(setq org-tag-alist
      '(("@home" . ?h) ("@work" . ?w) ("@errands" . ?e)
        ("@phone" . ?p) ("@computer" . ?c)
        (:newline)
        ("project" . ?P) ("someday" . ?S) ("urgent" . ?u)))
```

#### 2. Structured Org Directory and Agenda Files

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Explicit `org-directory` | System needs a home | LOW | `(setq org-directory "~/org/")` |
| GTD inbox file | Everything starts in inbox | LOW | `~/org/inbox.org` |
| Projects file | Active projects with sub-tasks | LOW | `~/org/projects.org` |
| Someday/maybe file | GTD deferred items | LOW | `~/org/someday.org` |
| Reference file | Non-actionable reference material | LOW | `~/org/reference.org` |
| Archive mechanism | Completed items leave active files | LOW | `org-archive-location` per-file or global |
| `org-agenda-files` properly set | Agenda must scan the right files | LOW | List of GTD files, exclude roam/journal |

**Confidence:** HIGH - Standard GTD file structure from org-mode manual and community patterns.

**Recommended directory structure:**
```
~/org/
  inbox.org         # Capture landing zone
  todo.org          # Next actions / standalone tasks
  projects.org      # Multi-step projects with sub-tasks
  someday.org       # Someday/maybe items
  reference.org     # Non-actionable reference
  archive.org       # Archived done items
  work.org          # Work-specific tasks (existing file)
  roam/             # org-roam knowledge base
  journal/          # org-journal daily entries
```

#### 3. Capture Templates (Essential Set)

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Inbox capture (quick TODO) | GTD: everything goes to inbox first | LOW | `"t"` key, entry to `inbox.org` |
| Note capture | Quick thought/idea | LOW | `"n"` key, entry to `inbox.org` or roam |
| Project capture | New project with sub-tasks | MEDIUM | `"p"` key, entry to `projects.org` |
| Meeting notes capture | Timestamped meeting record | MEDIUM | `"m"` key, with attendees, date |
| Journal entry capture | Quick journal via org-capture | LOW | `"j"` key, integrates with org-journal |

**Confidence:** HIGH - Verified from org-mode manual (Capture templates section) and org-journal README (Journal Capture Template section).

**Standard capture template pattern:**
```elisp
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %?\n%U\n%a" :empty-lines 1)
        ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
         "* %? :note:\n%U" :empty-lines 1)
        ("m" "Meeting" entry (file+headline "~/org/inbox.org" "Meetings")
         "* MEETING %? :meeting:\nSCHEDULED: %T\n** Attendees\n** Notes\n** Action Items"
         :empty-lines 1)
        ("p" "Project" entry (file+headline "~/org/projects.org" "Projects")
         "* TODO %? [/] :project:\n** TODO " :empty-lines 1)))
```

#### 4. Basic Agenda Views

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Daily agenda view | See today's schedule | LOW | Default `org-agenda-list` |
| Weekly agenda view | See the week ahead | LOW | Built-in with `org-agenda-span 'week` |
| All TODOs view | See everything open | LOW | Built-in `org-todo-list` |
| Stuck projects view | Projects with no NEXT action | MEDIUM | Built-in via `org-stuck-projects` |

**Confidence:** HIGH - These are built-in org-agenda views documented in the manual.

#### 5. org-super-agenda Grouped Views

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| Enable org-super-agenda-mode | Package is installed but unused | LOW | One line: `(org-super-agenda-mode)` |
| Group by priority | A/B/C items separated visually | LOW | `:priority` selector |
| Group by context tags | See `@work` vs `@home` items | LOW | `:tag` selector |
| Time grid grouping | Today's scheduled items prominent | LOW | `:time-grid t` selector |
| Custom group ordering | Important groups first | MEDIUM | `:order` parameter |

**Confidence:** HIGH - Verified from org-super-agenda README with complete selector documentation.

**Recommended super-agenda config:**
```elisp
(setq org-super-agenda-groups
      '((:name "Today" :time-grid t :scheduled today)
        (:name "Due Soon" :deadline future)
        (:name "Important" :priority "A")
        (:name "Next Actions" :todo "NEXT")
        (:name "Waiting" :todo "WAITING")
        (:name "Projects" :tag "project")
        (:name "Someday/Maybe" :todo "SOMEDAY" :order 9)))
```

#### 6. Visual Polish

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| org-modern-mode | Modern styling for headlines, keywords, tables, timestamps | LOW | `(global-org-modern-mode)` or hook per buffer |
| org-modern for agenda | Styled agenda buffer | LOW | `(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)` |
| Color-coded TODO keywords | Visual distinction between states | LOW | `org-todo-keyword-faces` alist |
| Color-coded priorities | A=red, B=yellow, C=green convention | LOW | `org-priority-faces` alist |
| Hidden emphasis markers | Clean look without `*bold*` showing `*` | LOW | `org-hide-emphasis-markers t` |
| Pretty entities | UTF-8 symbols for LaTeX fragments | LOW | `org-pretty-entities t` |

**Confidence:** HIGH - Verified from org-modern README (configuration section). org-modern is the modern replacement for both org-superstar and org-bullets (stated in README: "org-modern is a full replacement for both org-superstar and org-bullets").

#### 7. SPC-based Keybindings

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| SPC m prefix for org-mode | Doom's localleader pattern | LOW | Already exists in Doom |
| SPC n prefix for notes/org | Standard Doom notes prefix | LOW | Doom provides `SPC n` |
| SPC o a for agenda | Quick agenda access | LOW | Doom default binding |
| SPC X for capture | Quick capture from anywhere | LOW | Doom default binding |
| Custom SPC o prefix for org life | Unified prefix for all org-life commands | MEDIUM | New keybinding group |

**Confidence:** HIGH - Doom Emacs conventions are well-established. Doom already provides `SPC n` (notes), `SPC o a` (agenda), `SPC X` (capture).

### Differentiators (Competitive Advantage)

Features that make this setup go from "functional GTD" to "system I actually want to live in." Not expected from a basic setup, but what separates a great org-mode config from a mediocre one.

#### 1. org-roam Knowledge Base

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| org-roam core setup | Obsidian-like backlinks, per-note files, graph DB | MEDIUM | Requires sqlite, `org-roam-directory`, `org-roam-db-autosync-mode` |
| org-roam node find/insert | Find existing notes, insert links contextually | LOW | `org-roam-node-find`, `org-roam-node-insert` |
| org-roam backlinks buffer | See what links to current note | LOW | `org-roam-buffer-toggle`, configure display |
| org-roam capture templates | Different templates for permanent notes, literature notes | MEDIUM | `org-roam-capture-templates` with multiple template types |
| Completion everywhere | Link to notes while typing anywhere in org | LOW | `(setq org-roam-completion-everywhere t)` |
| Tags/aliases on nodes | Find notes by tag or alternative name | LOW | `#+filetags:` and `ROAM_ALIASES` property |

**Confidence:** HIGH - Verified from org-roam manual (Getting Started, Templating System, Node Properties, Completion sections).

**Recommended org-roam capture templates (Zettelkasten method from manual):**
```elisp
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: \n")
         :unnarrowed t)
        ("l" "literature" plain "%?"
         :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :literature:\n* Source\n* Notes\n")
         :unnarrowed t)
        ("c" "concept" plain "%?"
         :target (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :concept:\n")
         :unnarrowed t)))
```

#### 2. org-roam-ui Graph Visualization

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| org-roam-ui setup | Interactive 3D knowledge graph in browser | MEDIUM | Requires websocket, simple-httpd packages |
| Follow mode | Graph follows cursor in Emacs | LOW | `org-roam-ui-follow t` |
| Theme sync | Graph matches Emacs theme | LOW | `org-roam-ui-sync-theme t` |
| Local graph view | Focus on neighborhood of current note | LOW | `org-roam-ui-node-local` command |

**Confidence:** HIGH - Verified from org-roam-ui README. Requires `websocket`, `simple-httpd`, `f` packages. For Doom: unpin org-roam, add `(package! org-roam-ui)`.

#### 3. org-journal Daily Journaling

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| org-journal core setup | Per-day journal files with timestamps | LOW | Set `org-journal-dir`, `org-journal-date-format`, `org-journal-file-type` |
| TODO carry-over | Unfinished TODOs auto-move to next day | LOW | `org-journal-carryover-items` (default: `TODO="TODO"`) |
| Calendar integration | Journal entries visible in Emacs calendar | LOW | Built-in, entries marked on calendar |
| Journal search | Search across all journal entries | LOW | `org-journal-search` (C-c C-s in journal mode) |
| Agenda integration | Current/future journal TODOs in agenda | LOW | `org-journal-enable-agenda-integration t` |

**Confidence:** HIGH - Verified from org-journal README (Synopsis, Setup and customization, Advanced Usage sections).

#### 4. Advanced Custom Agenda Views

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| GTD "Daily Review" view | Block agenda: today's schedule + next actions + inbox count | HIGH | `org-agenda-custom-commands` with multiple blocks |
| Context-filtered views | "What can I do @home right now?" | MEDIUM | Tags-todo view filtered by context |
| Waiting-for review | All delegated/waiting items with timestamps | MEDIUM | Tags-todo with WAITING state |
| Stuck projects detector | Projects that need a NEXT action | MEDIUM | `org-stuck-projects` configuration |
| Weekly review checklist | Guided weekly review workflow | HIGH | Custom agenda + elisp helper functions |

**Confidence:** MEDIUM - The building blocks are HIGH confidence (org-agenda-custom-commands from manual), but specific view combinations are community patterns not from official docs.

**Recommended custom commands structure:**
```elisp
(setq org-agenda-custom-commands
      '(("d" "Daily Review"
         ((agenda "" ((org-agenda-span 'day)))
          (tags-todo "PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority")))
          (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
          (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))
          (tags-todo "+inbox" ((org-agenda-overriding-header "Inbox - Process These!")))))
        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-span 'week)))
          (stuck "" nil)
          (todo "WAITING" nil)
          (todo "SOMEDAY" nil)
          (todo "TODO" ((org-agenda-overriding-header "Unprocessed Inbox")
                        (org-agenda-files '("~/org/inbox.org"))))))
        ("c" "Contexts"
         ((tags-todo "@work" ((org-agenda-overriding-header "Work")))
          (tags-todo "@home" ((org-agenda-overriding-header "Home")))
          (tags-todo "@errands" ((org-agenda-overriding-header "Errands")))
          (tags-todo "@phone" ((org-agenda-overriding-header "Phone Calls")))))))
```

#### 5. denote Integration (Alongside org-roam)

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| denote basic config | Simple file-naming convention notes, no DB needed | LOW | Already installed, needs `denote-directory`, `denote-known-keywords` |
| denote for structured notes | Meeting notes, project docs that don't need graph linking | LOW | `denote-create`, `denote-open-or-create` |
| Separation of concerns | org-roam for interconnected knowledge, denote for structured docs | LOW | Different directories, different use cases |

**Confidence:** MEDIUM - denote is already installed in packages.el but configuration patterns are based on training data, not verified from current docs.

#### 6. Cross-linking Between Systems

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| Link from task to roam note | "Research X" TODO links to knowledge note | LOW | Standard org-id links between files |
| Link from journal to task | Journal reflects on task progress | LOW | `org-store-link` + `org-insert-link` |
| Capture to roam from agenda | Turn agenda item into knowledge note | MEDIUM | Custom capture template or function |

**Confidence:** MEDIUM - The mechanisms exist (org-id links are how org-roam works per the manual), but specific integration patterns are community-derived.

#### 7. Refile Workflow

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| `org-refile-targets` config | Move items from inbox to proper GTD files | MEDIUM | Set refile targets to all agenda files, depth 2-3 |
| Refile with completion | Fuzzy-find target heading | LOW | Works with vertico (already configured) |
| Refile-to-datetree | Move items to date-organized archives | LOW | `org-refile-targets` with datetree |

**Confidence:** HIGH - org-refile is core org-mode functionality documented in the manual.

### Anti-Features (Commonly Requested, Often Problematic)

Features that seem good but create problems. Things to deliberately NOT build.

| Feature | Why Requested | Why Problematic | Alternative |
|---------|---------------|-----------------|-------------|
| Over-engineered TODO states | "I need 15 different states!" | Creates decision fatigue, slows processing. GTD works with 5-7 states max. Items sit in weird states forever. | Stick to TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELLED. Add more only after using the system for weeks. |
| Auto-scheduling everything | "Schedule all my TODOs for specific days" | Agenda becomes a wall of overdue red. You spend time rescheduling instead of doing. | Use SCHEDULED sparingly (only time-bound). Use NEXT for "do soon." Use DEADLINE for hard deadlines. |
| Too many capture templates | "I need templates for 20 different situations" | Can't remember which key does what. Capture becomes slower than just typing. | Start with 4-5 templates. Add more only when you notice friction. |
| Syncing org files via Dropbox/Syncthing | "I want my org on all devices" | Merge conflicts with org files are catastrophic. Binary sqlite DB for org-roam can't merge. | Git for the org directory. Or just accept desktop-only for v1. |
| Custom org-agenda sorting functions | "I want items sorted by 5 criteria" | Elisp complexity explodes, debugging is painful, and priorities + super-agenda groups handle 95% of cases. | Use org-super-agenda grouping instead of custom sort. |
| org-roam for everything | "Every thought should be a roam node" | Database bloat, meaningless graph, slow queries. Fleeting notes clog the permanent knowledge base. | Use org-roam for durable concepts only. Use org-journal/dailies for fleeting notes. Inbox for tasks. |
| Effort/clocking on all tasks | "Track time on everything" | Overhead of clocking in/out kills flow. Most tasks don't need time tracking. | Only clock when billing or analyzing time. Don't set up org-clock until you need it. |
| Complex weekly review automation | "The weekly review should auto-generate reports" | Fragile elisp, hard to maintain, doesn't adapt to your evolving workflow. The review IS the thinking, not the output. | Use a simple checklist + agenda views. The review is a process, not a report. |
| Habit tracking in v1 | "I want to track habits from day one" | org-habit adds visual noise to agenda, interacts poorly with super-agenda grouping, and is a separate workflow to learn. | Defer to v2 per project scope. Add after core GTD is working. |
| Calendar sync in v1 | "I need Google Calendar events in my agenda" | org-gcal is fragile (OAuth tokens expire, API rate limits, JSON parsing issues). Debugging sync issues wastes hours. | Defer to v2 per project scope. org-gcal is already installed for later. |

## Feature Dependencies

```
[org-directory structure]
    |
    +--requires--> [Capture Templates] (need files to capture into)
    |                  |
    |                  +--requires--> [Refile Targets] (need structure to refile into)
    |
    +--requires--> [TODO States] (needed before agenda views make sense)
    |                  |
    |                  +--requires--> [Tags/Contexts] (contextualize TODO items)
    |
    +--requires--> [org-agenda-files] (tell agenda where to look)
                       |
                       +--requires--> [Basic Agenda Views]
                       |                  |
                       |                  +--requires--> [org-super-agenda Groups]
                       |                  |
                       |                  +--requires--> [Custom Agenda Commands]
                       |
                       +--enhances--> [Stuck Projects View]

[org-roam setup]
    |
    +--requires--> [org-roam-directory] (separate from GTD files)
    |
    +--requires--> [sqlite system package]
    |
    +--requires--> [org-roam-db-autosync-mode]
    |
    +--enhances--> [org-roam-ui] (depends on org-roam being configured)
    |
    +--enhances--> [org-roam capture templates]

[org-journal setup]
    |
    +--independent of org-roam and GTD
    |
    +--enhances--> [Agenda] (via org-journal-enable-agenda-integration)

[org-modern / Visual polish]
    |
    +--independent, can be added at any point
    |
    +--enhances--> [All org buffers + agenda]

[denote setup]
    |
    +--independent of org-roam (uses different directory)
    |
    +--enhances--> [Note organization for structured docs]

[Cross-linking]
    |
    +--requires--> [GTD setup complete]
    +--requires--> [org-roam setup complete]
    +--requires--> [org-journal setup complete]
```

### Dependency Notes

- **GTD structure must come first:** Capture templates, agenda views, and super-agenda all depend on having files, TODO states, and tags defined
- **org-roam is independent of GTD:** Can be set up in parallel but should be in a separate directory
- **org-journal is independent:** Can be added at any phase
- **Visual polish is independent:** org-modern can be added first or last without affecting anything
- **org-super-agenda requires basic agenda:** Must have working `org-agenda-files` and TODO states before grouping makes sense
- **Custom agenda commands require everything:** These are the capstone feature that ties GTD + super-agenda + tags together

## MVP Definition

### Launch With (v1 - Phase 1: Foundation)

Minimum to make the system functional and worth using daily.

- [x] org-directory and GTD file structure created
- [x] TODO keyword sequence (TODO, NEXT, WAITING, SOMEDAY | DONE, CANCELLED)
- [x] Context tags (@home, @work, @errands, @phone, @computer)
- [x] 4-5 essential capture templates (inbox, note, meeting, project)
- [x] org-agenda-files configured
- [x] org-super-agenda-mode enabled with basic grouping
- [x] Refile targets configured
- [x] org-modern-mode for visual polish

**Why essential:** Without these, you have no GTD system. You can't capture, can't review, can't process.

### Add After Validation (v1.x - Phase 2-3: Enrichment)

- [x] org-roam knowledge base with capture templates -- after GTD workflow is comfortable
- [x] org-roam-ui graph visualization -- after roam has enough nodes to visualize
- [x] org-journal daily journaling -- after capture workflow is habitual
- [x] Custom agenda views (daily review, weekly review, contexts) -- after basic agenda is used daily
- [x] denote for structured notes -- after roam is established, add for meeting notes/project docs
- [x] Cross-linking between GTD tasks and roam notes -- after both systems have content
- [x] Advanced SPC keybindings for org-life -- after knowing which commands you use most

### Future Consideration (v2+)

Explicitly deferred per project scope.

- [ ] org-habit tracking -- wait until daily GTD review is habitual
- [ ] org-gcal calendar sync -- wait until workflow is solid, then add external calendar
- [ ] org-protocol browser capture -- wait until in-Emacs capture is fully habitual
- [ ] Finance/budgeting (org-ledger or hledger) -- separate domain entirely
- [ ] Reading/media log -- separate workflow
- [ ] Contacts/CRM -- separate workflow
- [ ] Mobile access -- out of scope, desktop only
- [ ] org-clock time tracking -- only add when needed for billing/analysis

## Feature Prioritization Matrix

| Feature | User Value | Implementation Cost | Priority |
|---------|------------|---------------------|----------|
| GTD TODO states + tags | HIGH | LOW | P1 |
| org-directory + GTD files | HIGH | LOW | P1 |
| Capture templates (core 4-5) | HIGH | LOW | P1 |
| org-agenda-files config | HIGH | LOW | P1 |
| org-super-agenda grouping | HIGH | LOW | P1 |
| org-modern visual polish | MEDIUM | LOW | P1 |
| Refile targets | HIGH | LOW | P1 |
| Color-coded TODOs/priorities | MEDIUM | LOW | P1 |
| org-roam core setup | HIGH | MEDIUM | P2 |
| org-roam capture templates | HIGH | MEDIUM | P2 |
| org-roam-ui | MEDIUM | MEDIUM | P2 |
| org-journal setup | MEDIUM | LOW | P2 |
| Custom daily review agenda | HIGH | MEDIUM | P2 |
| Weekly review workflow | HIGH | HIGH | P2 |
| Context-filtered views | MEDIUM | MEDIUM | P2 |
| denote configuration | LOW | LOW | P3 |
| Cross-linking between systems | MEDIUM | MEDIUM | P3 |
| Advanced SPC keybindings | MEDIUM | MEDIUM | P3 |
| org-roam dailies (fleeting notes) | LOW | LOW | P3 |
| Stuck projects view | MEDIUM | MEDIUM | P3 |

**Priority key:**
- P1: Must have for launch -- the system is broken without these
- P2: Should have, add when core is working -- these make it great
- P3: Nice to have, add for polish -- these round out the experience

## Comparable Setup Analysis

| Feature | Typical "Basic" Setup | Power User Setup | Our Approach |
|---------|----------------------|------------------|--------------|
| TODO states | TODO/DONE | 5-7 states with fast-access keys, logging | 6 states (TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELLED) with timestamp logging |
| Capture | 1-2 templates | 6-10 domain-specific templates | 5 core templates, add more as needed |
| Agenda | Default agenda | Block agenda with super-agenda groups | Super-agenda grouped daily + custom weekly review |
| Notes | Flat org files | org-roam with backlinks + graph | org-roam for knowledge, denote for structured docs |
| Journal | Datetree in one file | org-journal with per-day files | org-journal with carry-over and agenda integration |
| Visual | Default org-mode | org-modern + theme-matched faces | org-modern + color-coded TODOs/priorities |
| Keybindings | Doom defaults | Custom SPC prefix tree | Doom defaults + custom `SPC o` org-life prefix |
| File structure | ~/org with random files | Structured GTD + roam + journal dirs | Hybrid: GTD files at root, roam/ and journal/ subdirs |
| Review | Manual | Guided weekly review agenda view | Custom agenda command + checklist |

## Sources

- Org-mode manual: TODO Items, TODO Extensions, Workflow States, Capture Templates, Custom Agenda Views (orgmode.org/manual/) - **HIGH confidence**
- org-roam manual v2.3.1-devel: Getting Started, Zettelkasten Method, Templating System, Node Properties, Extensions/Dailies (orgroam.com/manual.html) - **HIGH confidence**
- org-super-agenda README: Group selectors, examples, usage patterns (github.com/alphapapa/org-super-agenda) - **HIGH confidence**
- org-journal README: Synopsis, setup, carry-over, agenda integration, capture template (github.com/bastibe/org-journal) - **HIGH confidence**
- org-modern README: Configuration, alternatives, incompatibilities (github.com/minad/org-modern) - **HIGH confidence**
- org-roam-ui README: Installation for Doom, configuration, features (github.com/org-roam/org-roam-ui) - **HIGH confidence**
- Existing codebase: config-org.el, packages.el, init.el analyzed for current state - **HIGH confidence**

---
*Feature research for: Org-mode Life Management System*
*Researched: 2026-02-24*
