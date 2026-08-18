# Technology Stack: Org-mode Life Management System

**Project:** OrgLife -- Comprehensive Org-mode Life Management in Doom Emacs
**Researched:** 2026-02-24
**Mode:** Ecosystem (Stack dimension)
**Overall Confidence:** HIGH

---

## Executive Summary

The Org-mode life management ecosystem is mature and well-integrated with Doom Emacs. Most critical packages (org-roam, org-journal, org-modern, org-appear) are **already bundled as Doom module flags** -- meaning installation is a single flag in `init.el` rather than manual `packages.el` declarations. The user's existing `+pretty` flag already provides org-modern and org-appear. Adding `+roam` and `+journal` flags covers the two biggest missing pieces. The remaining work is configuration, not installation.

Key insight: **Doom Emacs's org module is far more batteries-included than most users realize.** The `+pretty` flag already installs `org-modern` and `org-appear`. The `+roam` flag installs `org-roam`. The `+journal` flag installs `org-journal`. The base module already includes `org-cliplink`. This means the packages.el changes are minimal -- mostly `org-roam-ui` and `org-super-agenda` (already installed).

---

## Recommended Stack

### Doom Emacs init.el Module Flags

**This is the primary "installation" mechanism.** Change the org module line in `init.el`:

| Current | Recommended | What It Adds |
|---------|-------------|--------------|
| `(org +pretty +attach +babel +capture +export +present)` | `(org +pretty +attach +babel +capture +export +present +roam +journal)` | org-roam, org-journal |

**Confidence: HIGH** -- Verified from Doom's official `modules/lang/org/README.org` (fetched 2026-02-24).

**What each flag provides (verified from Doom source):**
- `+pretty` -> `org-modern` + `org-appear` (already enabled!)
- `+roam` -> `org-roam` (v2, requires sqlite)
- `+journal` -> `org-journal`
- `+capture` -> org-capture infrastructure (already enabled)

### Core Packages (via Doom Module Flags)

| Package | Version | Source | Purpose | Confidence |
|---------|---------|--------|---------|------------|
| **org-roam** | v2.3.1 (latest, Jun 2025) | Doom `+roam` flag | Zettelkasten knowledge base with backlinks, per-note files, sqlite-backed graph | HIGH -- GitHub release page verified |
| **org-journal** | Latest MELPA (1.4k stars) | Doom `+journal` flag | Daily/weekly/monthly journaling with per-file entries, agenda integration, carry-over | HIGH -- GitHub README verified |
| **org-modern** | Latest (1.9k stars, GNU ELPA) | Doom `+pretty` flag (already active) | Modern visual styling: pretty headlines, keywords, tables, source blocks, tags | HIGH -- GitHub README verified |
| **org-appear** | v0.3.1 (Jul 2024) | Doom `+pretty` flag (already active) | Auto-reveal hidden markup (emphasis, links, entities) when cursor enters element | HIGH -- GitHub release verified |
| **org-cliplink** | Latest MELPA | Doom base org module (already included!) | Insert org-mode links from clipboard with auto-fetched page title | HIGH -- Doom README lists it as base package |

### Packages via packages.el (Manual Addition)

| Package | Version | Declaration | Purpose | Why Not a Flag |
|---------|---------|-------------|---------|----------------|
| **org-roam-ui** | Latest MELPA (2.2k stars) | `(unpin! org-roam)` + `(package! org-roam-ui)` | Interactive web-based knowledge graph (3D/2D, theme sync, follow mode) | Not bundled in Doom's `+roam` flag |
| **org-super-agenda** | v1.3 (Sep 2023, 1.5k stars) | Already in packages.el! | Group agenda items into sections by TODO, priority, tag, time-grid, etc. | Not a Doom flag, but already installed |
| **denote** | Latest MELPA | Already in packages.el! | Simple file-naming-convention notes alongside org-roam | Already installed, just needs config |

### Packages via packages.el (New Additions -- Nice-to-Have)

| Package | Declaration | Purpose | Priority | Confidence |
|---------|-------------|---------|----------|------------|
| **org-fancy-priorities** | `(package! org-fancy-priorities)` | Replace priority cookies [#A] with icons/emoji | LOW -- defer to v2 | MEDIUM -- org-modern already prettifies priorities |
| **websocket** | Doom dependency (auto-installed) | Required by org-roam-ui | N/A -- auto | HIGH |
| **simple-httpd** | Doom dependency (auto-installed) | Required by org-roam-ui | N/A -- auto | HIGH |

### NixOS System Packages

| Package | Where to Add | Purpose | Why |
|---------|-------------|---------|-----|
| **sqlite** | `users/common/packages.nix` or `machines/vm-shared.nix` | Required by org-roam's emacsql-sqlite | org-roam stores its database in SQLite; Emacs must be compiled with sqlite support OR have sqlite available at runtime |
| **graphviz** | `users/common/packages.nix` | Optional: org-roam graph visualization via `org-roam-graph` | Renders DOT graphs; not strictly needed if using org-roam-ui instead |

**CRITICAL NixOS Note:** Doom's `+roam` flag README states: "org-roam requires Emacs to be built with sqlite support." On NixOS, the Emacs package from nixpkgs 25.05 should include sqlite support by default (Emacs 29+ has built-in sqlite3 via `sqlite3-api`). **Verify with `(sqlite-available-p)` in Emacs.** If it returns `nil`, you need to ensure the Emacs build includes `--with-sqlite3`. This is the #1 potential blocker.

**Confidence: MEDIUM** -- The NixOS Emacs package *should* include sqlite, but this needs runtime verification.

---

## Alternatives Considered

### Visual Polish: org-modern vs org-superstar

| Criterion | org-modern (RECOMMENDED) | org-superstar | org-bullets |
|-----------|-------------------------|---------------|-------------|
| **Scope** | Headlines, keywords, tables, src blocks, tags, priorities, timestamps | Headlines, lists only | Headlines only |
| **Technique** | Text properties (efficient, future-proof) | Character composition | Character composition |
| **Maintainer** | Daniel Mendler (minad) -- extremely active, also maintains corfu, vertico, etc. | integral-dw -- less active | sabof -- archived/unmaintained |
| **Stars** | 1.9k | 780 | 1.3k (but archived) |
| **Doom integration** | Bundled with `+pretty` flag | Not bundled | Not bundled |
| **Agenda support** | Yes (`org-modern-agenda`) | No | No |
| **Recommendation** | **USE THIS** | Don't use | Don't use |

**Verdict: org-modern.** It's already installed via `+pretty`. It's maintained by one of the most prolific Emacs package authors. It covers far more than org-superstar. org-superstar is a subset of what org-modern does. org-bullets is archived. Don't install any alternatives.

**Confidence: HIGH** -- Verified from all three GitHub repos and Doom's `+pretty` flag source.

### Knowledge Base: org-roam vs denote (Coexistence)

| Criterion | org-roam | denote |
|-----------|----------|--------|
| **Purpose** | Zettelkasten with backlinks, graph, sqlite index | Simple file-naming-convention notes |
| **Complexity** | Higher (sqlite, ID properties, sync) | Very low (just file naming) |
| **Graph** | Yes (org-roam-ui) | No |
| **Backlinks** | Yes (automatic, buffer display) | Manual (via grep/consult) |
| **Best for** | Interconnected knowledge, research, evergreen notes | Quick standalone notes, meeting notes, fleeting thoughts |

**Verdict: Use BOTH.** They serve complementary purposes and don't conflict. org-roam for the interconnected knowledge graph. denote for quick notes that don't need backlinks. Keep them in **separate directories** to avoid confusion:
- `~/org/roam/` -- org-roam notes
- `~/org/notes/` -- denote notes
- `~/org/` -- GTD files (inbox.org, projects.org, etc.)

**Confidence: HIGH** -- Both are already installed; coexistence is well-documented in community.

### Journaling: org-journal vs org-roam dailies

| Criterion | org-journal (RECOMMENDED) | org-roam dailies |
|-----------|---------------------------|------------------|
| **Scope** | Dedicated journal with per-day/week/month files | Daily notes in org-roam (just another node) |
| **Carry-over** | Yes -- TODO items carry over to next day automatically | No |
| **Calendar** | Full Emacs calendar integration | No |
| **Search** | Built-in time-range search | Uses org-roam search |
| **Agenda** | Integrates with org-agenda | Only if added to agenda-files |
| **Doom flag** | `+journal` | Part of `+roam` |

**Verdict: org-journal for daily journaling.** org-roam dailies are better suited as "daily working notes" in a Zettelkasten context. org-journal provides the full journaling experience with carry-over, calendar, and search. You CAN use both: org-journal for structured daily journal, org-roam dailies for fleeting daily notes that link into the knowledge graph.

**Confidence: HIGH** -- Verified from both GitHub READMEs.

---

## What NOT to Use (and Why)

| Package | Why Not | What to Use Instead |
|---------|---------|---------------------|
| **org-superstar** | Subset of org-modern's functionality; uses character composition (less future-proof) | org-modern (already via `+pretty`) |
| **org-bullets** | Archived/unmaintained; org-superstar succeeded it, which org-modern then superseded | org-modern |
| **org-fancy-priorities** | org-modern already handles priority prettification; adding another layer is redundant and can conflict | org-modern's built-in priority styling |
| **org-roam v1** | Deprecated; v2 is a complete rewrite with different architecture | org-roam v2 (what Doom installs) |
| **org-brain** | Older, less maintained alternative to org-roam | org-roam |
| **org-gcal** | Already installed but OUT OF SCOPE for v1; complex OAuth setup | Defer to v2 milestone |

---

## Package-by-Package Setup Notes (Doom-Specific)

### 1. org-roam (via `+roam` flag)

**Installation:**
```elisp
;; init.el -- add +roam to org flags
(org +pretty +attach +babel +capture +export +present +roam +journal)
```

**packages.el:**
```elisp
;; IMPORTANT: Doom pins org-roam for stability. Unpin for latest features.
;; org-roam-ui docs recommend this:
(unpin! org-roam)
(package! org-roam-ui)
```

**config.el / config-org.el:**
```elisp
(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename "~/org/roam/"))
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))
```

**NixOS requirement:** `pkgs.sqlite` in system packages (or verify Emacs has built-in sqlite3).

**Confidence: HIGH** -- Doom README + org-roam GitHub both document this exact flow.

### 2. org-roam-ui

**Installation:** `(package! org-roam-ui)` in packages.el

**config.el:**
```elisp
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))  ;; don't auto-open browser
```

**Dependencies:** `websocket`, `simple-httpd`, `f` -- all auto-installed by Doom's straight.el.

**Usage:** `M-x org-roam-ui-mode` starts web server on `http://127.0.0.1:35901/`.

**Known issues:** 123 open issues on GitHub. Performance degrades with >2k nodes. Use Chromium-based browser for best performance. Alpha software per maintainers' own description.

**Confidence: HIGH** -- GitHub README provides exact Doom setup.

### 3. org-journal (via `+journal` flag)

**Installation:** Add `+journal` to org flags in init.el (shown above).

**config.el:**
```elisp
(after! org-journal
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-type 'daily
        org-journal-file-format "%Y-%m-%d.org"))
```

**Agenda integration:**
```elisp
(setq org-journal-enable-agenda-integration t)
```

**Confidence: HIGH** -- Doom `+journal` flag is well-documented; org-journal has 1.4k stars and active maintenance.

### 4. org-super-agenda (already installed)

**No installation needed** -- already in packages.el.

**config.el:**
```elisp
(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode))
```

Then configure `org-super-agenda-groups` for GTD. Key selectors for GTD:
- `:time-grid t` -- items on today's time grid
- `:todo "NEXT"` -- next actions
- `:todo "WAITING"` -- waiting-for items
- `:tag "inbox"` -- inbox items
- `:priority "A"` -- high priority
- `:deadline past` / `:deadline today` / `:scheduled today`
- `:habit t` -- habits (v2)
- `:auto-category` -- auto-group by file/category

**Version note:** v1.3 is latest release (Sep 2023). v1.4-pre in master has useful `:ancestor-with-todo` selector. MELPA tracks master, so you get 1.4-pre features automatically.

**Confidence: HIGH** -- Verified from GitHub README with extensive examples.

### 5. org-modern (already active via `+pretty`)

**No installation or packages.el changes needed.** Already active.

**Recommended config additions:**
```elisp
(after! org
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ..."))

;; Enable org-modern-mode in all org buffers (Doom's +pretty may already do this)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
```

**Font note:** org-modern works best when `variable-pitch` and `fixed-pitch` fonts have similar heights. The user has Iosevka fonts installed (recommended by org-modern author).

**Confidence: HIGH** -- Doom `+pretty` verified to install org-modern.

### 6. org-appear (already active via `+pretty`)

**No installation needed.** Already active via `+pretty` flag.

**Recommended config:**
```elisp
(after! org-appear
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t))

;; For evil-mode: only show markup in insert mode
(setq org-appear-trigger 'manual)
(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'evil-insert-state-entry-hook
                                     #'org-appear-manual-start nil t)
                           (add-hook 'evil-insert-state-exit-hook
                                     #'org-appear-manual-stop nil t)))
```

**Confidence: HIGH** -- org-appear GitHub README provides exact evil-mode integration.

### 7. org-cliplink (already included by Doom)

**No installation needed.** Doom's base org module includes org-cliplink.

**Usage:** Copy a URL, then `M-x org-cliplink` to insert `[[url][Page Title]]`.

**Bind it:**
```elisp
(map! :map org-mode-map
      :localleader
      "L" #'org-cliplink)
```

**Confidence: HIGH** -- Verified from Doom's `modules/lang/org/README.org` package list.

### 8. denote (already installed)

**No installation needed** -- already in packages.el.

**Recommended config:**
```elisp
(after! denote
  (setq denote-directory (expand-file-name "~/org/notes/")
        denote-known-keywords '("meeting" "idea" "reference" "project")
        denote-file-type 'org))
```

**Coexistence with org-roam:** Keep separate directories. denote uses filename-based metadata (`20260224T143000--meeting-with-team__meeting.org`), org-roam uses ID properties. They don't interfere as long as `org-roam-directory` and `denote-directory` are different paths.

**Confidence: HIGH** -- denote is designed to be non-invasive and directory-scoped.

---

## Complete packages.el Changes

```elisp
;; === NEW: Unpin org-roam for org-roam-ui compatibility ===
(unpin! org-roam)

;; === NEW: org-roam-ui for knowledge graph visualization ===
(package! org-roam-ui)

;; === ALREADY PRESENT (no changes needed) ===
;; (package! org-super-agenda)  ;; line 76
;; (package! denote)            ;; line 125
```

That's it. **Two lines added to packages.el.** Everything else is configuration.

---

## Complete init.el Changes

```elisp
;; Change this line:
(org +pretty +attach +babel +capture +export +present)

;; To this:
(org +pretty +attach +babel +capture +export +present +roam +journal)
```

That's it. **Two flags added.**

---

## Post-Installation Commands

```bash
# After changing init.el and packages.el:
doom sync -u        # Sync Doom packages (downloads org-roam, org-journal, org-roam-ui)

# Create directory structure:
mkdir -p ~/org/{gtd,roam,journal,notes,archive}

# Verify sqlite support (in Emacs):
# M-: (sqlite-available-p)  ;; should return t

# Rebuild NixOS if adding sqlite to system packages:
sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"

# Then rebuild Doom:
doom sync -u
```

---

## Dependency Graph

```
init.el flags
  +roam  -----> org-roam -----> emacsql, emacsql-sqlite, magit-section
                    |
                    v
               packages.el: org-roam-ui -----> websocket, simple-httpd, f
                    
  +journal ---> org-journal (no special deps)
  
  +pretty ----> org-modern (no special deps, already active)
           \--> org-appear (no special deps, already active)
  
  +capture ---> org-capture (built-in, already active)

packages.el (existing)
  org-super-agenda (no special deps, already installed)
  denote (no special deps, already installed)

NixOS system
  sqlite -----> required by org-roam's emacsql-sqlite
  graphviz ---> optional, for org-roam-graph (CLI graph, not UI)
```

---

## Sources

| Source | URL | Confidence | Date Verified |
|--------|-----|------------|---------------|
| org-roam GitHub | https://github.com/org-roam/org-roam | HIGH | 2026-02-24 |
| org-roam v2.3.1 Release | https://github.com/org-roam/org-roam/releases/tag/v2.3.1 | HIGH | 2026-02-24 |
| org-roam-ui GitHub | https://github.com/org-roam/org-roam-ui | HIGH | 2026-02-24 |
| org-journal GitHub | https://github.com/bastibe/org-journal | HIGH | 2026-02-24 |
| org-modern GitHub | https://github.com/minad/org-modern | HIGH | 2026-02-24 |
| org-super-agenda GitHub | https://github.com/alphapapa/org-super-agenda | HIGH | 2026-02-24 |
| org-appear GitHub | https://github.com/awth13/org-appear | HIGH | 2026-02-24 |
| org-cliplink GitHub | https://github.com/rexim/org-cliplink | HIGH | 2026-02-24 |
| Doom Emacs org module README | https://github.com/doomemacs/doomemacs/blob/master/modules/lang/org/README.org | HIGH | 2026-02-24 |
| Existing packages.el | Local file: `users/doom.d/packages.el` | HIGH | 2026-02-24 |
| Existing init.el | Local file: `users/doom.d/init.el` | HIGH | 2026-02-24 |
| Existing config-org.el | Local file: `users/doom.d/config-org.el` | HIGH | 2026-02-24 |

---

## Confidence Assessment

| Area | Level | Reason |
|------|-------|--------|
| Doom module flags | HIGH | Verified from official Doom README.org raw source |
| org-roam version/setup | HIGH | Verified v2.3.1 release (Jun 2025) + Doom docs |
| org-roam-ui setup | HIGH | GitHub README has exact Doom config |
| org-journal setup | HIGH | Doom `+journal` flag documented + GitHub README |
| org-modern / org-appear | HIGH | Already active via `+pretty`; verified from Doom source |
| org-super-agenda | HIGH | Already installed; extensive GitHub docs with examples |
| denote coexistence | HIGH | Separate directories, no conflict by design |
| NixOS sqlite requirement | MEDIUM | Should work with NixOS Emacs 29+, but needs runtime `(sqlite-available-p)` verification |
| org-fancy-priorities | MEDIUM | Likely redundant with org-modern; not critical |
| org-cliplink in Doom base | HIGH | Listed in Doom README package list |

---

## Roadmap Implications

1. **Phase 1 should be init.el + packages.el changes + NixOS sqlite + doom sync** -- this is the "infrastructure" phase that makes everything available.
2. **Phase 2 should be GTD structure + org-super-agenda** -- core workflow before knowledge base.
3. **Phase 3 should be org-roam + org-roam-ui** -- knowledge base after task management is solid.
4. **Phase 4 should be org-journal + capture templates** -- journaling layer.
5. **Phase 5 should be visual polish + dashboard** -- org-modern config tuning, keybindings, startup dashboard.

The key insight: **installation is trivial (2 flags + 2 package lines). Configuration is where the real work lives.** The roadmap should be organized around configuration domains, not installation.
