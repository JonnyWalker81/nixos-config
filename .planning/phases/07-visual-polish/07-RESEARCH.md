# Phase 7: Visual Polish - Research

**Researched:** 2026-03-05
**Domain:** Doom Emacs Org visual UX (org-modern, org-appear, inline previews)
**Confidence:** HIGH

## Summary

Phase 7 should use Doom's existing `:lang org +pretty` stack rather than introducing a parallel styling system. In current Doom upstream, `+pretty` already ships `org-modern` and `org-appear`, and configures them with the same hook pattern this phase needs (`org-mode-hook` + `org-agenda-finalize-hook`). For this repo, the correct strategy is to add a dedicated `users/doom.d/config-org-visual.el`, load it after current org modules, and keep semantic TODO/priority face ownership explicit so visuals do not regress when Doom defaults change.

Locked context decisions are directly achievable with standard Org + Doom primitives: `org-modern` for subtle hierarchy/tables/timestamps + agenda polish, `org-todo-keyword-faces` for exact TODO semantics, `org-appear` for reveal-at-point emphasis markers with small delay for anti-flicker, and Org startup/hook options for inline images and LaTeX preview generation in current buffers.

Most risk is load-order and ownership, not feature capability. This repo already splits Org behavior across `config-org.el`, `config-org-gtd.el`, and `config-org-agenda.el`; visual behavior must be centralized in a new visual module and loaded last in the org stack to avoid silent Doom/module overrides.

**Primary recommendation:** Implement Phase 7 as two plans: (1) org-modern + agenda visual baseline, then (2) TODO/priority/emphasis/preview behavior and validation.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| Org mode | Doom pin `release_9.8` | Native Org rendering, inline images, LaTeX preview | Canonical upstream feature surface used by Doom |
| org-modern | Doom `+pretty` pin `b4b5b1c...` | Modern visual styling for Org buffers and agenda | Official Doom `+pretty` package; supports `org-modern-agenda` hook |
| org-appear | Doom `+pretty` pin `32ee50f...` | Reveal hidden markers at point/edit location | Purpose-built for interactive hidden-marker reveal |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| org-super-agenda | repo package | Agenda grouping already used in Phase 3/4 | Keep; only restyle faces/spacing, do not replace grouping logic |
| Doom `after!` / `use-package!` | Doom core macros | Safe load-order control | Required for all org visual config in this repo |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| org-modern | org-superstar/org-bullets | Older, narrower scope; less complete than org-modern |
| org-appear | manual overlay/advice code | Reinvents complex cursor-sensitive behavior and edge cases |
| Org built-in only styling | custom ad-hoc faces and composition | More maintenance, less consistency with Doom `+pretty` |

**Installation:**
```bash
# No new package required if :lang org +pretty remains enabled.
# Doom already installs org-modern and org-appear via modules/lang/org/packages.el.

# Only if explicit local declaration is desired:
# package! org-modern
# package! org-appear
```

## Architecture Patterns

### Recommended Project Structure
```
users/doom.d/
├── config-org.el          # core org behavior (babel/export/etc.)
├── config-org-gtd.el      # GTD semantics (todo keywords, priority meaning)
├── config-org-agenda.el   # agenda commands and org-super-agenda grouping
└── config-org-visual.el   # NEW: all Phase 7 visuals and preview behavior
```

### Pattern 1: Visual Module Ownership + Late Load
**What:** Keep all visual knobs in `config-org-visual.el`, loaded after `config-org-gtd.el` and `config-org-agenda.el`.
**When to use:** Always for this phase; prevents face/settings split-brain.
**Example:**
```elisp
;; Source: Doom load-order pattern + repo config.el sequencing
(after! org
  ;; visuals only
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"))
```

### Pattern 2: org-modern for Buffers + Agenda
**What:** Enable `org-modern-mode` in org buffers and `org-modern-agenda` after agenda render finalization.
**When to use:** Required for VIS-01 and VIS-02.
**Example:**
```elisp
;; Source: org-modern README + Doom lang/org/contrib/pretty.el
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-modern-label-border 1
        org-modern-timestamp t
        org-modern-table t
        org-modern-priority nil)) ; keep priority styling subtle/manual
```

### Pattern 3: TODO Colors Primary, Priority Overlay Secondary
**What:** Keep TODO state color as primary signal; avoid org-modern inverse boxes overriding semantic colors.
**When to use:** Always with locked semantics and subtle priority overlay requirement.
**Example:**
```elisp
;; Source: Doom pretty.el face-mapping behavior + phase locked decisions
(after! org
  (setq org-todo-keyword-faces
        '(("TODO"      . (:foreground "#ff6c6b" :weight semibold))
          ("NEXT"      . (:foreground "#51afef" :weight bold))
          ("WAITING"   . (:foreground "#dca561" :weight semibold))
          ("SOMEDAY"   . (:foreground "#7f8490" :weight normal))
          ("DONE"      . (:foreground "#98be65" :weight normal))
          ("CANCELLED" . (:foreground "#5b6268" :weight normal :strike-through t))))

  (setq org-priority-faces
        '((?A . (:weight semibold :foreground "#e5c07b"))
          (?B . (:foreground "#7f8490"))
          (?C . (:foreground "#5b6268"))))

  ;; Prevent org-modern from replacing TODO/priority faces with inverse labels.
  (setq org-modern-todo-faces nil
        org-modern-priority-faces nil))
```

### Pattern 4: Hidden Markup with Reveal-at-Point
**What:** Enable hidden emphasis markers globally in Org and use `org-appear` for contextual reveal with short delay.
**When to use:** Required for VIS-04 and anti-flicker decision.
**Example:**
```elisp
;; Source: org-appear README
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-delay 0.12
        org-appear-trigger 'always))
```

### Pattern 5: Auto Inline Images and LaTeX Previews
**What:** Use startup variables plus buffer-entry hooks to ensure previews appear automatically in opened Org buffers.
**When to use:** Required for VIS-05.
**Example:**
```elisp
;; Source: Org manual (inline images + latex preview)
(after! org
  (setq org-startup-with-inline-images t
        org-startup-with-latex-preview t)
  (add-hook 'org-mode-hook
            (defun org-life-visual-preview-init-h ()
              (org-display-inline-images)
              (org-latex-preview '(16))))) ; preview whole buffer on open
```

### Anti-Patterns to Avoid
- **Split visual config across GTD/agenda/core files:** causes silent overrides and hard-to-debug regressions; centralize in `config-org-visual.el`.
- **Rely on org-modern default TODO/priority badges:** can obscure locked TODO semantics and over-emphasize labels.
- **Custom overlay implementation for emphasis reveal:** duplicates org-appear behavior and introduces cursor/redisplay bugs.
- **Running full-buffer LaTeX preview on every keystroke/save:** can become slow; trigger on buffer open and explicit refresh points.

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Modern bullets/tables/timestamps/agenda labels | Manual font-lock overlay suite | `org-modern` + `org-modern-agenda` | Already handles many Org syntax surfaces and edge cases |
| Reveal hidden emphasis markers near cursor | Custom point-motion overlays/advice | `org-appear` | Maintained solution with delay/trigger controls |
| Inline image refresh system | Custom link parsing and image overlays | `org-display-inline-images` / startup option | Org-native and compatible with export/link behavior |
| LaTeX fragment render pipeline | Ad-hoc external command orchestration | `org-latex-preview` + startup option | Uses Org's converter/process settings and cache handling |

**Key insight:** In this domain, custom visual overlays drift quickly as Org internals change; package- and Org-native entrypoints are lower-risk and easier to maintain.

## Common Pitfalls

### Pitfall 1: Doom `+pretty` already configures org-modern/org-appear
**What goes wrong:** Local config unexpectedly fights Doom defaults.
**Why it happens:** `:lang org +pretty` auto-enables both packages and sets defaults.
**How to avoid:** Configure in one `config-org-visual.el` with `after! org` and `use-package!` overrides.
**Warning signs:** TODO badges unexpectedly boxed/inverse; settings "revert" after restart.

### Pitfall 2: Visual ownership split across modules
**What goes wrong:** TODO colors and agenda visuals diverge between files.
**Why it happens:** Existing repo intentionally separates GTD semantics and agenda logic; visual tweaks scattered later become non-deterministic.
**How to avoid:** Keep semantics in `config-org-gtd.el`; keep all presentation in `config-org-visual.el`.
**Warning signs:** Agenda colors do not match Org buffer TODO colors.

### Pitfall 3: Agenda styling not applied
**What goes wrong:** Org buffers look modern but agenda buffers stay plain.
**Why it happens:** Missing `org-agenda-finalize-hook` integration.
**How to avoid:** Ensure `org-modern-agenda` is hooked in finalize phase, not only `org-mode-hook`.
**Warning signs:** Agenda headings/timestamps appear legacy while .org files are styled.

### Pitfall 4: Emphasis marker flicker/no reveal
**What goes wrong:** Markers flicker too much or never reappear at point.
**Why it happens:** `org-hide-emphasis-markers` off, or `org-appear` trigger/delay misconfigured.
**How to avoid:** Set hide markers globally and tune `org-appear-delay` around 0.08-0.15 seconds.
**Warning signs:** Cursor entering `*bold*` shows no markers, or constant flashing while navigating.

### Pitfall 5: LaTeX previews fail silently
**What goes wrong:** Fragments remain raw source despite startup flags.
**Why it happens:** Missing converter backend (`dvipng`, `dvisvgm`, or `convert`) or process mismatch.
**How to avoid:** Verify converter availability and set `org-preview-latex-default-process` if needed.
**Warning signs:** `org-latex-preview` errors in `*Messages*` and no overlays rendered.

## Code Examples

Verified patterns from official sources:

### org-modern + agenda integration
```elisp
;; Source: https://raw.githubusercontent.com/minad/org-modern/main/README.org
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
```

### org-appear reveal configuration
```elisp
;; Source: https://raw.githubusercontent.com/awth13/org-appear/master/README.org
(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-appear-autoemphasis t
      org-appear-delay 0.12
      org-appear-trigger 'always)
```

### Inline images and startup behavior
```elisp
;; Source: https://orgmode.org/manual/In_002dbuffer-Settings.html
;; STARTUP keyword equivalent: #+STARTUP: inlineimages
(setq org-startup-with-inline-images t)
```

### LaTeX fragment preview startup
```elisp
;; Source: https://orgmode.org/manual/Previewing-LaTeX-fragments.html
;; STARTUP keyword equivalent: #+STARTUP: latexpreview
(setq org-startup-with-latex-preview t)
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| `org-bullets` / `org-superstar` for only headline symbols | `org-modern` for broad Org visual system | org-modern adoption in Doom `+pretty` | More complete styling for headings, tables, timestamps, agenda |
| Static hide/show markup behavior | `org-appear` contextual reveal | Matured in MELPA era (latest release 0.3.1, 2024-07-16) | Better editing UX with hidden markup by default |
| Manual inline image toggling only | Startup-enabled inline images and refresh commands | Org startup options available in current manual | Less friction for media-heavy notes |

**Deprecated/outdated:**
- Hand-rolled decorative overlay configs as primary approach: replaced by standardized `org-modern`/`org-appear` package flow.

## Implementation Blueprint

Two-plan split is appropriate and aligns with roadmap draft:

1. **07-01 org-modern baseline**
   - Create `users/doom.d/config-org-visual.el` and load it in `users/doom.d/config.el` after `config-org-agenda`.
   - Configure `org-modern` (org + agenda hooks, subtle bullets/tables/timestamps, compact agenda framing).
   - Keep agenda section labels quiet and priority-A mildly emphasized via faces, not heavy separators.

2. **07-02 semantic faces + reveal + previews + validation**
   - Apply locked TODO color semantics exactly (`NEXT blue`, `WAITING orange`, `SOMEDAY grey`, `TODO red`, `DONE green`, `CANCELLED dim`).
   - Keep priority overlay subtle and secondary to TODO color.
   - Configure `org-appear` reveal-at-point with anti-flicker delay.
   - Enable auto inline image display and auto LaTeX preview for current org buffers.
   - Add phase verification artifact mapping VIS-01..VIS-05.

## Verification Checklist

Planner-reusable checks:

```bash
# 1) Sync package graph (only needed if packages changed)
doom sync

# 2) Reload Doom and inspect package presence
doom info | rg "org-modern|org-appear"
```

In Emacs (`M-:` / `M-x`):

```elisp
;; Package and mode activation
(featurep 'org-modern)
(featurep 'org-appear)

;; In an .org buffer:
org-modern-mode
org-appear-mode
org-hide-emphasis-markers

;; Agenda styling hook check
(member #'org-modern-agenda org-agenda-finalize-hook)

;; Locked TODO semantics check
org-todo-keyword-faces
org-priority-faces

;; Preview behavior checks
org-startup-with-inline-images
org-startup-with-latex-preview
```

Manual acceptance tests:
- Open `~/org/gtd/inbox.org`: confirm modern headline/table/timestamp style and hidden emphasis markers.
- Move cursor into `*bold*` and `/italic/`: markers appear only near point, then hide smoothly.
- Insert `[[file:...png]]` image link in org file and reopen buffer: image auto-renders inline.
- Insert LaTeX fragment like `$E=mc^2$` and reopen buffer: preview image appears in current buffer.
- Open `SPC o a d` and `SPC o a R`: agenda uses modern styling, quiet group labels, and Priority A is visually first with mild emphasis.

## Open Questions

1. **Org link preview API compatibility (`org-display-inline-images` vs newer link-preview commands)**
   - What we know: Current Org manual documents newer link-preview command family and startup options; classic inline image functions remain widely used.
   - What's unclear: Exact command surface in this local Doom build at runtime.
   - Recommendation: Use `org-startup-with-inline-images` + `org-display-inline-images` first for compatibility, then optionally adopt newer link-preview APIs if present.

2. **Default org-modern face boxing intensity under `doom-tokyo-night`**
   - What we know: Locked decision requires subtle/quiet visuals.
   - What's unclear: Exact face border/box values that best satisfy user preference without manual calibration.
   - Recommendation: Start with low-contrast defaults, verify on real agenda/org buffers, then fine-tune face attrs in plan 07-02.

## Sources

### Primary (HIGH confidence)
- https://raw.githubusercontent.com/doomemacs/doomemacs/master/modules/lang/org/packages.el - Doom package pins and `+pretty` package ownership (`org-modern`, `org-appear`, Org 9.8 pin)
- https://raw.githubusercontent.com/doomemacs/doomemacs/master/modules/lang/org/contrib/pretty.el - Doom default hooks and baseline config for `org-modern` and `org-appear`
- https://raw.githubusercontent.com/minad/org-modern/main/README.org - official `org-modern` setup and known incompatibilities
- https://raw.githubusercontent.com/awth13/org-appear/master/README.org - official `org-appear` behavior and variables
- https://orgmode.org/manual/In_002dbuffer-Settings.html - startup options including `inlineimages`
- https://orgmode.org/manual/Previewing-LaTeX-fragments.html - LaTeX preview requirements and startup keyword
- https://orgmode.org/manual/Images-and-link-previews.html - inline preview behavior and commands

### Secondary (MEDIUM confidence)
- https://github.com/awth13/org-appear/releases/tag/0.3.1 - release date context for current package freshness

### Tertiary (LOW confidence)
- None

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - verified directly from Doom module package manifests and official package docs
- Architecture: HIGH - aligned with existing repo module split and Doom `after!` behavior
- Pitfalls: HIGH - confirmed against Doom `+pretty` defaults and repo's current org ownership pattern

**Research date:** 2026-03-05
**Valid until:** 2026-04-04 (30 days; moderate ecosystem churn)
