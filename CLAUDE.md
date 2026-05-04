# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

A personal Emacs configuration ("Modus Operandi Emacs") for Emacs 30 built with tree-sitter (and **without** pgtk). It is not an application or library; there is no build, test, or lint pipeline. "Running" the config means starting Emacs, and "shipping" means committing.

## File layout

- `init.el` — monolithic (~5k lines). All package configuration lives here as a flat sequence of `(use-package …)` forms. **Keep it monolithic** — don't split into multiple files.
- `early-init.el` — runs before package init; sets `gc-cons-threshold` high and disables `package.el`.
- `personal.el` — gitignored. Personal info, font, IRC nick. Loaded optionally from `init.el`.
- `custom.el` — gitignored. Auto-managed by `M-x customize`; in practice holds `safe-local-variable-values`. Don't hand-edit unless asked.
- `.cache/` — gitignored except `.cache/straight/versions/` (the straight.el lockfile, which **is** committed).
- `eln-cache/`, `tree-sitter/`, `snippets/`, `chatgpt/` — gitignored.

## Package management: straight.el

Packages are managed by [straight.el](https://github.com/radian-software/straight.el), bootstrapped from `init.el`. `straight-use-package-by-default` is `t`, so plain `(use-package foo)` will fetch `foo`. To pin a built-in or local package, add `:straight nil`.

**Do not upgrade packages.** Upgrades are user-initiated only — never run `straight-pull-all`, `straight-freeze-versions`, or otherwise modify `.cache/straight/versions/` on your own. For reference, the user's flow is `M-x straight-pull-all` then `M-x straight-freeze-versions`, committed as `straight: Upgrade all packages`.

## init.el conventions

When adding or editing entries in `init.el`, match the surrounding style:

- **Section header**: every `use-package` block is preceded by a one-line comment `;; Init <package> for <purpose>`. Always include this when adding a new block.
- **Symbol prefix**: project-defined functions, variables, and faces use `mo-` (public) or `mo--` (private). Don't introduce custom names without a prefix.
- **Where code lives is determined by which package owns the symbols it touches.** A `use-package <pkg>` block holds the configuration, hooks, bindings, and helper functions that use vars/functions defined by `<pkg>` — whether `<pkg>` is built-in or third-party. If code uses symbols from package A *and* package B, it belongs in the **using** package's block, not the defining one (e.g. a hook declared by A but whose handler calls into B's API goes in B's `use-package`). The `(use-package emacs …)` block is reserved for things defined at the **C layer** — true core, not Elisp packages that happen to ship with Emacs (those get their own `use-package … :straight nil` block).
- **`(use-package modus-operandi-emacs …)`** (`:straight nil :no-require t`) is **only** for logic that is unique to this configuration and doesn't belong to any specific package's namespace — cross-cutting helpers like `mo-copy-file-path`, `mo-open-project-with-tab`, the modeline composition. Extend it **only** when the change isn't attributable to a particular package; otherwise put the code in that package's block.
- **Ordering**: blocks are roughly grouped (core/files → evil → editing → completion → project/consult → vc/git → languages → UI → persistence). New entries should land near related ones, not at the end.
- **Spaced data parens**: when a paren-form is *data* rather than a function call, open it with a leading space — `( :keymaps 'foo "C-c" #'bar)`, `'( c-mode c-ts-mode rust-mode)`, `'( ( "a" . "Action"))`. This is purely a formatting trick: it stops the Lisp indenter from aligning continuation lines under the first element (as it would if it treated the form as a function call), so newlines can be added freely without indentation churn. Apply it to nested data lists too (e.g. `:states '( normal visual)`). Real function calls keep the normal form: `(setq foo bar)`, no leading space.
- **Cache paths**: persistent files (caches, history, backup dirs, bookmarks, etc.) belong under `.cache/`. Derive the path with `(mo-cache-path "<filename>")` — never hardcode `~/.emacs.d/.cache/...` or `user-emacs-directory`-relative paths.
- **Tree-sitter modes**: `treesit-auto` is enabled, so files open in `*-ts-mode` when the grammar is available. When wiring per-language configuration (hooks, LSP whitelist, mode-local bindings, etc.), target **both** the legacy and the tree-sitter mode — e.g. `python-mode` *and* `python-ts-mode`, `c++-mode` *and* `c++-ts-mode`. See `mo-lsp-enable-for-modes` for the established pattern.
- **Theme-dependent setup**: the config installs an advice on `enable-theme` that runs a custom `after-enable-theme` hook. Code that adjusts faces or otherwise depends on the active theme should attach to `after-enable-theme` rather than running at file load — load-time face setup gets silently overridden when a theme is later enabled. See `mo-lsp-configure-theme` for the canonical pattern.

## Keybindings: the quick-menu system

Bindings are declared via `general.el`'s `:general` keyword.

**How the leader works.**

- The leader key is `<menu>`, with three modifier variants (`C-<menu>`, `M-<menu>`, `C-M-<menu>`) bound to the **same** keymap, `mo-quick-menu-map`.
- After the leader, single-letter group keys route into sub-keymaps (e.g. `b` Buffer, `c` Code, `f` File, `g` Git, `j` Project — see `mo--quick-menu-groups` in `init.el` for the full list). At startup `mo--quick-menu-setup-modifier-variants` rebinds each group key under `C-`, `M-`, and `C-M-` modifiers too, so the same group is reachable as `c`, `C-c`, `M-c`, or `C-M-c`. The four variants let a modifier be held through the whole chord — useful when a saturated group binds modified action keys like `M-r`.

**When adding bindings.**

- Place new bindings inside an existing group when the action belongs to one. Don't invent a new top-level group key. Default to plain (unmodified) action keys; reach for modified ones only in saturated groups.
- **Global vs. mode-local quick-menu**: bind directly on `mo-quick-menu-map` (`:keymaps 'mo-quick-menu-map :prefix "<group>"`) when the action should always be reachable; use the `mo-quick-menu-definer` macro with `:keymaps 'foo-mode-map` when the binding should only be live while `foo-mode` is active. The macro expands to bind under all four leader variants — don't use the raw `mo--quick-menu-definer-*` definers directly.
- For **minor-mode** keymaps, use general's `:definer 'minor-mode` with `:keymaps 'foo-mode` (the mode symbol, not `foo-mode-map`) — this routes through `minor-mode-map-alist` and avoids precedence/shadowing issues that bite plain `:keymaps 'foo-mode-map` bindings on minor modes.
- **`C-M-s-<key>` is the "Hyper" prefix** — the user's QMK keyboard emits the full `Ctrl+Meta+Super` triple from a single physical key. Use it for **context-dependent** quick access to mode-local commands; bind it on a specific mode/minor-mode map (e.g. `emacs-lisp-mode-map`, `lsp-mode-map`), never on the global map or `mo-quick-menu-map`. Typical use: one-keystroke aliases for the most-used command in a given mode (e.g. `C-M-s-b` → `eval-buffer` in `emacs-lisp-mode-map`).
- **Adding evil-style bindings**: when the goal is a vim-style direct keystroke — typically a short mnemonic chord like `gc`, `z i`, `z SPC` that should only fire while evil is in a specific state — gate the binding by adding `:states 'normal` (or `'( normal visual)`, `'motion`, `'insert`) inside the `:general` arglist. Only these direct mode-map bindings are modal; quick-menu and Hyper bindings are never gated by state. Examples: `( :states '( normal visual) "z i" #'evil-numbers/inc-at-pt)`, `( :states 'normal "z SPC" #'string-inflection-cycle)`, `( :states 'motion "C-S-d" #'evil-scroll-up)`.

Examples of each pattern inside a `use-package`:

```elisp
;; Global quick-menu binding — leader → group → key.
;; The `:general' form binds directly on `mo-quick-menu-map'; the four leader
;; prefixes (<menu>, C-<menu>, M-<menu>, C-M-<menu>) all reach this map.
(use-package files
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"                       ; "f" group = File
    "f" #'find-file
    "w" #'write-file))

;; Mode-local quick-menu binding — use the `mo-quick-menu-definer' macro so
;; the binding is reachable from all four leader-modifier variants when the
;; mode's keymap is active.
(use-package lsp-mode
  :general
  (mo-quick-menu-definer
    :keymaps 'lsp-mode-map
    "h h" #'lsp-describe-thing-at-point))

;; Mode-local Hyper binding — context-dependent one-keystroke access. Bound
;; on the mode map directly, no leader involved.
(use-package elisp-mode
  :straight nil
  :general
  ( :keymaps 'emacs-lisp-mode-map
    "C-M-s-b" #'eval-buffer
    "C-M-s-r" #'eval-region))

;; Evil-style binding — vim-flavored direct keystroke gated by evil state.
;; No leader, no Hyper; `:states' declares which evil modes it's live in.
(use-package evil-numbers
  :general
  ( :states '( normal visual)
    "z i" #'evil-numbers/inc-at-pt
    "z d" #'evil-numbers/dec-at-pt))
```

## LSP

`lsp-mode` (not `eglot`) is the LSP client. The whitelist of major modes that auto-start LSP lives in `mo-lsp-enable-for-modes` inside the `lsp-mode` block. To enable LSP in a new mode, add it there — do not call `lsp-deferred` from a mode hook directly. Per-buffer disable: set `mo-lsp-disable` to non-nil.

## Evaluating changes without restarting

When Claude Code is launched from inside Emacs via the `claude-code-ide` package, the `mcp__ide__executeCode` tool is exposed and can apply changes in the running Emacs (deferred — load with `ToolSearch` first). It is **not** available when Claude Code is run from a plain terminal; in that case, the user has to reload manually.

When the tool is available, prefer evaluating the smallest scope that exercises the change: `eval-defun` on the edited form, or `eval-region`. Avoid `eval-buffer` on `init.el` — it re-runs every `use-package` and is slow / can re-trigger hooks. Evaluating in the user's live Emacs is mutating; narrow the scope, and don't run anything that could block, prompt modally, or kill buffers/frames.

## Git / commit conventions

Commit subjects fall into three patterns:

- **First-time introduction of a built-in package**: `Init <package> for <purpose>` (e.g. `Init faces for tty terminal setup`, `Init ffap and prevent network pings when encountering hostnames`). Since the package is already present in Emacs, this commit can include both the `use-package` form **and** its initial configuration.
- **First-time introduction of a third-party package**: `Add <package> for <purpose>` (e.g. `Add ghostel for terminal emulation`, `Add exec-path-from-shell for inheriting shell environment variables`). Prefer making this commit just the bare `(use-package …)` declaration; layer configuration on in subsequent commits using the prefixed form below. This is also the commit where the package's pin in `.cache/straight/versions/default.el` is added — stage that lockfile change together with the `init.el` change, in the same commit.
- **Subsequent changes to an already-introduced package**: `<package>: <Imperative summary>` (e.g. `ghostel: Enable in compile mode`, `vertico: Add command to toggle sort and key binding`). Lowercase the package prefix; capitalize the summary; no trailing period.

Keybinding-related commits use canonical phrasings — match them rather than improvising:

- Add a quick-menu binding: `<pkg>: Add <command-purpose> command key binding to the quick menu` (drop `<command-purpose>` if it would just restate the package, e.g. `ffap: Add command key binding to the quick menu`).
- Change a quick-menu binding: `<pkg>: Change <command-purpose> command quick menu key binding` (e.g. `pr-review: Change search open command quick menu key binding`).
- Add a Hyper (`C-M-s-`) binding: `<pkg>: Add multi-modifier <command-purpose> command key binding` (e.g. `org: Add multi-modifier sparse-tree command key binding`; or just `<pkg>: Add multi-modifier command key bindings` for a batch).
- Change a Hyper binding: `<pkg>: Change <command-purpose> command multi-modifier key binding` (e.g. `pr-review: Change request reviews command multi-modifier key binding`).

Don't commit `custom.el` or `personal.el` — both are gitignored intentionally. The `.cache/straight/versions/` lockfile **is** tracked and should be updated when packages are upgraded.
