# util tap

Catch-all tap for small settings, keybindings, and helpers that don't warrant a
tap of their own, plus a major mode for BQL.

| File | Contents |
| --- | --- |
| `after-init.el` | Global settings, keybindings, and utility commands |
| `bql-mode.el` | Major mode for Bloomberg Query Language (loaded at the end of `after-init.el`) |

## after-init.el

### Keyboard modifiers

`pc-mode` binds Alt to Meta and the Windows key to Super (for the Bloomberg
keyboard); `mac-mode` binds Option to Meta and Command to Super. PC mode is the
default.

### Commands

| Command | Description |
| --- | --- |
| `fill-unfill-paragraph` | `M-q` to fill, `C-u M-q` to unfill |
| `depunctuate` / `repunctuate` | One or two spaces after sentence ends in region or comment |
| `straighten-quotes` | Replace smart quotes with ASCII quotes |
| `note` | New unsaved markdown scratch buffer in `~/Documents/scratch` |
| `exordium-insert-today` | Insert `[YYYY-MM-DD]` Org timestamp (`C-c C-.`) |
| `open-devx-space-ssh` | Open a DevX Space in Dired from a pasted `ssh` string |

### Key bindings

| Key | Action |
| --- | --- |
| `C-x b` | `helm-buffers-list` (file buffers listed first) |
| `C-c f` | `helm-projectile` |
| `C-c r` | `revert-buffer-quick` |
| `C-M-<up>` / `C-M-<down>` | Scroll one line |
| `f6` / `f7` | `symbol-overlay-put` / flyspell correct previous word |
| `mouse-3` | Flyspell correct word at click |
| `s-SPC` | macOS character palette |
| `s-q` prefix | Unicode input: `s-q m …` math, `s-q g …` Greek, `s-q f …` status flags |

### Settings

Visual bell (mode line flash), Helm autoresize and TAB completion, Company
(TAB completes, no downcasing, 3-char minimum), git-gutter fringe bitmaps,
ediff/magit diff preferences, `prettify-symbols` for arrows in Org and
Markdown, `gfm-mode` for `.md`, `ultra-scroll`, `vterm`, and the Emacs server
(started unless one is already running).

## bql-mode.el

Major mode derived from `prog-mode`, auto-enabled for `.bql` files.

- Syntax highlighting for BQL keywords, ~100 builtin functions, constants,
  `table.field` identifiers, numbers with date suffixes, strings, and `#macros`
- `##` line comments
- Paren-depth indentation (`bql-indent-offset`, default 2)
- Electric pairs and Imenu (let bindings, get blocks)
