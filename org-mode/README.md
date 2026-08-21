# Org Mode tap

## Keys

### Special keys

| Keybinding                          | Description                                          |
| ----------------------------------- | ---------------------------------------------------- |
| <kbd>F13</kbd> or <kbd>M-F12</kbd>  | Capture                                              |
| <kbd>F12</kbd>                      | List files in Org repo                               |
| <kbd>S-F12</kbd>                    | Open todo list                                       |
| <kbd>C-F12</kbd>                    | Open catch up notes                                  |
| <kbd>C-c o c</kbd>                  | Show next-meeting TODOs for a person (see below)     |
| <kbd>F19</kbd>                      | Open/close calendar                                  |
| <kbd>⌃⌥⌘F12</kbd>                   | Capture to Inbox from anywhere in macOS (see below)  |
| <kbd>⇧⌃⌥⌘F12</kbd>                  | Same, with the full template menu                    |

#### Desktop capture

<kbd>⌃⌥⌘F12</kbd> is Capture (<kbd>M-F12</kbd>) made available everywhere in the macOS
desktop, not just inside Emacs. It pops a small dedicated Emacs frame, centered on the
screen holding the focused window, which closes itself on <kbd>C-c C-c</kbd>,
<kbd>C-c C-k</kbd>, <kbd>C-c C-w</kbd> or <kbd>C-g</kbd>. The plain binding goes straight
to the Inbox template; add <kbd>⇧</kbd> to get the template menu instead. Pressing the
hotkey again while a capture is open just refocuses that frame.

The Emacs half lives in `org-capture-frame.el`. The hotkey itself is bound in
Hammerspoon (`~/dotfiles/init.lua`), which shells out to `emacsclient` and passes the
screen rectangle to center in — Emacs has no way of knowing which display you are
looking at. It relies on the Emacs server, started by the `util` tap; if Emacs is not
running, or the server is down, Hammerspoon reports it as an on-screen alert. Note that
once the capture frame closes, macOS gives focus back to your main Emacs frame rather
than to the application you were in.

#### Catch-up TODOs

<kbd>C-c o c</kbd> is a companion to Capture (<kbd>M-F12</kbd>): from any buffer it pops
up the same single-key person menu built from `org-capture-templates` (plus Inbox and
Today). After picking a person, a small, dismissable buffer lists only their open
discussion items — the headings marked `TODO`, `WORK`, or `WAIT` directly under that
person in `catchup.org` (dated notes and closed items are skipped) — so you can quickly
see what to raise at your next meeting. Press <kbd>q</kbd> to dismiss.

### Navigation

| Keybinding            | Description                                                        |
| --------------------- | ------------------------------------------------------------------ |
| <kbd>super ↓</kbd>    | Move to next header (`org-forward-heading-same-level`).            |
| <kbd>super ↑</kbd>    | Move to previous header (`org-backward-heading-same-level`).       |
| <kbd>super ←</kbd>    | Move to parent header (`outline-up-heading`).                      |

### TODO list

| Keybinding            | Description                                                        |
| --------------------- | ------------------------------------------------------------------ |
| <kbd>C-c o d</kbd>    | Toggle between DONE and TODO                                       |
| <kbd>C-c o t</kbd>    | Move to beginning of Today (<kbd>C-u</kbd> for the end)            |
| <kbd>C-c o w</kbd>    | Move to beginning of Week (<kbd>C-u</kbd> for the end)             |
| <kbd>C-c o b</kbd>    | Move to beginning of Backlog (<kbd>C-u</kbd> for the end)          |

### Editing

| Keybinding            | Description                                                        |
| --------------------- | ------------------------------------------------------------------ |
| <kbd>C-c o d</kbd>    | Toggle between DONE and TODO                                       |
| <kbd>C-c o l</kbd>    | Paste a Jira epic link                                             |
| <kbd>C-c o i</kbd>    | Insert an image placeholder                                        |
| <kbd>C-c o m</kbd>    | Insert a Mermaid diagram placeholder                               |
| <kbd>C-c o #</kbd>    | Insert/update the file description (`my/update-description`)       |

### Tables

| Keybinding            | Description                                                                |
| --------------------- | -------------------------------------------------------------------------- |
| <kbd>C-c o T</kbd>    | Toggle table format: standard <-> box-drawing (<kbd>C-u</kbd> to force back) |
| <kbd>C-c o R</kbd>    | Resize table to fit `fill-column` by wrapping cell content                 |

### Viewing

| Keybinding            | Description                                                        |
| --------------------- | ------------------------------------------------------------------ |
| <kbd>C-c C-o</kbd>    | Standard shortcut to open a link in Emacs                          |
| <kbd>C-c o o<kbd>     | Open image externally (using native MacOS app)                     |
| <kbd>C-c o D<kbd>     | Open DRQS                                                          |

### Live preview

| Keybinding            | Description                                                             |
| --------------------- | ---------------------------------------------------------------------- |
| <kbd>C-c o v</kbd>    | Toggle live HTML preview in an xwidget (<kbd>C-u</kbd> to include a TOC) |
| <kbd>C-c o V</kbd>    | Toggle preview theme (Mocha <-> Latte)                                 |

Renders the buffer to HTML via `ox-html` in an xwidget window, re-rendered on save,
using the same Catppuccin CSS as the Markdown preview. Closing either the source Org
buffer or the preview buffer tears down the preview symmetrically: the other buffer
is closed and the temporary HTML file (in `/tmp`) is deleted. Closing the preview
never kills the source buffer, so it won't respawn.

## Statuses

| Status      | Meaning                                                                      |
| ----------- | ---------------------------------------------------------------------------- |
| TODO        | to do                                                                        |
| DONE        | done                                                                         |
| WORK        | in progress                                                                  |
| STOP        | no more work at this time, but not blocked                                   |
| WAIT        | waiting on dependency. First word should be who or what are we waiting on    |
| BLOCKED     | blocked on something (not someone)                                           |
| READY       | for mentoring                                                                |
| REVIEW      | for requirements                                                             |

These statuses are styled as SVG pills globally. A file that declares its own
extra statuses via a `#+todo:` line gets pills for them automatically (see
below), so you can use ad-hoc workflows without touching this config.

### Per-file statuses

When a file defines extra TODO keywords with a `#+todo:` (or `#+seq_todo:`)
line, any keyword that isn't already styled globally is given an SVG pill on the
fly, in that buffer only. Colours are auto-assigned: done-type keywords (those
after the `|`) are dimmed, the rest cycle through a small palette.

```org
#+todo: TODO GOAL | DONE DROPPED
```

To pin a specific colour instead of the auto-assigned one, add one or more
`#+svg_todo:` directives naming the keyword and a face:

```org
#+svg_todo: DROPPED font-lock-comment-face
#+svg_todo: GOAL    font-lock-warning-face
```

Invalid face names are ignored (the keyword falls back to the auto palette). The
global `svg-tag-tags` list is never modified — each buffer gets its own copy.

Note: pills are generated when the buffer loads. If you edit a `#+todo:` or
`#+svg_todo:` line in an open buffer, re-run `M-x org-mode` (or revert the file)
to pick up the change.

## File description

<kbd>C-c o #</kbd> (`M-x my/update-description`) inserts or updates a one-line
`#+description:` keyword near the top of the file, for use by an LLM-wiki
`index.md` indexer. An existing `#+description:` line (case-insensitive) is
updated in place; otherwise the keyword is inserted into the top keyword block —
right after `#+title:` if present, else after the last leading `#+keyword:`
line, else at the very top, always before the first headline. When updating, the
prompt is pre-filled with the current value. The same command works in Markdown
buffers (see the Markdown tap), where it maintains a YAML frontmatter
`description:` key instead.

## Special syntax

* Backquotes for code block (equivalent of `~`)
* `{:text:}` for text in a rectangular tag
* `((A))` or `((1))` for round pills with one letter or digit, and `((AA))` or
  `((11))` for round pills with two letters or digits
