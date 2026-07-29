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
| QUESTIONED  | for requirements                                                             |
| PROCEED     | for hiring                                                                   |
| REJECT      | for hiring                                                                   |

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
* `{text}` or `{:text:}` for text in pill
* `(1)` or `(A`) for pills with one letter or one or two digits
