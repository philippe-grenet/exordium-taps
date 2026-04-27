# Org Mode tap

## Keys

### Special keys

| Keybinding                          | Description                                          |
| ----------------------------------- | ---------------------------------------------------- |
| <kbd>F13</kbd> or <kbd>M-F12</kbd>  | Capture                                              |
| <kbd>F12</kbd>                      | List files in Org repo                               |
| <kbd>S-F12</kbd>                    | Open todo list                                       |
| <kbd>C-F12</kbd>                    | Open catch up notes                                  |
| <kbd>F19</kbd>                      | Open/close calendar                                  |

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

## Special syntax

* Backquotes for code block (equivalent of `~`)
* `{text}` or `{:text:}` for text in pill
* `(1)` or `(A`) for pills with one letter or one or two digits
