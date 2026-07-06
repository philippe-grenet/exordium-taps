# Markdown Mode tap

## Features

- Flyspell enabled by default
- Native code block fontification
- GFM checkboxes rendered as buttons
- Inline images displayed automatically
- `fill-column` set to 88 (for Tutti)
- Renderer: `multimarkdown` on macOS

## Keys

### General

| Keybinding              | Description                                          |
| ----------------------- | ---------------------------------------------------- |
| <kbd>s-Tab</kbd>        | Cycle visibility (`markdown-cycle`)                  |
| <kbd>s-$</kbd>          | Look up word in macOS Dictionary (macOS only)        |

### Table format

| Keybinding              | Description                                                            |
| ----------------------- | ---------------------------------------------------------------------- |
| <kbd>C-c m T</kbd>      | Toggle table format: standard <-> box-drawing (<kbd>C-u</kbd> to force back) |
| <kbd>C-c m R</kbd>      | Resize table to fit `fill-column` by wrapping cell content             |

### Table editing

Inside a pipe table, standard keys are context-sensitive:

| Keybinding              | Description                                              |
| ----------------------- | -------------------------------------------------------- |
| <kbd>Tab</kbd>          | Align table and move to next cell                        |
| <kbd>S-Tab</kbd>        | Align table and move to previous cell                    |
| <kbd>RET</kbd>          | Align table and move to next row (creates row if needed) |
| <kbd>M-left</kbd>       | Move column left                                         |
| <kbd>M-right</kbd>      | Move column right                                        |
| <kbd>M-up</kbd>         | Move row up                                              |
| <kbd>M-down</kbd>       | Move row down                                            |
| <kbd>M-S-left</kbd>     | Delete column                                            |
| <kbd>M-S-right</kbd>    | Insert column                                            |
| <kbd>M-S-up</kbd>       | Delete row                                               |
| <kbd>M-S-down</kbd>     | Insert row                                               |
| <kbd>C-c ^</kbd>        | Sort table lines (alpha or numeric)                      |
| <kbd>C-c &#124;</kbd>   | Convert region to pipe table                             |
| <kbd>C-c C-x C-t</kbd>  | Transpose table                                          |

Outside a table, <kbd>M-left</kbd> / <kbd>M-right</kbd> move by syntax unit.

## Utilities

### Straighten quotes

`M-x straighten-quotes` replaces smart/curly quotes in the region with ASCII equivalents.

### Markdown to Org conversion

`M-x markdown-to-org` converts Markdown in the active region (or whole buffer) to Org format in-place. Handles headings, emphasis, links, images, code blocks, blockquotes, lists, horizontal rules, and strikethrough.

## Files

| File                            | Purpose                                                               |
| ------------------------------- | --------------------------------------------------------------------- |
| `after-init.el`                 | Main configuration, keybindings, and utilities                        |
| `markdown-mode-table.el`        | Pipe table editing support (TAB/RET alignment, row/column operations) |
| `osx-dictionary.el`             | macOS Dictionary.app integration                                      |
| `markdown-to-org--string.t.el`  | Unit tests for `markdown-to-org--string`                              |
