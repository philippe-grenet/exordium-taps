# Markdown Mode tap

## Features

- Flyspell enabled by default
- Native code block fontification
- GFM checkboxes rendered as buttons
- Inline images displayed automatically
- Inline Mermaid diagram rendering (display-only overlays)
- `fill-column` set to 88 (for Tutti)
- Renderer: `multimarkdown` on macOS

## Keys

### General

| Keybinding              | Description                                          |
| ----------------------- | ---------------------------------------------------- |
| <kbd>s-Tab</kbd>        | Cycle visibility (`markdown-cycle`)                  |
| <kbd>s-$</kbd>          | Look up word in macOS Dictionary (macOS only)        |
| <kbd>C-c m #</kbd>      | Insert/update the file description (`my/update-description`) |

### Table format

| Keybinding              | Description                                                            |
| ----------------------- | ---------------------------------------------------------------------- |
| <kbd>C-c m T</kbd>      | Toggle table format: standard <-> box-drawing (<kbd>C-u</kbd> to force back) |
| <kbd>C-c m R</kbd>      | Resize table to fit `fill-column` by wrapping cell content             |

### Mermaid

| Keybinding              | Description                                                            |
| ----------------------- | ---------------------------------------------------------------------- |
| <kbd>C-c m m</kbd>      | Render / refresh all Mermaid blocks as inline images                   |
| <kbd>C-c m M</kbd>      | Clear the rendered Mermaid diagrams                                    |

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

### File description

`M-x my/update-description` (<kbd>C-c m #</kbd>) inserts or updates a one-line
description in the buffer's YAML frontmatter, for use by an LLM-wiki `index.md`
indexer. If frontmatter exists (a `---`-fenced block on line 1), the
`description:` key is updated in place or added; otherwise a new block is
prepended above any `# H1`. The value is double-quoted when it contains a colon
or other YAML-special characters. When updating, the prompt is pre-filled with
the current value. The same command works in Org buffers (see the Org tap).

### Straighten quotes

`M-x straighten-quotes` replaces smart/curly quotes in the region with ASCII equivalents.

### Markdown to Org conversion

`M-x markdown-to-org` converts Markdown in the active region (or whole buffer) to Org format in-place. Handles headings, emphasis, links, images, code blocks, blockquotes, lists, horizontal rules, and strikethrough.

### Inline Mermaid rendering

Renders ` ```mermaid ` fenced code blocks as images shown in overlays right below each block, mirroring the `ob-mermaid` experience used in Org. It is display-only: the Markdown file is never modified and no PNGs are written next to your notes (renders are cached in a temp directory, keyed on block content). Requires the Mermaid CLI (`mmdc`).

Press <kbd>C-c m m</kbd> to render and <kbd>C-c m M</kbd> to clear. Overlays do not auto-refresh, so re-run <kbd>C-c m m</kbd> after editing a block. Rendering options (theme, background, width) are `defvar`s at the top of `markdown-mermaid.el`.

## Files

| File                            | Purpose                                                               |
| ------------------------------- | --------------------------------------------------------------------- |
| `after-init.el`                 | Main configuration, keybindings, and utilities                        |
| `markdown-mode-table.el`        | Pipe table editing support (TAB/RET alignment, row/column operations) |
| `markdown-mermaid.el`           | Inline Mermaid diagram rendering via display-only overlays            |
| `osx-dictionary.el`             | macOS Dictionary.app integration                                      |
| `markdown-to-org--string.t.el`  | Unit tests for `markdown-to-org--string`                              |
