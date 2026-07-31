# Common tap libraries

This directory contains shared Elisp libraries used by multiple taps. It is
**not** a tap itself (no `after-init.el`), so Exordium does not load it
automatically. Each tap that needs a library here should `load-file` it
explicitly.

## Files

| File                  | Description                                                        |
| --------------------- | ------------------------------------------------------------------ |
| `table-format.el`     | Toggle pipe tables between standard and box-drawing format, resize tables to fit `fill-column`. Used by the `org-mode` and `markdown-mode` taps. |
| `description-field.el` | `my/update-description`: insert/update a one-line file description — Org `#+description:` keyword or Markdown YAML frontmatter `description:` key, chosen by major mode. Used by the `org-mode` and `markdown-mode` taps. |
