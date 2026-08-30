# Exordium taps

Local extensions to [Exordium](https://github.com/emacs-exordium/exordium): my
own Emacs configuration, kept in a repo of its own nested inside `~/.emacs.d`.

Each subdirectory is a *tap*. Exordium scans this directory at startup and
loads, in alphabetical order, every tap's `before-init.el` first, then every
tap's `after-init.el` — so a tap can rely on values declared in another tap's
`before-init.el`, and a tap late in the alphabet can override an earlier one.

## Taps

| Directory        | Features                                                                                       |
| ---------------- | ---------------------------------------------------------------------------------------------- |
| `ai`             | Claude Code in Emacs (`C-c c`), plus the org repo's Claude skills as `M-x` commands            |
| `code-review`    | Reviewing GitHub pull requests, and reviewing local changes written by a coding agent          |
| `common`         | Shared libraries for all taps:  table formatting, file descriptions, and the org repo location |
| `lsp`            | C++ tree-sitter settings and an (off by default) LSP setup                                     |
| `markdown-mode`  | Markdown editing: Mermaid diagrams, inline images, tables, macOS dictionary                    |
| `org-mode`       | Org Mode and my second-brain notes repo: capture, refile, agenda, Drive sync, DRQS links       |
| `treemacs`       | Treemacs bindings (<kbd>F5</kbd>, `C-c e`)                                                     |
| `util`           | Catch-all settings, keybindings, and helpers, plus a major mode for BQL                        |
| `window-manager` | Window and frame navigation, splitting, and resizing                                           |
| `zz-internal`    | Bloomberg-only tap; a separate repo, absent on public machines                                 |

Every tap has its own `README.md` with the details. `common` is loaded
explicitly by the taps that need it rather than by Exordium, since it has no
`after-init.el`.

## The org repo

Parts of the `ai` and `org-mode` taps address my notes repo, which is not on
every machine. Point the config at it with:

```sh
export ORG_REPO_DIR="$HOME/Documents/org"   # in ~/.zshrc
```

`common/before-init.el` turns this into `my/org-repo`, which is nil where the
repo is absent; the features built on it are then skipped rather than left
pointing at missing files. See
[org-mode/ARCHITECTURE.md](org-mode/ARCHITECTURE.md#the-org-repo-gate) for why
a shell export alone is not enough for a GUI Emacs.
