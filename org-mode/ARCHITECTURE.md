# Org Mode tap — code organization

How this tap is put together. For what the keys do, see [README.md](README.md).

## Entry point and load order

Exordium collects the tap files at startup (`init.el`), then loads them in two passes:

```
before-init:  ~/.emacs.d/before-init.el        (machine-local, gitignored)
              taps/common/before-init.el
              ... one per tap, alphabetically

  [ Exordium's own modules and themes ]

after-init:   ~/.emacs.d/emacs-custom.el
              taps/ai/after-init.el
              taps/lsp/after-init.el
              taps/markdown-mode/after-init.el
              taps/org-mode/after-init.el      <- this tap
              ... alphabetically, zz-internal last
```

Two consequences the code relies on:

- **Every `before-init.el` runs before every `after-init.el`**, whatever the tap. That is
  why `my/org-repo` is defined in `taps/common/before-init.el` and can be read both here
  and in the `ai` tap, which loads earlier in the after-init pass.
- The machine-local `~/.emacs.d/before-init.el` runs first of all, so it can influence
  everything below it.

`after-init.el` is the only file Exordium loads directly. Everything else in this
directory is pulled in by an explicit `load-file` from it, with an absolute path. The
sub-modules are configuration fragments, not libraries: no `provide`, no `require` of
each other, loaded once for effect.

## Two layers

The tap has a hard split, and it is the main thing to understand before editing:

| Layer | Applies to | Loaded |
| ------------------- | ---------------------------------------- | ------------------ |
| **Org Mode itself** | any Org buffer, on any machine           | always |
| **The second brain**| my notes repo (`todo.org`, `catchup.org`, `areas/`, `projects/`, `resources/`) | only where the repo exists |

Everything in `after-init.el` above the `;;; The second brain` section at the bottom is
layer one. The bottom section is a single conditional that loads layer two:

```elisp
(if my/org-repo
    (progn
      (load-file ".../org-notes.el")
      (load-file ".../org-capture-refile.el")
      (load-file ".../org-drive-sync.el")
      (load-file ".../org-drqs.el"))
  (message "org-mode tap: no org repo on this machine (set ORG_REPO_DIR), ..."))
```

On a machine without the repo, Org Mode still gets its keys, pills, preview and
exporters; the notes browser, the capture templates, the agenda, the Drive sync and the
DRQS links simply never come into existence, and `*Messages*` says so once.

## Files

### Layer one — Org Mode, everywhere

| File | Lines | What |
| --------------------- | ----: | ------------------------------------------------ |
| `after-init.el`       |   379 | entry point: Org settings, faces, capture UX, backtick syntax, Calfw, Mermaid, per-file line numbers, and the `load-file` calls for everything below |
| `org-util.el`         |   122 | the `C-c o` prefix and the small commands behind it: TODO toggle, Jira link, image and Mermaid insertion, `:ARCHIVE:` tagging, `super-<arrow>` navigation |
| `org-svg-tags.el`     |   181 | `svg-tag-mode` rules — TODO pills, priorities, dates, `{:tags:}`, `((pills))`, progress cookies — plus the on-the-fly pills for per-file `#+todo:` keywords |
| `org-preview.el`      |   289 | live HTML preview in an xwidget (`C-c o v` / `C-c o V`), sharing the Markdown tap's Catppuccin CSS |
| `org-capture-frame.el`|   147 | the desktop-wide capture frame, driven from Hammerspoon via `emacsclient` |
| `org-to-markdown.el`  |   150 | `M-x org-to-markdown`, a pure-Elisp region converter |
| `org-modern-indent.el`|   370 | **vendored** copy of jdtsmith's `org-modern-indent` v0.5.1, not my code |

### Layer two — the second brain, conditional

| File | Lines | What |
| ----------------------- | ----: | ---------------------------------------------- |
| `org-notes.el`          |   154 | where the notes live and how to open them: the <kbd>F12</kbd> picker over `areas/`, `projects/`, `resources/`, the <kbd>S-F12</kbd>/<kbd>C-F12</kbd> shortcuts, the `org:` link abbreviation, `org-agenda-files` |
| `org-capture-refile.el` |    83 | the write side: `org-default-notes-file`, the capture templates, the `C-c o t/w/b` refile commands |
| `org-catchup-people.el` |   462 | one section per person in `catchup.org`: the `C-c o p` / `C-c o c` pickers, the `C-c o n` move of open items into today's meeting note, and the per-person capture templates, generated from the file so a renamed heading cannot go stale |
| `org-drive-sync.el`     |    37 | hourly copy of `todo.org` and `catchup.org` to Google Drive, for PlainOrg on the phone |
| `org-drqs.el`           |    60 | `{DRQS 1234567}` as a clickable link. Bloomberg-specific; gated on the repo because that is where the work notes are |

`org-capture-refile.el` is the only one that loads another sub-module
(`org-catchup-people.el`), because it needs its templates.

### Tests

`org-to-markdown.t.el` (33 `ert-deftest`s) is never loaded at startup. Run it by hand:
`M-x eval-buffer` in the test file, then `M-x ert`.

### Outside the tap

| File                               | What                                                  |
| ---------------------------------- | ----------------------------------------------------- |
| `taps/common/before-init.el`       | defines `my/org-repo` and `my/org-file`               |
| `taps/common/table-format.el`      | `C-c o T` / `C-c o R`, shared with the Markdown tap   |
| `taps/common/description-field.el` | `C-c o #`, shared with the Markdown tap               |
| `~/.emacs.d/before-init.el`        | machine-local, gitignored; the `ORG_REPO_DIR` stopgap |
| `~/dotfiles/init.lua`              | Hammerspoon; binds the desktop capture hotkey         |

## The org repo gate

The notes repo is not on every machine, so its location is not written down anywhere in
the configuration. It comes from the `ORG_REPO_DIR` environment variable:

```sh
export ORG_REPO_DIR="$HOME/Documents/org"   # in ~/.zshrc
```

`taps/common/before-init.el` turns it into two things every consumer uses:

```elisp
(defconst my/org-repo ...)     ; absolute, slash-terminated -- or nil
(defun my/org-file (relative)) ; path inside the repo, or a user-error
```

`my/org-repo` is nil when the variable is unset **and** when it points at a directory
that does not exist, so a stale export cannot half-enable the features. `my/org-file`
signals rather than returning a bogus path, so a command reached through a stale binding
fails loudly instead of quietly creating a file somewhere unexpected.

Consumers: this tap's `after-init.el` gates the four layer-two files on `my/org-repo`,
and the `ai` tap gates `org-skills.el` the same way (its `org-skills-repo` defaults to
`my/org-repo`). Nothing else in the configuration mentions the path.

### Why `before-init` and not just the shell

A GUI Emacs started from Finder or the Dock inherits no shell environment, so
`$ORG_REPO_DIR` is simply absent there. Exordium does run `exec-path-from-shell`, but on
`after-init-hook` — long after the taps have loaded — and by default it only imports
`PATH` and `MANPATH`. So the shell export alone is not enough.

The stopgap is `~/.emacs.d/before-init.el`, which is machine-local, gitignored, and the
very first file Exordium loads:

```elisp
(unless (getenv "ORG_REPO_DIR")
  (setenv "ORG_REPO_DIR" "~/Documents/org/"))
```

The `unless` matters: the shell stays authoritative when it has something to say, and
this only fills the gap when Emacs was launched without an environment.

## Conventions

- Sub-modules are named `org-<topic>.el` and start with a `;;;;` title line, a `;;;`
  commentary block listing their key bindings, and a `-*- lexical-binding: t -*-` cookie.
- They end with `;;; <name>.el ends here` and a `Local Variables:` block disabling
  `emacs-lisp-checkdoc`, matching the rest of the taps.
- Interactive commands are prefixed `my/` (a few older ones use `my-`); everything in
  this tap that is not upstream code carries one of those prefixes.
- `after-init.el` separates its sections with form feeds (`^L`, `C-q C-l`). They are
  invisible in most diff viewers — mind them when moving blocks around.
- A section moved out to its own file leaves its heading behind in `after-init.el`, with
  a one-line pointer, so the file still reads in the original order.
