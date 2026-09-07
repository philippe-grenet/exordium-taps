# Dashboard

<kbd>M-x</kbd> `dashboard` opens a read-only overview of the second-brain org repo in a
buffer of its own, `*dashboard*`. It is a launcher: <kbd>RET</kbd> on almost any line
opens what it names, closing the dashboard and leaving the target in its place.

Only the projects are drawn as links. The tasks and the agenda lines carry a
`my-dashboard-target` text property instead — <kbd>RET</kbd> opens them just the same,
but they read as a list rather than as a page of underlined links, and <kbd>TAB</kbd>
does not stop on them.

On a graphical frame it is also what Emacs starts on, in place of `*scratch*` — see
[At startup](#at-startup).

The whole tap addresses `my/org-repo` (see [../common/before-init.el](../common/before-init.el)),
so it is skipped entirely on a machine without the notes repo.

## What it shows

From top to bottom:

- **The Exordium logo**, scaled down and centred. Skipped in a terminal.
- **Today's date**, spelled out, with its ISO week number.
- **Calendar** — three months with the current one in the middle. This is the same
  display `org-time-stamp` pops up, drawn by `calendar-generate`, with ISO week numbers
  down the left margin and today highlighted.
- **Agenda** — `org-agenda-list` over the next 21 days, with the empty days dropped.
  Each line stays linked to the entry it came from.
- **This week** — the tasks worth caring about, read out of `todo.org`. TODO keywords
  and priority cookies are drawn as the same SVG pills an Org buffer shows.
- **Recent projects** — the seven entries under `projects/` that git says were touched
  most recently, newest first, with how long ago.
- **The keys**, in small dim text, from `my-dashboard-keys`.

Point is left at the end of the buffer after a redraw, so `hl-line-mode` lands on the
key reminder rather than drawing a band across the logo. The rules between sections are
stretch spaces reaching the right edge of the window — their line is the underline of
`my-dashboard-separator` — so they stay exact when the window is resized, which a run of
box-drawing characters would not.

### How the tasks are picked

`todo.org` is a set of top-level sections, one task per second-level heading. Two rules
decide what reaches the dashboard, and done tasks are dropped in both:

1. Every task in the **first block** of the `Week` section. That section is written as
   blocks separated by blank lines, with older concerns further down; only the first
   block is this week's.
2. Every task in the `Today` or `Week` sections carrying a **priority cookie**, a ⭐
   (important) or a 🔥 (urgent), wherever it sits in the section.

Tasks flagged with ⭐ or 🔥 are shown in `my-dashboard-important`.

The pills come from the rules in `svg-tag-tags`, which
[../org-mode/org-svg-tags.el](../org-mode/org-svg-tags.el) sets up — so a keyword looks
the same here as it does in an Org buffer, and a keyword with no rule (or a rule naming
a face that does not exist) simply falls back to plain coloured text, as it does in a
terminal. Titles are aligned past the pills with a stretch space, since an image's width
has nothing to do with the length of the text behind it.

### How the projects are picked

`projects/` holds one project per entry — either a single file (`23x5-trading.org`) or a
directory (`metadata/`). Which ones are live is a question git answers better than the
filesystem does, since mtimes move for reasons unrelated to working on something, so the
ordering comes from `git log`. A project links to its `README.org`/`README.md` when it
has one, and otherwise to whichever of its files was committed last. Projects renamed or
deleted since their last commit are skipped.

## Keys

| Key                  | Effect                                              |
| -------------------- | --------------------------------------------------- |
| <kbd>RET</kbd>       | Open the task, agenda entry or project at point     |
| <kbd>TAB</kbd>       | Next project link                                   |
| <kbd>S-TAB</kbd>     | Previous project link                               |
| <kbd>n</kbd> / <kbd>p</kbd> | Next / previous line                         |
| <kbd>g</kbd>         | Redraw                                              |
| <kbd>t</kbd>         | Close, and open `todo.org`                          |
| <kbd>c</kbd>         | Close, and open `catchup.org`                       |
| <kbd>r</kbd>         | Close, and open `roadmap.org`                       |
| <kbd>a</kbd>         | Close, and open the org agenda                      |
| <kbd>q</kbd>         | Close                                               |

<kbd>t</kbd> and <kbd>c</kbd> call `open-todo-file` and `open-catchup-file` from the
`org-mode` tap, and fall back to the paths themselves where that tap is absent.

## At startup

On a graphical frame, Emacs starts on the dashboard instead of `*scratch*`. It stands
down in three cases, so it never gets in the way of what you actually asked for:

- in a terminal (`emacs -nw`), where `*scratch*` is shown as before;
- when files were named on the command line, or a desktop was restored — anything that
  has already put a file in the window;
- when `my-dashboard-at-startup` is nil.

This costs one global setting: the tap sets `inhibit-startup-screen`. `command-line-1`
runs `emacs-startup-hook` *before* putting up the GNU Emacs startup screen, and the
screen would then replace whatever the hook had displayed; with the screen inhibited,
the hook is left to `normal-top-level`, which runs it once everything is on screen. The
visible side effect is that a terminal Emacs now starts on `*scratch*` rather than on
the splash.

`emacs-startup-hook` is used rather than `initial-buffer-choice` precisely because it
can look at what is already displayed. `initial-buffer-choice` cannot: whatever its
function returns *is* displayed, so a file named on the command line would land in a
split beside the dashboard, with the dashboard focused.

Building the agenda is what the dashboard spends its time on — around 1.5s on a cold
start, on top of Emacs's own init. Set `my-dashboard-at-startup` to nil (from
`~/.emacs.d/before-init.el`, which is early enough to count) if that is not worth it.

## Settings

All of these are `M-x customize-group` `my-dashboard`:

| Variable                    | Default              | Meaning                            |
| --------------------------- | -------------------- | ---------------------------------- |
| `my-dashboard-at-startup`   | `t`                  | Start on the dashboard, in a GUI   |
| `my-dashboard-logo`         | `Exordium-mocha.png` | Image at the top, or nil for none  |
| `my-dashboard-logo-width`   | `520`                | Logo width in pixels               |
| `my-dashboard-agenda-days`  | `21`                 | Days of agenda to show             |
| `my-dashboard-projects`     | `7`                  | Projects to list                   |
| `my-dashboard-keys`         | see below            | Keys named in the footer           |

`my-dashboard-keys` is an alist of (KEY . WHAT) kept separate from
`dashboard-mode-map`, so the footer can stay short; edit both when you add a binding
worth advertising.

## Files

| File                        | Contents                                            |
| --------------------------- | --------------------------------------------------- |
| `my-dashboard.el`           | Faces, layout helpers, the major mode and its keys  |
| `my-dashboard-calendar.el`  | The calendar and agenda sections                    |
| `my-dashboard-tasks.el`     | Reading this week's tasks out of `todo.org`         |
| `my-dashboard-projects.el`  | Asking git which projects are live                  |

## Notes

Two things about org's agenda are worth knowing, since both took a fix:

- `org-agenda-list` overwrites `org-agenda-buffer-name`. Naming the throwaway buffer —
  and so leaving any agenda of your own alone — is done with `org-agenda-buffer-tmp-name`.
- `org-agenda-mode` resets its markers from `kill-buffer-hook`, so the `org-marker`
  properties on the copied agenda lines are dead by the time you would follow one. The
  dashboard copies each line's file and position out into a property of its own while
  the agenda buffer is still alive.
