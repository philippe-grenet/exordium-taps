# Emacs dashboard tap

Create a new tap in this directory that implements a dashboard. Similar example (and potential reuse of code):
https://github.com/emacs-dashboard/dashboard

The dashboard should open with M-x `dashboard`. Once it works well, we'll make it open
as the first buffer once Emacs starts (in GUI mode only).

## Context

The idea is to create a basic dashboard for my "second brain", which is the repository
in `$ORG_REPO_DIR`. Read the CLAUDE.md in this directory to get the context.

The information that is relevant to display:
- The calendar: what I have scheduled for this week and the next 2.
- The important work items I need to take care of this week. Those are in `todo.org` in
  the `** Week` section. Note that in this section includes blocks of separated by empty
  lines; only the first block should be picked. Also include anything in the Today and
  the Week sections that are marked as priority or that includes an emoji ⭐️ (which
  means important) or 🔥 (which means urgent).
- The 5 most recent projects I am working on, with links to files. Git can tell which ones.

## What it should display

In vertical order:
- The Exordium logo, in file `Exordium-mocha.png`
- Today's date spelled out, including the week number. You can make it a bit bigger than the rest of the text.
- Horizontal separator.
- Calendar section:
  - A calendar with 3 months, with current month centered. This should be displayed in
    the same style as `org-timestamp`.
  - List of items from org agenda for the next 3 weeks
- Horizontal separator.
- Project section:
  - List of 7 most-recently updated projects, with links to files.
- Horizontal separator.

## Keys

The dashboard should have its own major mode with these keys:

- q: quit, close the buffer.
- t: close buffer and `open-todo-file`.
- c: close buffer and `open-catchup-file`

Feel free to suggest more.
