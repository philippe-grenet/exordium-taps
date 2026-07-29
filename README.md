# Exordium taps

Local extensions to [Exordium](https://github.com/philippe-grenet/exordium) (e.g. Bloomberg-specific stuff).

## Org mode

Keybinding           | Description
---------------------|---------------------------------------------------------
<kbd>F12</kbd>       | Open TODOs
<kbd>S-F12</kbd>     | Open HIRE
<kbd>C-F12</kbd>     | Capture (today, week, next, team meeting)
<kbd>C-c t</kbd>     | Move task to today
<kbd>C-c o #</kbd>   | Insert/update the file description (`#+description:`)

## Markdown mode

Keybinding           | Description
---------------------|---------------------------------------------------------
<kbd>S-$</kbd>       | Look for word in the MacOS dictionary app
<kbd>C-c m #</kbd>   | Insert/update the file description (YAML frontmatter)

Functions:

- M-x `straighten-quotes`: replace non-ascii quotes in the region.
- M-x `my/update-description`: insert/update a one-line file description (Org `#+description:` or Markdown frontmatter, by major mode).

## Utilities

Functions:

- M-x `repunctuate`: BDE style (2 spaces between sentences).
- M-x `depunctuate`: normal style (1 space between sentences).
- M-x `scratch-msg`: create a scratch buffer in markdown mode.

Keybinding           | Description
---------------------|---------------------------------------------------------
<kbd>F6</kbd>        | Highlight symbol
<kbd>F8</kbd>        | UTF symbol: *, c(heck), l(ove), u(brella), &rarr;

## Window manager

Keybinding           | Description
---------------------|---------------------------------------------------------
<kbd>F9</kbd>        | One window
<kbd>F10</kbd>       | Two windows
<kbd>S-F10</kbd>     | Show complete component

Functions:

- M-x `toggle-window-split`: switch windows from horizontally to vertically, and vice-versa.
- M-x `resplit-vertically`: switch window split from horizontal to vertical, resizing the frame as well.
- M-x `kill-other-buffers`: kill all other buffers.
