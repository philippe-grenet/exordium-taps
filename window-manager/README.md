# Window and Frame Management

This document describes all key bindings related to window and frame management in this Emacs configuration.

## Window Navigation

### Move Cursor Between Windows

| Key Binding    | Command                  | Description                                                                                                                        |
|----------------|--------------------------|------------------------------------------------------------------------------------------------------------------------------------|
| `C-c <left>`   | `windmove-left`          | Move cursor to the window on the left                                                                                              |
| `C-c <right>`  | `windmove-right`         | Move cursor to the window on the right                                                                                             |
| `C-c <up>`     | `windmove-up`            | Move cursor to the window above                                                                                                    |
| `C-c <down>`   | `windmove-down`          | Move cursor to the window below                                                                                                    |
| `M-o <letter>` | `ace-window`             | Display a letter in each window and jump by typing the letter. With 2 windows, cycles between them. With prefix arg, swaps windows |
| `C-c t t`      | `treemacs-select-window` | Jump to treemacs window                                                                                                            |

*Defined in:* `modules/init-window-manager.el:150-158`, `modules/init-window-manager.el:192`, `taps/treemacs/after-init.el:16`

## Window Manipulation

### Move and Swap Windows

| Key Binding     | Command             | Description                                      |
|-----------------|---------------------|--------------------------------------------------|
| `C-c S-<left>`  | `move-buffer-left`  | Swap current window with the window on the left  |
| `C-c S-<right>` | `move-buffer-right` | Swap current window with the window on the right |
| `C-c S-<up>`    | `move-buffer-up`    | Swap current window with the window above        |
| `C-c S-<down>`  | `move-buffer-down`  | Swap current window with the window below        |

*Defined in:* `modules/init-window-manager.el:155-158`

### Split Windows

| Key Binding | Command                             | Description                                                     |
|-------------|-------------------------------------|-----------------------------------------------------------------|
| `C-c 2`     | `split-window-vertically-instead`   | Delete other windows, split vertically, and show other buffer   |
| `C-c 3`     | `split-window-horizontally-instead` | Delete other windows, split horizontally, and show other buffer |

*Defined in:* `taps/window-manager/after-init.el:136-137`

Note: There's also a `toggle-window-split` function defined (line 83) that switches
window split from horizontal to vertical or vice versa, but it's not bound to a key by
default.

### Window Dedication

| Key Binding | Command                            | Description                                                                                        |
|-------------|------------------------------------|----------------------------------------------------------------------------------------------------|
| `C-c w d`   | `exordium-toggle-window-dedicated` | Toggle whether the current window is dedicated (prevents Emacs from reusing it for another buffer) |

*Defined in:* `taps/window-manager/after-init.el:140` (originally in
`modules/init-window-manager.el:173` but bound to `<pause>` which doesn't exist on Mac
keyboards)

## Frame Management

### Frame Layouts

| Key Binding | Command                  | Description                                                     |
|-------------|--------------------------|-----------------------------------------------------------------|
| `F10`       | `frame-show-one-window`  | Show a single window, resize frame to width 120                 |
| `Shift-F10` | `frame-show-two-windows` | Show two windows side by side, resize frame to width 240        |
| `Meta-F10`  | `frame-show-component`   | Show all 3 files for a C++ component (.h, .cpp, .t), fullscreen |

*Defined in:* `taps/window-manager/after-init.el:21,35,50`

**Note:** F10 is also bound to `next-error` in `init-prog-mode.el:342`, which may conflict with the window manager binding.

### Fullscreen and Distraction-Free Mode

| Key Binding | Command                 | Description                                                      |
|-------------|-------------------------|------------------------------------------------------------------|
| `Shift-F11` | `distraction-free-mode` | Toggle darkroom mode and fullscreen for distraction-free writing |

*Defined in:* `taps/window-manager/after-init.el:61`

There's also a `toggle-fullscreen` function defined in `modules/init-osx.el:18-23` but it's not bound to a key by default.

## Buffer Management

| Key Binding | Command                  | Description                                                   |
|-------------|--------------------------|---------------------------------------------------------------|
| `M-C-l`     | `switch-to-other-buffer` | Switch to the most recently used buffer in the current window |
| `C-``       | `kill-current-buffer`    | Kill the current buffer                                       |

*Defined in:* `modules/init-look-and-feel.el:292,284`

There's also a `kill-other-buffers` function defined in
`taps/window-manager/after-init.el:143` that kills all buffers except the current one,
but it's not bound to a key by default.

## Ace-window Configuration

The ace-window package is configured with:
- Keys `a s d f g h j k l` (home row) for window selection in the base modules
- Keys `1 2 3 4 5 6 7 8 9` for window selection in the window-manager tap (line 68)
- Scope set to current frame only
- Posframe mode enabled when available (shows window numbers in overlays)
- Large red numbers for the Catppuccin Mocha theme

*Defined in:* `modules/init-window-manager.el:179-196`, `taps/window-manager/after-init.el:66-74`

## Quick Reference

### Common Workflows

**Navigate between windows:**
- `C-c <arrow>` - Move cursor directionally
- `M-o <letter>` - Jump to specific window by letter

**Rearrange windows:**
- `C-c S-<arrow>` - Swap windows directionally
- `C-c 2` / `C-c 3` - Reset to 2 windows (vertical/horizontal)
- `C-c w d` - Toggle window dedication (lock buffer to window)

**Frame layouts:**
- `F10` - Single window
- `Shift-F10` - Two windows side by side
- `Shift-F11` - Distraction-free fullscreen

**Switch buffers:**
- `M-C-l` - Most recent buffer
- `C-`` - Kill current buffer
