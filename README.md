# panel

A small Emacs startup panel focused on recent files, startup info, and optional weather.

![Screenshot](./screenshots/screenshot_1.png)

## What it does

- Shows up to 9 recent files with icons and numeric shortcuts.
- Shows a terminal-friendly ASCII intro block by default in TTY Emacs.
- Highlights the `[n]` shortcut for the current line.
- Opens the current line with `RET` or `o`.
- Shows startup time and loaded package count.
- Optionally shows current weather from Open-Meteo.
- Optionally shows a centered PNG image.
- Refreshes with `g` or `r`.

## Requirements

- Emacs 27.1+
- [`plz`](https://github.com/alphapapa/plz)
- [`nerd-icons`](https://github.com/rainstormstudio/nerd-icons.el) for file and status icons (optional but recommended)

`recentf` is built in.

## Installation

### `use-package` with `straight.el`

```elisp
(use-package panel
  :straight (:host github
             :repo "LuciusChen/panel")
  :init
  (setq panel-title "Quick access [C-number to open file]"
        panel-min-left-padding 10
        panel-path-max-length 72)
  :config
  (panel-create-hook))
```

### Local checkout

```elisp
(add-to-list 'load-path "/path/to/panel")
(require 'panel)
(panel-create-hook)
```

## Configuration

### Basic

```elisp
(setq panel-title "Quick access [C-number to open file]"
      panel-min-left-padding 10
      panel-path-max-length 72
      panel-intro-display 'tty
      panel-intro-horizontal-offset -2
      panel-use-icons t)
```

### Terminal intro

The intro block is rendered with plain text, so it works in terminal Emacs without image support.

```elisp
(setq panel-intro-display 'tty
      panel-intro-lines
      '("┌─┐ ┌┬┐ ┌─┐ ┌─┐ ┌─┐"
        "├┤  │││ ├─┤ │   └─┐"
        "└─┘ ┴ ┴ ┴ ┴ └─┘ └─┘")
      panel-intro-help-lines
      '(("C-x C-f" . "find a file")
        ("C-h t" . "start the Emacs tutorial")
        ("q" . "close this panel")))
```

- `panel-intro-display`: show the intro in `tty`, `always`, or `never`.
- `panel-intro-lines`: override the ASCII logo lines.
- `panel-intro-horizontal-offset`: shift the top header horizontally. Affects both the TTY intro and the graphical image.
- `panel-intro-help-lines`: override the key hints shown below the logo.
- `panel-use-icons`: keep using `nerd-icons` when available, including in terminal Emacs.

### Weather

Set both coordinates to enable weather. Negative values are valid.

```elisp
(setq panel-latitude 31.2304
      panel-longitude 121.4737
      panel-weather-update-interval 900
      panel-weather-cache-duration 900
      panel-weather-max-retries 3)
```

Weather is fetched from Open-Meteo and refreshed on a timer. Cached data is reused while valid.

### Image

If `panel-image-file` points to an existing PNG file, it is shown above the recent-files list in graphical Emacs.

```elisp
(setq panel-image-file "~/Pictures/panel.png"
      panel-image-width 200
      panel-image-height 200)
```

## Keybindings

- `RET`: open the recent file on the current line
- `o`: open the recent file on the current line
- `1`..`9`: open recent file by index
- `M-s-1`..`M-s-9`: open recent file by index
- `g`: refresh panel
- `r`: refresh panel

## Customization variables

- `panel-title`
- `panel-min-left-padding`
- `panel-path-max-length`
- `panel-latitude`
- `panel-longitude`
- `panel-image-file`
- `panel-image-width`
- `panel-image-height`
- `panel-weather-update-interval`
- `panel-weather-cache-duration`
- `panel-weather-max-retries`
- `panel-show-file-path`
- `panel-intro-display`
- `panel-intro-lines`
- `panel-intro-horizontal-offset`
- `panel-intro-help-lines`
- `panel-use-icons`

## Notes

- The panel buffer is `*welcome*`.
- Set `panel-show-file-path` to nil to hide directory prefixes in recent-file entries.
- Missing files are shown in the list and opening them reports a user error instead of creating a new buffer.

## Commands

- `M-x panel-create-hook`
- `M-x panel-refresh`
