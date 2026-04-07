;;; panel.el --- Simple welcome-panel screen -*- lexical-binding: t -*-

;; Welcome-panel screen

;; Author: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Maintainer: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Created: 2023
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (plz "0.7"))
;; Homepage: https://github.com/konrad1977/welcome-panel

;;; Commentary:

;;; Minimalistic panel for Emacs.

(require 'cl-lib)
(require 'json)
(require 'plz)
(require 'recentf)
(require 'subr-x)

(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function nerd-icons-icon-for-dir "nerd-icons")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-wicon "nerd-icons")

;;; Code:

(defgroup panel nil
  "Panel group."
  :group 'applications)

(defvar panel-mode nil)
(defvar panel-recentfiles '() "Recent list.")
(defvar panel-temperature nil)
(defvar panel-weatherdescription nil)
(defvar panel-weathericon nil)
(defvar panel--weather-error-message nil
  "Last weather error message, if any.")

(defvar panel--weather-timer nil
  "Timer for periodic weather updates.")

(defvar panel--weather-fetch-in-progress nil
  "Flag to prevent concurrent weather fetches.")

(defvar panel--resize-timer nil
  "Timer for debouncing resize events.")

(defvar panel--last-weather-update nil
  "Timestamp of last successful weather update.")

(defvar panel--weather-retry-count 0
  "Number of retry attempts for weather fetch.")

(defvar panel--cached-image nil
  "Cached image object.")

(defcustom panel-title "Quick access [C-number to open file]"
  "Panel title."
  :group 'panel
  :type 'string)

(defcustom panel-show-file-path t
  "Show file path in welcome-panel."
  :group 'panel
  :type 'boolean)

(defcustom panel-min-left-padding 10
  "Minimum left padding when resizing window."
  :group 'panel
  :type 'natnum)

(defcustom panel-path-max-length 72
  "Maximum path length for display."
  :group 'panel
  :type 'natnum)

(defcustom panel-latitude nil
  "Latitude for weather information."
  :group 'panel
  :type 'float)

(defcustom panel-longitude nil
  "Longitude for weather information in panel package."
  :group 'panel
  :type 'float)

(defcustom panel-image-file ""
  "Image file in panel package."
  :group 'panel
  :type 'file)

(defcustom panel-image-width 200
  "Image width for weather information."
  :group 'panel
  :type 'natnum)

(defcustom panel-image-height 200
  "Image height for weather information."
  :group 'panel
  :type 'natnum)

(defcustom panel-intro-display 'tty
  "When to show the intro block."
  :group 'panel
  :type '(choice (const :tag "Only in terminal frames" tty)
                 (const :tag "Always" always)
                 (const :tag "Never" never)))

(defcustom panel-intro-lines nil
  "Lines used for the optional intro block.

When nil, use the built-in Emacs ASCII header."
  :group 'panel
  :type '(repeat string))

(defcustom panel-intro-horizontal-offset -2
  "Horizontal offset applied to the panel header.

Negative values move the image or intro left; positive values move it right."
  :group 'panel
  :type 'integer)

(defcustom panel-intro-help-lines nil
  "Key hints shown below the intro block.

Each entry is a cons cell of the form (KEY . DESCRIPTION)."
  :group 'panel
  :type '(repeat (cons (string :tag "Key")
                       (string :tag "Description"))))

(defcustom panel-use-icons t
  "Whether to use `nerd-icons' when available."
  :group 'panel
  :type 'boolean)

(defcustom panel-weather-update-interval 900
  "Interval in seconds between weather updates."
  :group 'panel
  :type 'integer)

(defcustom panel-weather-cache-duration 900
  "Weather cache duration in seconds."
  :group 'panel
  :type 'integer)

(defcustom panel-weather-max-retries 3
  "Maximum number of retry attempts for weather fetch."
  :group 'panel
  :type 'integer)

(defcustom panel-weather-api-base-url "https://api.open-meteo.com/v1/forecast"
  "Base URL for weather requests."
  :group 'panel
  :type 'string)

(defconst panel-buffer "*welcome*"
  "Panel buffer name.")

(defvar panel--file-icon-cache (make-hash-table :test 'equal)
  "Cache for file icons.")

(defvar-local panel--padding-cache nil
  "Cache for padding in the panel buffer.")

(defvar-local panel--last-window-width nil
  "Last window width in the panel buffer.")

(defvar-local panel--shortcut-overlay nil
  "Overlay used to highlight the shortcut on the current line.")

(defvar panel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)

    (define-key map (kbd "RET") 'panel--open-recent-file)
    (define-key map (kbd "o") 'panel--open-recent-file)
    (define-key map (kbd "g") 'panel-refresh)
    (define-key map (kbd "r") 'panel-refresh)

    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (number-to-string i))
                  `(lambda ()
                     (interactive)
                     (panel--open-recent-file-at-index ,i)))
      (define-key map (kbd (concat "M-s-" (number-to-string i)))
                  `(lambda ()
                     (interactive)
                     (panel--open-recent-file-at-index ,i))))

    map)
  "Keymap for `panel-mode'.")

(define-derived-mode panel-mode special-mode "dashboard"
  "Major mode for the welcome-panel screen."
  :group 'panel
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (unless (overlayp panel--shortcut-overlay)
    (setq-local panel--shortcut-overlay (make-overlay (point-min) (point-min) nil t nil))
    (overlay-put panel--shortcut-overlay 'face 'panel-shortcut-current-face))
  (add-hook 'post-command-hook #'panel--update-shortcut-highlight nil t)
  (setq-local cursor-type nil))

(defface panel-title-face
  '((t :inherit font-lock-constant-face :height 1.3 :italic t))
  "Title face."
  :group 'panel)

(defface panel-subtitle-face
  '((t :foreground "#9399b2"))
  "Subtitle face."
  :group 'panel)

(defface panel-intro-face
  '((t :inherit panel-title-face :italic nil))
  "Face for the intro logo."
  :group 'panel)

(defface panel-intro-version-face
  '((t :inherit panel-time-face :weight normal))
  "Face for the intro version line."
  :group 'panel)

(defface panel-intro-key-face
  '((t :inherit font-lock-constant-face :height 0.9 :bold t))
  "Face for key hints in the intro block."
  :group 'panel)

(defface panel-separator-face
  '((t :inherit font-lock-comment-face))
  "Separator face."
  :group 'panel)

(defface panel-info-face
  '((t :inherit font-lock-property-name-face :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'panel)

(defface panel-text-info-face
  '((t :inherit default :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'panel)

(defface panel-path-face
  '((t :inherit font-lock-comment-face :height 0.9 :weight thin :bold nil :italic nil))
  "Face for the file path."
  :group 'panel)

(defface panel-filename-face
  '((t :inherit default :weight semi-bold))
  "Face for the file name."
  :group 'panel)

(defface panel-time-face
  '((t :inherit font-lock-comment-face :height 0.9 :weight thin))
  "Face for time."
  :group 'panel)

(defface panel-weather-description-face
  '((t :foreground "#E2943B" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for weather description."
  :group 'panel)

(defface panel-startup-time-face
  '((t :foreground "#ab82f7" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for startup time."
  :group 'panel)

(defface panel-shortcut-face
  '((t :inherit font-lock-constant-face :height 0.9 :bold t))
  "Face for recent files shortcuts."
  :group 'panel)

(defface panel-shortcut-current-face
  '((t :inherit panel-shortcut-face :inverse-video t))
  "Face for the current line shortcut in the panel buffer."
  :group 'panel)

(defface panel-weather-icon-face
  '((t :height 0.9))
  "Face for weather icon."
  :group 'panel)

(defface panel-weather-temperature-face
  '((t :foreground "#f38ba8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for temperature."
  :group 'panel)

(defconst panel--default-intro-lines
  '("┌─┐ ┌┬┐ ┌─┐ ┌─┐ ┌─┐"
    "├┤  │││ ├─┤ │   └─┐"
    "└─┘ ┴ ┴ ┴ ┴ └─┘ └─┘")
  "Default ASCII intro logo shown in terminal frames.")

(defun panel--nerd-icons-available-p ()
  "Return non-nil when `nerd-icons' can be used."
  (and panel-use-icons
       (require 'nerd-icons nil t)))

(defun panel--with-icon-fallback (fn icon fallback &rest args)
  "Call FN with ICON and ARGS, or return FALLBACK when icons are unavailable."
  (if (panel--nerd-icons-available-p)
      (apply fn icon args)
    fallback))

(defun panel--weather-icon-from-code (code)
  "Map weather CODE to a corresponding string."
  (panel--with-icon-fallback
   #'nerd-icons-wicon
   (pcase code
     (`0 "nf-weather-day_sunny")
     ((or `1 `2 `3) "nf-weather-cloudy")
     ((or `45 `48) "nf-weather-fog")
     ((or `51 `53 `55) "nf-weather-sleet")
     ((or `56 `57) "nf-weather-snow")
     ((or `61 `63 `65) "nf-weather-day_rain_mix")
     ((or `66 `67) "nf-weather-rain-mix")
     ((or `71 `73 `75) "nf-weather-snow")
     (`77 "nf-weather-snow")
     ((or `80 `81 `82) "nf-weather-rain")
     ((or `85 `86) "nf-weather-rain-mix")
     ((or `95 `96 `99) "nf-weather-thunderstorm")
     (_ "Unknown"))
   (pcase code
     (`0 "sun")
     ((or `1 `2 `3) "cloud")
     ((or `45 `48) "fog")
     ((or `51 `53 `55 `56 `57) "drizzle")
     ((or `61 `63 `65 `66 `67 `80 `81 `82) "rain")
     ((or `71 `73 `75 `77 `85 `86) "snow")
     ((or `95 `96 `99) "storm")
     (_ ""))))

(defun panel--block-width (lines)
  "Return the maximum display width of LINES."
  (if lines
      (apply #'max 0 (mapcar #'string-width lines))
    0))

(defun panel--block-left-padding (lines &optional offset)
  "Return the left padding needed to center LINES with OFFSET."
  (max 0 (+ (floor (/ (- (window-width) (panel--block-width lines)) 2))
            (or offset 0))))

(defun panel--insert-block (lines &optional offset)
  "Insert LINES as a centered block with OFFSET."
  (let ((left-padding (panel--block-left-padding lines offset)))
    (dolist (line lines)
      (insert (make-string left-padding ?\s))
      (insert line)
      (insert "\n"))))

(defun panel--intro-visible-p ()
  "Return non-nil when the intro block should be rendered."
  (pcase panel-intro-display
    ('always t)
    ('tty (not (display-graphic-p)))
    (_ nil)))

(defun panel--format-intro-help-lines ()
  "Build the formatted help lines shown in the intro block."
  (let* ((pairs panel-intro-help-lines)
         (key-width (if pairs
                        (apply #'max 0 (mapcar (lambda (pair)
                                                 (string-width (car pair)))
                                               pairs))
                      0)))
    (mapcar (lambda (pair)
              (concat
               (propertize "type " 'face 'panel-text-info-face)
               (propertize (format (format "%%-%ds" key-width) (car pair))
                           'face 'panel-intro-key-face)
               (propertize "  " 'face 'panel-text-info-face)
               (propertize (cdr pair) 'face 'panel-text-info-face)))
            pairs)))

(defun panel--build-intro-lines ()
  "Return the help lines used for the intro block."
  (let* ((logo-width (panel--block-width (or panel-intro-lines
                                             panel--default-intro-lines)))
         (help-lines (panel--format-intro-help-lines))
         (separator (when help-lines
                      (propertize (make-string (max logo-width
                                                    (panel--block-width help-lines))
                                               ?─)
                                  'face 'panel-separator-face))))
    (when help-lines
      (append
       (list separator)
       help-lines
       (list separator)))))

(defun panel--insert-intro ()
  "Insert the optional intro block."
  (when (panel--intro-visible-p)
    (let* ((raw-lines (or panel-intro-lines panel--default-intro-lines))
           (version (propertize
                     (format "v%d.%d" emacs-major-version emacs-minor-version)
                     'face 'panel-intro-version-face))
           (composed-lines
            (cl-loop for line in raw-lines
                     for index from 0
                     collect (if (= index (1- (length raw-lines)))
                                 (concat line "  " version)
                               line)))
           (left-padding (panel--block-left-padding composed-lines
                                                   panel-intro-horizontal-offset))
           (last-index (1- (length raw-lines)))
           (index 0))
      (dolist (line raw-lines)
        (insert (make-string left-padding ?\s))
        (insert
         (if (= index last-index)
             (concat (propertize line 'face 'panel-intro-face)
                     "  "
                     version)
           (propertize line 'face 'panel-intro-face)))
        (insert "\n")
        (setq index (1+ index))))
    (when-let* ((help-block (panel--build-intro-lines)))
      (insert "\n")
      (panel--insert-block help-block panel-intro-horizontal-offset))
    (insert "\n")))

(defun panel--weather-code-to-string (code)
  "Map weather CODE to a corresponding string."
  (pcase code
    (`0 "Clear sky")
    ((or `1 `2 `3) "Partly cloudy")
    ((or `45 `48) "Fog")
    ((or `51 `53 `55) "Drizzle")
    ((or `56 `57) "Freezing drizzle")
    ((or `61 `63 `65) "Rain")
    ((or `66 `67) "Freezing rain")
    ((or `71 `73 `75) "Snowfall")
    (`77 "Snow grains")
    ((or `80 `81 `82) "Rain showers")
    ((or `85 `86) "Snow showers")
    ((or `95 `96 `99) "Thunderstorm")
    (_ "Unknown")))

(defun panel--insert-centered (text)
  "Insert TEXT at the center of the current line."
  (let ((padding (max 0 (floor (/ (- (window-width) (string-width text)) 2)))))
    (insert (make-string padding ?\s))
    (insert text)))

(defun panel--ensure-recentf ()
  "Ensure `recentf' is initialized and loaded."
  (unless recentf-mode
    (recentf-mode 1))
  (unless (bound-and-true-p recentf-list)
    (setq recentf-list nil))
  (when (and (boundp 'recentf-list)
             (not recentf-list))
    (recentf-load-list)))

(defun panel--file-icon (file)
  "Return the icon for FILE."
  (or (gethash file panel--file-icon-cache)
      (puthash file
               (cond ((not (file-exists-p file))
                      (panel--with-icon-fallback
                       #'nerd-icons-mdicon
                       "nf-md-file_remove"
                       (propertize "!" 'face 'error)
                       :face '(:inherit nerd-icons-red)))
                     ((file-directory-p file)
                      (panel--with-icon-fallback
                       #'nerd-icons-icon-for-dir
                       file
                       (propertize "/" 'face 'dired-directory)))
                     (t
                      (panel--with-icon-fallback
                       #'nerd-icons-icon-for-file
                       file
                       (propertize "-" 'face 'shadow))))
               panel--file-icon-cache)))

(defun panel--string-suffix-to-width (text width)
  "Return the widest suffix of TEXT that fits in WIDTH columns."
  (let ((start 0))
    (while (and (< start (length text))
                (> (string-width (substring text start)) width))
      (setq start (1+ start)))
    (substring text start)))

(defun panel--update-shortcut-highlight ()
  "Highlight the shortcut on the current line."
  (when (overlayp panel--shortcut-overlay)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward " \\[[1-9]\\]" (line-end-position) t)
          (move-overlay panel--shortcut-overlay
                        (1+ (match-beginning 0))
                        (match-end 0))
        (move-overlay panel--shortcut-overlay (point-min) (point-min))))))

(defun panel--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let ((pos (line-beginning-position))
        (line-end (line-end-position))
        file)
    (while (and (< pos line-end)
                (not (setq file (get-text-property pos 'path))))
      (setq pos (1+ pos)))
    (if file
      (if (file-exists-p file)
          (find-file file)
        (user-error "File does not exist: %s" file))
      (user-error "No recent file on this line"))))

(defun panel--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files panel-recentfiles))
    (when (<= 1 index (length files))
      (let ((file (nth (1- index) files)))
        (if (file-exists-p file)
            (find-file file)
          (user-error "File does not exist: %s" file))))))

(defun panel--truncate-path-in-middle (path n)
  "Truncate the middle of PATH to length N with an ellipsis."
  (if (<= (string-width path) n)
      path
    (let* ((ellipsis "...")
           (ellipsis-width (string-width ellipsis)))
      (if (<= n ellipsis-width)
          (truncate-string-to-width path n)
        (let* ((left-width (floor (/ (- n ellipsis-width) 2)))
               (right-width (- n ellipsis-width left-width))
               (head (truncate-string-to-width path left-width))
               (tail (panel--string-suffix-to-width path right-width)))
          (concat head ellipsis tail))))))

(defun panel--recent-file-line (file index)
  "Return the rendered line for FILE at INDEX."
  (let* ((display-file (directory-file-name file))
         (full-path (expand-file-name display-file))
         (shortcut (format "[%d]" index))
         (file-name (file-name-nondirectory display-file))
         (file-dir (or (file-name-directory display-file) ""))
         (fallback-name (panel--truncate-path-in-middle
                         (abbreviate-file-name full-path)
                         panel-path-max-length))
         (path-part (if (and panel-show-file-path
                             (not (string-empty-p file-name)))
                        (propertize
                         (panel--truncate-path-in-middle file-dir panel-path-max-length)
                         'face 'panel-path-face)
                      ""))
         (title (concat (panel--file-icon file)
                        " "
                        path-part
                        (propertize (if (string-empty-p file-name)
                                        fallback-name
                                      file-name)
                                    'face 'panel-filename-face))))
    (concat (propertize title 'path full-path)
            (propertize (format " %s" shortcut) 'face 'panel-shortcut-face))))

(defun panel--insert-recent-files ()
  "Insert the first x recent files with icons in the panel buffer."
  (panel--ensure-recentf)
  (setq panel-recentfiles (seq-take recentf-list 9))
  (let ((left-margin (panel--calculate-padding-left))
        (index 1))
    (dolist (file panel-recentfiles)
      (insert (make-string left-margin ?\s))
      (insert (panel--recent-file-line file index))
      (insert "\n")
      (setq index (1+ index)))))

(defun panel--calculate-padding-left ()
  "Calculate padding for left side."
  (let ((current-width (window-width)))
    (when (or (null panel--padding-cache)
              (not (eq current-width panel--last-window-width)))
      (setq panel--last-window-width current-width)
      (setq panel--padding-cache
            (if panel-recentfiles
                (let ((max-width 0)
                      (index 1))
                  (dolist (file panel-recentfiles)
                    (setq max-width
                          (max max-width
                               (string-width (panel--recent-file-line file index))))
                    (setq index (1+ index)))
                  (max panel-min-left-padding
                       (floor (/ (- current-width max-width) 2))))
              panel-min-left-padding)))
    panel--padding-cache))

(defun panel--insert-text (text)
  "Insert TEXT with left padding."
  (let ((left-margin (panel--calculate-padding-left)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text))))

(defun panel--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer with debouncing."
  (when (equal (buffer-name) panel-buffer)
    (when panel--resize-timer
      (cancel-timer panel--resize-timer))
    (setq panel--resize-timer
          (run-with-idle-timer 0.2 nil
                               (lambda ()
                                 (when (get-buffer-window panel-buffer)
                                   (panel--refresh-screen)))))))

(defun panel--weather-cache-valid-p ()
  "Check if weather cache is still valid."
  (and panel--last-weather-update
       panel-temperature
       (< (- (float-time) panel--last-weather-update)
          panel-weather-cache-duration)))

(defun panel--weather-data-available-p ()
  "Check if we have weather data ready to display."
  (and panel-weatherdescription
       panel-temperature))

(defun panel--weather-request-url ()
  "Build the weather request URL."
  (format "%s?latitude=%s&longitude=%s&current=temperature_2m,weather_code"
          panel-weather-api-base-url
          panel-latitude
          panel-longitude))

(defun panel--extract-weather-fields (json-obj)
  "Extract current temperature and weather code from JSON-OBJ."
  (let* ((current (alist-get 'current json-obj))
         (current-weather (alist-get 'current_weather json-obj))
         (temperature (or (alist-get 'temperature_2m current)
                          (alist-get 'temperature current-weather)))
         (weather-code (or (alist-get 'weather_code current)
                           (alist-get 'weathercode current-weather))))
    (when (and temperature weather-code)
      (list temperature weather-code))))

(defun panel--get-image ()
  "Get or create cached image."
  (when (and (display-images-p)
             (file-exists-p panel-image-file)
             (not (string-empty-p panel-image-file)))
    (unless (and panel--cached-image
                 (equal panel-image-file (plist-get (cdr panel--cached-image) :file)))
      (setq panel--cached-image
            (create-image panel-image-file
                          nil
                          nil
                          :width panel-image-width
                          :height panel-image-height)))
    panel--cached-image))

(defun panel--process-weather-json (json-obj initial)
  "Update weather state from JSON-OBJ.
INITIAL indicates if this is the first fetch; starts the periodic timer."
  (condition-case err
      (if-let* ((weather-fields (panel--extract-weather-fields json-obj)))
          (pcase-let ((`(,temperature ,weather-code) weather-fields))
            (setq panel--weather-error-message nil)
            (setq panel-temperature (format "%.1f" temperature))
            (setq panel-weatherdescription
                  (panel--weather-code-to-string weather-code))
            (setq panel-weathericon
                  (panel--weather-icon-from-code weather-code))
            (setq panel--last-weather-update (float-time))
            (when (and initial (not panel--weather-timer))
              (setq panel--weather-timer
                    (run-with-timer panel-weather-update-interval
                                    panel-weather-update-interval
                                    #'panel--fetch-weather-data)))
            (when (panel--active-p)
              (panel--refresh-weather-only)))
        (setq panel--weather-error-message "Weather unavailable")
        (message "Panel: Weather parse error: missing current weather data")
        (when (and (panel--active-p)
                   (not (panel--weather-data-available-p)))
          (panel--refresh-weather-only)))
    (error
     (setq panel--weather-error-message "Weather unavailable")
     (message "Panel: Weather parse error: %s" err)
     (when (and (panel--active-p)
                (not (panel--weather-data-available-p)))
       (panel--refresh-weather-only)))))

(defun panel--fetch-weather-data (&optional initial force)
  "Fetch weather data from API.
INITIAL indicates if this is the first fetch.
FORCE bypasses cache check."
  (when (or initial force (panel--active-p))
    (when (and (not initial) (not force) (panel--weather-cache-valid-p))
      (message "Panel: Using cached weather data"))
    (unless (or panel--weather-fetch-in-progress
                (and (not initial) (not force) (panel--weather-cache-valid-p)))
      (setq panel--weather-fetch-in-progress t)
      (let ((url (panel--weather-request-url)))
        (plz 'get url
          :as #'json-read
          :then (lambda (json-obj)
                  (setq panel--weather-fetch-in-progress nil)
                  (setq panel--weather-retry-count 0)
                  (panel--process-weather-json json-obj initial))
          :else (lambda (err)
                  (setq panel--weather-fetch-in-progress nil)
                  (message "Panel: Weather error (attempt %d/%d): %s"
                           (1+ panel--weather-retry-count)
                           panel-weather-max-retries err)
                  (unless (panel--weather-data-available-p)
                    (setq panel--weather-error-message "Weather unavailable")
                    (when (panel--active-p)
                      (panel--refresh-weather-only)))
                  (when (< panel--weather-retry-count panel-weather-max-retries)
                    (setq panel--weather-retry-count (1+ panel--weather-retry-count))
                    (run-with-timer (* 30 panel--weather-retry-count) nil
                                   #'panel--fetch-weather-data initial force))))))))

(defun panel--refresh-weather-only ()
  "Only refresh weather information without redrawing entire screen."
  (when-let* ((buf (get-buffer panel-buffer))
              (win (get-buffer-window buf 'visible)))
    (with-current-buffer buf
      (if (and panel--last-window-width
               (/= (window-width win) panel--last-window-width))
          (panel--refresh-screen)
        (let ((inhibit-read-only t)
              (saved-pos (point)))
          (save-excursion
            (goto-char (point-min))
            (when-let* ((match (text-property-search-forward 'panel-section 'weather t))
                        (beg (prop-match-beginning match))
                        (end (prop-match-end match)))
              (goto-char beg)
              (delete-region beg end)
              (panel--insert-weather-info)))
          (goto-char (min saved-pos (point-max))))))))

(defun panel--cleanup-weather ()
  "Cancel weather timer and reset state."
  (when panel--weather-timer
    (cancel-timer panel--weather-timer)
    (setq panel--weather-timer nil))
  (setq panel--weather-fetch-in-progress nil)
  (setq panel--last-weather-update nil)
  (setq panel--weather-retry-count 0))

(defun panel--init-weather ()
  "Initialize weather fetching with cleanup."
  (panel--cleanup-weather)
  (when (panel--weather-info-p)
    (panel--fetch-weather-data t)))

(defun panel-refresh ()
  "Manually refresh the panel and weather."
  (interactive)
  (panel--refresh-screen)
  (when (panel--weather-info-p)
    (panel--fetch-weather-data nil t)))

;;;###autoload
(defun panel-create-hook ()
  "Setup panel screen."
  (remove-hook 'window-configuration-change-hook #'panel--redisplay-buffer-on-resize)
  (add-hook 'window-configuration-change-hook #'panel--redisplay-buffer-on-resize)
  (when (< (length command-line-args) 2)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (let ((frame (selected-frame)))
                  (run-with-idle-timer
                   0.1 nil
                   (lambda (frame)
                     (with-selected-frame frame
                       (panel--refresh-screen)
                       (run-with-idle-timer 0.1 nil #'panel--init-weather)))
                   frame))))))

(defun panel--insert-startup-time ()
  "Insert startup time."
  (panel--insert-text (format "%s %s %s %s"
                              (propertize
                               (panel--with-icon-fallback
                                #'nerd-icons-octicon
                                "nf-oct-clock"
                                "time")
                               'display '(raise 0))
                              (propertize "Startup time:" 'face 'panel-text-info-face)
                              (propertize (emacs-init-time "%.2f") 'face 'panel-startup-time-face)
                              (propertize "seconds" 'face 'panel-text-info-face))))

(defun panel--insert-package-info (packages)
  "Insert package info as PACKAGES."
  (panel--insert-text (format "%s %s %s"
                              (propertize
                               (panel--with-icon-fallback
                                #'nerd-icons-codicon
                                "nf-cod-package"
                                "pkg")
                               'display '(raise -0.1))
                              (propertize packages 'face 'panel-info-face 'display '(raise -0.1))
                              (propertize "packages loaded" 'face 'panel-text-info-face 'display '(raise -0.1)))))

(defun panel--weather-info-p ()
  "Check if we have latitude and longitude to show weather info."
  (and (numberp panel-latitude)
       (numberp panel-longitude)))

(defun panel--insert-weather-info ()
  "Insert weather info, tagged with \\='panel-section \\='weather text property."
  (when (panel--weather-info-p)
    (let ((beg (point))
          (icon (or panel-weathericon "")))
      (if panel-weatherdescription
          (panel--insert-text
           (if (string-empty-p icon)
               (format "%s, %s%s"
                       (propertize panel-weatherdescription 'face 'panel-weather-description-face)
                       (propertize panel-temperature 'face 'panel-weather-temperature-face)
                       (propertize "℃" 'face 'panel-text-info-face))
             (format "%s %s, %s%s"
                     (if (panel--nerd-icons-available-p)
                         (propertize icon
                                     'face '(:family "Weather icons" :height 1.0)
                                     'display '(raise 0))
                       (propertize icon 'face 'panel-weather-icon-face))
                     (propertize panel-weatherdescription 'face 'panel-weather-description-face)
                     (propertize panel-temperature 'face 'panel-weather-temperature-face)
                     (propertize "℃" 'face 'panel-text-info-face))))
        (panel--insert-text
         (propertize (or panel--weather-error-message
                         "Loading weather data...")
                     'face 'panel-weather-temperature-face)))
      (put-text-property beg (point) 'panel-section 'weather))))

(defun panel--package-length ()
  "Get the number of installed packages."
  (cond
   ((bound-and-true-p package-alist)
    (length package-activated-list))
   ((boundp 'straight--profile-cache)
    (hash-table-count straight--profile-cache))
   ((boundp 'elpaca--queued)
    (length elpaca--queued))
   (t 0)))

(defun panel--active-p ()
  "Check if buffer is active and visible."
  (and (buffer-live-p (get-buffer panel-buffer))
       (get-buffer-window panel-buffer 'visible)))

(defun panel--refresh-screen ()
  "Show the panel screen."
  (panel--ensure-recentf)
  (setq panel-recentfiles (seq-take recentf-list 9))
  (with-current-buffer (get-buffer-create panel-buffer)
    (let* ((image (panel--get-image))
           (size (when image (image-size image)))
           (width (when size (car size)))
           (left-margin (max 0
                             (+ (if width
                                    (max panel-min-left-padding
                                         (floor (/ (- (window-width) width) 2)))
                                  panel-min-left-padding)
                                panel-intro-horizontal-offset)))
           (packages (format "%d" (panel--package-length))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (unless (or (null panel-title)
                    (string-empty-p panel-title))
          (panel--insert-text (propertize panel-title 'face 'panel-title-face))
          (insert "\n"))
        (when image
          (insert "\n")
          (insert (make-string left-margin ?\ ))
          (insert-image image)
          (insert "\n\n"))

        (when (and (not image)
                   (panel--intro-visible-p))
          (insert "\n\n")
          (panel--insert-intro))

        ;; (panel--insert-separator)
        (panel--insert-recent-files)

        (insert "\n")
        (panel--insert-startup-time)
        (panel--insert-package-info packages)
        (panel--insert-weather-info)

        (insert "\n")
        (panel--insert-centered (propertize (format-time-string "%A, %B %d %R") 'face 'panel-time-face))

        (switch-to-buffer panel-buffer)
        (panel-mode)
        (goto-char (point-min))
        (if (re-search-forward " \\[[1-9]\\]" nil t)
            (beginning-of-line)
          (goto-char (point-min)))
        (panel--update-shortcut-highlight)))))

(defun panel--insert-separator ()
  "Insert a separator line."
  (insert "\n")
  (panel--insert-text
   (propertize (make-string (+ panel-path-max-length (* panel-min-left-padding 2)) ?─) 'face 'panel-separator-face)))

(provide 'panel)
;;; panel.el ends here
