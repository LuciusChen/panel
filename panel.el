;;; panel.el --- Simple welcome-panel screen -*- lexical-binding: t -*-

;; Welcome-panel screen

;; Author: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Maintainer: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (async "1.9.7"))
;; Homepage: https://github.com/konrad1977/welcome-panel

;;; Commentary:

;;; Minimalistic panel for Emacs.

(require 'async)
(require 'json)
(require 'recentf)
(require 'url)
(require 'nerd-icons)
(require 'cl-lib)

;;; Code:

(defvar panel-mode nil)
(defvar panel-recentfiles '() "Recent list.")
(defvar panel-temperature nil)
(defvar panel-weatherdescription nil)
(defvar panel-weathericon nil)

(defvar panel--weather-timer nil
  "Timer for periodic weather updates.")

(defvar panel--weather-fetch-in-progress nil
  "Flag to prevent concurrent weather fetches.")

(defcustom panel-title "Quick access [C-number to open file]"
  "Panel title."
  :group 'panel
  :type 'string)

(defcustom panel-show-file-path nil
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

(defgroup panel nil
  "Panel group."
  :group 'applications)

(defconst panel-buffer "*welcome*"
  "Panel buffer name.")

(defvar panel--file-icon-cache (make-hash-table :test 'equal)
  "Cache for file icons.")

(defvar-local panel--padding-cache nil
  "Cache for padding in the panel buffer.")

(defvar-local panel--last-window-width nil
  "Last window width in the panel buffer.")

(defvar panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'panel--open-recent-file)
    (define-key map (kbd "o") 'panel--open-recent-file)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "M-s-" (number-to-string i)))
                  `(lambda ()
                     (interactive)
                     (panel--open-recent-file-at-index ,i))))

    map)
  "Keymap for `panel-mode'.")

(define-derived-mode panel-mode fundamental-mode "dashboard"
  "Major mode for the welcome-panel screen."
  :group 'panel
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local global-hl-line-mode nil)
  (setq-local buffer-read-only t)
  (use-local-map panel-mode-map))

(defface panel-title-face
  '((t :inherit font-lock-constant-face :height 1.3 :italic t))
  "Title face."
  :group 'panel)

(defface panel-subtitle-face
  '((t :foreground "#9399b2"))
  "Subtitle face."
  :group 'panel)

(defface panel-separator-face
  '((t :inherit 'font-lock-comment-face))
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

(defface panel-weather-icon-face
  '((t :height 0.9))
  "Face for weather icon."
  :group 'panel)

(defface panel-weather-temperature-face
  '((t :foreground "#f38ba8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for temperature."
  :group 'panel)

(defun panel--weather-icon-from-code (code)
  "Map weather CODE to a corresponding string."
  (nerd-icons-wicon
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
     (_ "Unknown"))))

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
  (let ((width (window-width)))
    (insert (make-string (/ (- width (length text)) 2) ?\ ))
    (insert text)))

(defun panel--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))

(defun panel--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files panel-recentfiles))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))

(defun panel--truncate-path-in-middle (path n)
  "Truncate the middle of PATH to length N by removing characters and adding an ellipsis."
  (if (<= (length path) n)
      path
    (let* ((left (/ (- n 3) 2))
           (right (- n left 3))
           (head (substring path 0 (+ left 1)))
           (tail (substring path (- (length path) right)))
           (ellipsis "..."))
      (concat head ellipsis tail))))

(defun panel--insert-recent-files ()
  "Insert the first x recent files with icons in the panel buffer."
  (recentf-mode)
  (setq panel-recentfiles (seq-take recentf-list 9))
  (let* ((files panel-recentfiles)
         (left-margin (panel--calculate-padding-left)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s %s%s"
                            (or (gethash file panel--file-icon-cache)
                                (puthash file
                                         (propertize (cond ((not (file-exists-p file)) (nerd-icons-mdicon "nf-md-file_remove" :face '(:inherit nerd-icons-red)))
                                                           ((file-directory-p file) (nerd-icons-icon-for-dir file))
                                                           (t (nerd-icons-icon-for-file file))))
                                         panel--file-icon-cache))
                            (propertize (panel--truncate-path-in-middle file-dir panel-path-max-length) 'face 'panel-path-face)
                            (propertize file-name 'face 'panel-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat title-with-path (propertize (format " [%s]" shortcut) 'face 'panel-shortcut-face))))
        (insert (format "%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut))))))

(defun panel--calculate-padding-left ()
  "Calculate padding for left side."
  (let ((current-width (window-width)))
    (when (or (null panel--padding-cache)
              (not (eq current-width panel--last-window-width)))
      (setq panel--last-window-width current-width)
      (setq panel--padding-cache
            (if-let* ((files panel-recentfiles)
                      (max-length (apply #'max (mapcar (lambda (path)
                                                         (length (panel--truncate-path-in-middle path panel-path-max-length)))
                                                       files)))
                      (filenames (mapcar (lambda (path) (file-name-nondirectory path)) files))
                      (max-filename-length (/ (apply #'max (mapcar #'length filenames)) 2))
                      (left-margin (max (+ panel-min-left-padding max-filename-length)
                                        (/ (- current-width max-length) 2))))
                (- left-margin max-filename-length)
              panel-min-left-padding)))
    panel--padding-cache))

(defun panel--insert-text (text)
  "Insert TEXT with left padding."
  (let ((left-margin (panel--calculate-padding-left)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text))))

(defun panel--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) panel-buffer)
    (panel--refresh-screen)))

(defun panel--fetch-weather-data (&optional initial)
  "Fetch weather data from API. INITIAL indicates if this is the first fetch."
  (unless panel--weather-fetch-in-progress
    (setq panel--weather-fetch-in-progress t)

    (let ((url-request-method "GET")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                       panel-latitude panel-longitude)))
      (url-retrieve url
                    (lambda (status)
                      (setq panel--weather-fetch-in-progress nil)
                      (unless (plist-get status :error)
                        (condition-case err
                            (progn
                              (goto-char (point-min))
                              (when (and (re-search-forward "^HTTP/[0-9\\.]+ \\([0-9]+\\)" nil t)
                                         (= 200 (string-to-number (match-string 1))))
                                (goto-char (point-min))
                                (when (re-search-forward "^$" nil t)
                                  (forward-char 1)
                                  (let* ((json-string (buffer-substring-no-properties (point) (point-max))))
                                    (unless (string-empty-p (string-trim json-string))
                                      (let ((json-obj (json-read-from-string json-string)))
                                        (let-alist json-obj
                                          (when (and .current_weather
                                                     .current_weather.temperature
                                                     .current_weather.weathercode)
                                            (setq panel-temperature
                                                  (format "%.1f" .current_weather.temperature))
                                            (setq panel-weatherdescription
                                                  (panel--weather-code-to-string .current_weather.weathercode))
                                            (setq panel-weathericon
                                                  (panel--weather-icon-from-code .current_weather.weathercode))
                                            (when (and initial (not panel--weather-timer))
                                              (setq panel--weather-timer
                                                    (run-with-timer 900 900 #'panel--fetch-weather-data)))
                                            (when (panel--is-active)
                                              (panel--refresh-screen))))))))))
                          (json-end-of-file
                           (message "Panel: Incomplete JSON data"))
                          (error
                           (message "Panel: Weather error: %s" err)))))
                    nil
                    t))))

(defun panel--cleanup-weather ()
  "Cancel weather timer and reset state."
  (when panel--weather-timer
    (cancel-timer panel--weather-timer)
    (setq panel--weather-timer nil))
  (setq panel--weather-fetch-in-progress nil))

(defun panel--init-weather ()
  "Initialize weather fetching with cleanup."
  (panel--cleanup-weather)
  (when (panel--show-weather-info)
    (panel--fetch-weather-data t)))

;;;###autoload
(defun panel-create-hook ()
  "Setup panel screen."
  (when (< (length command-line-args) 2)
    (remove-hook 'switch-to-buffer #'panel--redisplay-buffer-on-resize)
    (add-hook 'window-configuration-change-hook #'panel--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (panel--refresh-screen)
                                    (run-with-idle-timer 0.1 nil #'panel--init-weather)))))

(defun panel--truncate-text-right (text)
  "Truncate TEXT at the right to a maximum of 100 characters."
  (if (> (length text) panel-path-max-length)
      (concat (substring text 0 (- panel-path-max-length 3)) "...")
    text))

(defun panel--insert-startup-time ()
  "Insert startup time."
  (panel--insert-text (format "%s %s %s %s"
                              (propertize (nerd-icons-octicon "nf-oct-clock")
                                          'display '(raise 0))
                              (propertize "Startup time:" 'face 'panel-text-info-face)
                              (propertize (emacs-init-time "%.2f") 'face 'panel-startup-time-face)
                              (propertize "seconds" 'face 'panel-text-info-face))))

(defun panel--insert-package-info (packages)
  "Insert package info as PACKAGES."
  (panel--insert-text (format "%s %s %s"
                              (propertize (nerd-icons-codicon "nf-cod-package")
                                          'display '(raise -0.1))
                              (propertize packages 'face 'panel-info-face 'display '(raise -0.1))
                              (propertize "packages loaded" 'face 'panel-text-info-face 'display '(raise -0.1)))))

(defun panel--show-weather-info ()
  "Check if we have latitude and longitude to show weather info."
  (and (floatp panel-latitude) (floatp panel-longitude)
       (> panel-latitude 0.0) (> panel-longitude 0.0)))

(defun panel--insert-weather-info ()
  "Insert weather info."
  (when (panel--show-weather-info)
    (if panel-weatherdescription
        (panel--insert-text (format "%s %s, %s%s"
                                    (propertize panel-weathericon 'face '(:family "Weather icons" :height 1.0) 'display '(raise 0))
                                    (propertize panel-weatherdescription 'face 'panel-weather-description-face)
                                    (propertize panel-temperature 'face 'panel-weather-temperature-face)
                                    (propertize "℃" 'face 'panel-text-info-face)))
      (panel--insert-text (propertize "Loading weather data..." 'face 'panel-weather-temperature-face)))))

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

(defun panel--is-active ()
  "Check if buffer is active and visible."
  (or (eq panel-buffer (window-buffer (selected-window)))
      (get-buffer-window panel-buffer 'visible)))

(defun panel--refresh-screen ()
  "Show the panel screen."
  (setq panel-recentfiles (seq-take recentf-list 9))
  (set-face-background 'fringe (face-attribute 'default :background))
  (with-current-buffer (get-buffer-create panel-buffer)
    (let* ((buffer-read-only)
           (image (create-image panel-image-file 'png nil :width panel-image-width :height panel-image-height))
           (size (image-size image))
           (width (car size))
           (left-margin (max panel-min-left-padding (floor (/ (- (window-width) width) 2))))
           (packages (format "%d" (panel--package-length))))
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (panel--insert-text (propertize panel-title 'face 'panel-title-face))
        (insert "\n")
        ;; (panel--insert-separator)
        (panel--insert-recent-files)
        (setq cursor-type nil)

        (insert "\n")
        (panel--insert-startup-time)
        (panel--insert-package-info packages)
        (panel--insert-weather-info)

        (insert "\n")
        (panel--insert-centered (propertize (format-time-string "%A, %B %d %R") 'face 'panel-time-face))

        (insert "\n\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)

        (switch-to-buffer panel-buffer)
        (panel-mode)
        (goto-char (point-min))
        (forward-line 3)))))

(defun panel--insert-separator ()
  "Insert a separator line."
  (insert "\n")
  (panel--insert-text
   (propertize (make-string (+ panel-path-max-length (* panel-min-left-padding 2)) ?─) 'face 'panel-separator-face)))

(provide 'panel)
;;; panel.el ends here
