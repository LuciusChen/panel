;;; panel-test.el --- Startup screen regression tests -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for panel.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory
                                     (or load-file-name buffer-file-name))))
(require 'panel)

(ert-deftest panel-test-recent-path-obeys-display-limit ()
  (let ((file "/long/directory/目录/abcdefghijklmnopqrstuvwxyz0123456789.txt")
        (panel-path-max-length 20))
    (dolist (panel-show-file-path '(t nil))
      (cl-letf (((symbol-function 'panel--file-icon) (lambda (_) "I")))
        (let* ((line (panel--recent-file-line file 1))
               (plain (substring-no-properties line))
               (full-path (expand-file-name file))
               (path (string-remove-prefix
                      "I " (string-remove-suffix " [1]" plain))))
          (should (<= (string-width path) panel-path-max-length))
          (should (equal (get-text-property 0 'path line) full-path))
          (should (equal (get-text-property 0 'panel--recent-file line) file))
          (should-not (get-text-property 0 'help-echo line))
          (should (equal (get-text-property 2 'help-echo line) full-path))
          (should-not
           (get-text-property (1- (length line)) 'help-echo line)))))))

(ert-deftest panel-test-remote-recent-path-does-not-probe-filesystem ()
  (let* ((file "/ssh:test@example.invalid:/tmp/example.org/")
         (panel-use-icons nil)
         handler-called
         (file-name-handler-alist
          (cons (cons "\\`/ssh:"
                      (lambda (&rest _)
                        (setq handler-called t)
                        (error "Remote file-name handler was invoked")))
                file-name-handler-alist)))
    (let ((line (panel--recent-file-line file 1)))
      (should (stringp line))
      (should (equal (get-text-property 0 'path line) file))
      (should (equal (get-text-property 0 'panel--recent-file line) file))
      (should
       (cl-loop for position below (length line)
                thereis (equal (get-text-property position 'help-echo line)
                               file))))
    (should-not handler-called)))

(ert-deftest panel-test-recent-file-shortcuts-align ()
  (let ((panel-use-icons nil)
        (panel-show-file-path nil))
    (let ((lines (list (panel--recent-file-line "/tmp/a.el" 1)
                       (panel--recent-file-line
                        "/tmp/a-much-longer-file-name.el" 2))))
      (with-temp-buffer
        (setq panel--recent-file-lines lines)
        (cl-letf (((symbol-function 'panel--calculate-padding-left)
                   (lambda () 0)))
          (panel--insert-recent-files))
        (goto-char (point-min))
        (search-forward "[1]")
        (goto-char (match-beginning 0))
        (let ((first-column (current-column)))
          (forward-line 1)
          (search-forward "[2]")
          (goto-char (match-beginning 0))
          (should (= (current-column) first-column)))))))

(ert-deftest panel-test-recent-file-at-point-is-line-bounded ()
  (with-temp-buffer
    (insert "Panel heading\n  "
            (propertize "file [1]"
                        'path "/tmp/full-file"
                        'panel--recent-file "/tmp/file"))
    (panel-mode)
    (should eldoc-mode)
    (should (local-variable-p 'eldoc-documentation-function))
    (should (eq eldoc-documentation-function
                #'panel--recent-file-at-point))
    (goto-char (point-min))
    (should-not (panel--recent-file-at-point))
    (forward-line 1)
    (end-of-line)
    (should (equal (panel--recent-file-at-point) "/tmp/full-file"))
    (should (equal (panel--recent-file-at-point 'panel--recent-file)
                   "/tmp/file"))))

(ert-deftest panel-test-open-recent-file-uses-rendered-full-path ()
  (let* ((file (make-temp-file "panel-open-"))
         (entry (file-relative-name file default-directory))
         (panel-use-icons nil)
         (line (panel--recent-file-line entry 1))
         (full-path (get-text-property 0 'path line))
         opened-file)
    (unwind-protect
        (with-temp-buffer
          (insert line)
          (goto-char (point-min))
          (let ((default-directory temporary-file-directory))
            (cl-letf (((symbol-function 'find-file)
                       (lambda (target) (setq opened-file target))))
              (panel--open-recent-file)))
          (should (equal opened-file full-path)))
      (delete-file file))))

(ert-deftest panel-test-forget-recent-file-updates-history-only ()
  (let* ((file (make-temp-file "panel-forget-"))
         (entry (file-relative-name file default-directory))
         (file-regexp (concat "\\`\\(?:" (regexp-quote entry) "\\|"
                              (regexp-quote file) "\\)\\'"))
         (other "/tmp/other-file")
         (recentf-list (list entry other))
         (panel-use-icons nil)
         saved-list
         refreshed)
    (unwind-protect
        (with-temp-buffer
          (insert "  " (panel--recent-file-line entry 1))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'recentf-save-list)
                     (lambda () (setq saved-list (copy-sequence recentf-list))))
                    ((symbol-function 'panel--refresh-screen)
                     (lambda () (setq refreshed t))))
            (let ((file-name-handler-alist
                   (cons (cons file-regexp
                               (lambda (&rest _)
                                 (error "Unexpected file operation")))
                         file-name-handler-alist)))
              (panel--forget-recent-file)))
          (should (equal recentf-list (list other)))
          (should (equal saved-list (list other)))
          (should-not (equal entry (expand-file-name entry)))
          (should refreshed)
          (should (file-exists-p file))
          (should (eq (lookup-key panel-mode-map (kbd "d"))
                      'panel--forget-recent-file)))
      (delete-file file))))

(ert-deftest panel-test-recentf-initialization-avoids-tramp ()
  (let ((recentf-mode nil)
        (recentf-list nil)
        (recentf-auto-cleanup 'mode)
        (recentf-keep '(recentf-keep-default-predicate))
        (recentf-initialize-file-name-history t)
        (file-name-history nil)
        (file-name-handler-alist
         (cons '("\\.zip/" . tramp-archive-autoload-file-name-handler)
               file-name-handler-alist))
        mode-called)
    (cl-letf (((symbol-function 'recentf-mode)
               (lambda (_arg)
                 (setq mode-called t
                       recentf-mode t
                       recentf-list '("/ssh:test@example.invalid:/tmp/a"))
                 (should (eq recentf-auto-cleanup 'mode))
                 (should-not recentf-initialize-file-name-history)
                 (should
                  (cl-some
                   (lambda (keep)
                     (and (stringp keep)
                          (string-match-p keep (car recentf-list))))
                   recentf-keep))
                 (should-not
                  (cl-find-if
                   (lambda (entry)
                     (memq (cdr entry)
                           '(tramp-autoload-file-name-handler
                             tramp-file-name-handler
                             tramp-archive-autoload-file-name-handler
                             tramp-archive-file-name-handler)))
                   file-name-handler-alist))
                 (should
                  (cl-some
                   (lambda (keep)
                     (and (stringp keep)
                          (string-match-p keep "/tmp/a.zip/inside.txt")))
                   recentf-keep)))))
      (panel--ensure-recentf))
    (should mode-called)
    (should (equal file-name-history recentf-list))))

(ert-deftest panel-test-recent-files-render-each-line-once ()
  (let ((recentf-list '("/tmp/first.el" "/tmp/second.org"))
        (panel-recentfiles nil)
        (panel-title "Recent files")
        (panel-time-format "CUSTOM-TIME")
        (panel-intro-display 'never)
        (panel-image-file "")
        (panel-latitude nil)
        (panel-longitude nil)
        (panel-use-icons nil)
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'panel--ensure-recentf) #'ignore)
                  ((symbol-function 'panel--package-length) (lambda () 0))
                  ((symbol-function 'panel--recent-file-line)
                   (lambda (file index)
                     (push (cons file index) calls)
                     (concat file
                             (propertize (format " [%d]" index)
                                         'panel--shortcut t)))))
          (panel--refresh-screen)
          (should (equal (nreverse calls)
                         '(("/tmp/first.el" . 1)
                           ("/tmp/second.org" . 2))))
          (with-current-buffer panel-buffer
            (should (equal (buffer-name) "*panel*"))
            (should (eq major-mode 'panel-mode))
            (should (equal mode-name "Panel"))
            (should (= (length panel--recent-file-lines) 2))
            (goto-char (point-min))
            (should (search-forward "CUSTOM-TIME" nil t))))
      (when (get-buffer panel-buffer)
        (kill-buffer panel-buffer)))))

(ert-deftest panel-test-file-icon-is-not-stale ()
  (let ((panel-use-icons nil)
        (file (make-temp-file "panel-icon-" nil ".el")))
    (unwind-protect
        (cl-letf (((symbol-function 'panel--with-icon-fallback)
                   (lambda (_fn _icon fallback &rest _)
                     (if panel-use-icons "ICON" fallback))))
          (should (equal (substring-no-properties (panel--file-icon file)) "-"))
          (setq panel-use-icons t)
          (should (equal (panel--file-icon file) "ICON")))
      (delete-file file))))

(ert-deftest panel-test-image-settings-are-applied-on-every-render ()
  (let ((panel-image-file (make-temp-file "panel-image-" nil ".png"))
        (panel-image-width 100)
        (panel-image-height 50)
        widths)
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p) (lambda () t))
                  ((symbol-function 'create-image)
                   (lambda (_file _type _data &rest properties)
                     (push (plist-get properties :width) widths)
                     properties)))
          (panel--get-image)
          (setq panel-image-width 200)
          (panel--get-image)
          (should (equal widths '(200 100))))
      (delete-file panel-image-file))))

(ert-deftest panel-test-weather-json-accepts-only-requested-schema ()
  (let ((panel-use-icons nil)
        panel-temperature
        panel-weatherdescription
        panel-weathericon)
    (panel--process-weather-json
     '((current . ((temperature_2m . 21.25) (weather_code . 0)))))
    (should (equal panel-temperature "21.2"))
    (should (equal panel-weatherdescription "Clear sky"))
    (should-not
     (panel--process-weather-json
      '((current_weather . ((temperature . 21.25) (weathercode . 0))))))))

(ert-deftest panel-test-weather-timer-does-not-depend-on-first-success ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-update-interval 900)
        (panel-weather-request-timeout 15)
        panel--weather-timer
        panel--weather-retry-timer
        panel--weather-request
        events)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _)
                 (push 'timer events)
                 'periodic-timer))
              ((symbol-function 'panel--fetch-weather-data)
               (lambda (&rest _)
                 (push 'fetch events))))
      (panel--init-weather)
      (should (eq panel--weather-timer 'periodic-timer))
      (should (equal (nreverse events) '(timer fetch))))))

(ert-deftest panel-test-url-success-cleans-request-and-parses-json ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 0)
        (panel-use-icons nil)
        (url-http-attempt-keepalives t)
        (url-max-redirections 30)
        panel--weather-request
        panel--weather-retry-timer
        callback
        request-buffer
        request-url
        request-headers
        request-noninteractive
        request-keepalive
        request-max-redirections)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url function _args _silent _cookies)
                 (setq request-url url
                       request-headers url-request-extra-headers
                       request-noninteractive url-request-noninteractive
                       request-keepalive url-http-attempt-keepalives
                       request-max-redirections url-max-redirections
                       callback function
                       request-buffer (generate-new-buffer " *panel-test-url*"))
                 request-buffer))
              ((symbol-function 'run-with-timer) (lambda (&rest _) 'timeout-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'panel--active-p) (lambda () nil)))
      (panel--fetch-weather-data)
      (should (string-match-p "current=temperature_2m,weather_code" request-url))
      (should (equal request-headers '(("Accept" . "application/json"))))
      (should request-noninteractive)
      (should-not request-keepalive)
      (should (local-variable-p 'url-http-attempt-keepalives request-buffer))
      (should-not (buffer-local-value 'url-http-attempt-keepalives
                                      request-buffer))
      (should (zerop request-max-redirections))
      (should (local-variable-p 'url-max-redirections request-buffer))
      (should (zerop (buffer-local-value 'url-max-redirections
                                         request-buffer)))
      (with-current-buffer request-buffer
        (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                "{\"current\":{\"temperature_2m\":18.5,\"weather_code\":1}}")
        (let ((json-object-type 'hash-table)
              (json-key-type 'string))
          (funcall callback nil)))
      (should-not (buffer-live-p request-buffer))
      (should-not panel--weather-request)
      (should (equal panel-temperature "18.5"))
      (should (equal panel-weatherdescription "Partly cloudy")))))

(ert-deftest panel-test-url-timeout-aborts-and-cleans-request ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 0)
        (panel-temperature "old")
        (panel-weatherdescription "old")
        (panel-weathericon "old")
        panel--weather-request
        panel--weather-retry-timer
        timeout-function
        request-buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _function _args _silent _cookies)
                 (setq request-buffer (generate-new-buffer " *panel-test-timeout*"))))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat function &rest args)
                 (setq timeout-function (lambda () (apply function args)))
                 'timeout-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'panel--active-p) (lambda () nil)))
      (panel--fetch-weather-data)
      (funcall timeout-function)
      (should-not panel--weather-request)
      (should-not (buffer-live-p request-buffer))
      (should (equal panel--weather-error-message "Weather unavailable"))
      (should-not panel-temperature)
      (should-not panel-weatherdescription)
      (should-not panel-weathericon))))

(ert-deftest panel-test-url-start-and-parse-errors-stay-contained ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 0)
        panel--weather-request
        panel--weather-retry-timer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _) (signal 'file-error '("Cannot start"))))
              ((symbol-function 'panel--active-p) (lambda () nil)))
      (should-not (panel--fetch-weather-data))
      (should-not panel--weather-request)
      (should (equal panel--weather-error-message "Weather unavailable")))
    (let (callback response-buffer)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url function _args _silent _cookies)
                   (setq callback function
                         response-buffer
                         (generate-new-buffer " *panel-test-malformed*"))))
                ((symbol-function 'run-with-timer) (lambda (&rest _) 'timeout-timer))
                ((symbol-function 'cancel-timer) #'ignore)
                ((symbol-function 'panel--active-p) (lambda () nil)))
        (panel--fetch-weather-data)
        (with-current-buffer response-buffer
          (insert "HTTP/1.1 200 OK\r\n\r\nnot-json")
          (should-not (funcall callback nil)))
        (should-not panel--weather-request)
        (should-not (buffer-live-p response-buffer))))))

(ert-deftest panel-test-url-nil-start-fails-immediately ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 0)
        panel--weather-request
        timer-created)
    (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _)
                 (setq timer-created t)))
              ((symbol-function 'panel--active-p) (lambda () nil)))
      (panel--fetch-weather-data)
      (should-not panel--weather-request)
      (should-not timer-created)
      (should (equal panel--weather-error-message "Weather unavailable")))))

(ert-deftest panel-test-internal-render-error-propagates-after-cleanup ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 0)
        (panel-use-icons nil)
        panel--weather-request
        callback
        response-buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url function _args _silent _cookies)
                 (setq callback function
                       response-buffer
                       (generate-new-buffer " *panel-test-render-error*"))))
              ((symbol-function 'run-with-timer) (lambda (&rest _) 'timeout-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'panel--active-p) (lambda () t))
              ((symbol-function 'panel--refresh-weather-only)
               (lambda () (error "Render failed"))))
      (panel--fetch-weather-data)
      (with-current-buffer response-buffer
        (insert "HTTP/1.1 200 OK\r\n\r\n"
                "{\"current\":{\"temperature_2m\":18.5,\"weather_code\":1}}")
        (should-error (funcall callback nil) :type 'error))
      (should-not panel--weather-request)
      (should-not (buffer-live-p response-buffer))
      (should (equal panel-temperature "18.5"))
      (should-not panel--weather-retry-timer))))

(ert-deftest panel-test-weather-retry-precedes-rendering ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 1)
        panel--weather-request
        panel--weather-retry-timer
        callback
        response-buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url function _args _silent _cookies)
                 (setq callback function
                       response-buffer
                       (generate-new-buffer " *panel-test-retry-render*"))))
              ((symbol-function 'run-with-timer)
               (lambda (delay _repeat _function &rest _)
                 (if (= delay panel-weather-request-timeout)
                     'timeout-timer
                   'retry-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'panel--active-p) (lambda () t))
              ((symbol-function 'panel--refresh-weather-only)
               (lambda () (error "Render failed"))))
      (panel--fetch-weather-data)
      (with-current-buffer response-buffer
        (should-error
         (funcall callback '(:error (error "Offline")))
         :type 'error))
      (should-not panel--weather-request)
      (should-not (buffer-live-p response-buffer))
      (should (eq panel--weather-retry-timer 'retry-timer))
      (panel--cleanup-weather))))

(ert-deftest panel-test-each-weather-chain-has-a-full-retry-budget ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 1)
        panel--weather-request
        panel--weather-retry-timer
        callbacks
        buffers
        retry-function)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url function _args _silent _cookies)
                 (let ((buffer (generate-new-buffer " *panel-test-retry*")))
                   (push function callbacks)
                   (push buffer buffers)
                   buffer)))
              ((symbol-function 'run-with-timer)
               (lambda (delay _repeat function &rest _)
                 (if (= delay panel-weather-request-timeout)
                     'timeout-timer
                   (setq retry-function function)
                   'retry-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'panel--active-p) (lambda () t))
              ((symbol-function 'panel--refresh-weather-only) #'ignore))
      (panel--fetch-weather-data)
      (with-current-buffer (car buffers)
        (funcall (car callbacks) '(:error (error "Offline"))))
      (should (eq panel--weather-retry-timer 'retry-timer))
      (funcall retry-function)
      (with-current-buffer (car buffers)
        (funcall (car callbacks) '(:error (error "Offline"))))
      (should-not panel--weather-retry-timer)

      ;; A later periodic/manual chain starts again with the full budget.
      (panel--fetch-weather-data)
      (with-current-buffer (car buffers)
        (funcall (car callbacks) '(:error (error "Offline"))))
      (should (eq panel--weather-retry-timer 'retry-timer))
      (panel--cleanup-weather))))

(ert-deftest panel-test-dispose-weather-request-terminates-process ()
  (let* ((buffer (generate-new-buffer " *panel-test-process*"))
         (process (make-pipe-process :name "panel-test-process"
                                     :buffer buffer
                                     :noquery t))
         (request (vector buffer 'timeout-timer)))
    (cl-letf (((symbol-function 'cancel-timer) #'ignore))
      (panel--dispose-weather-request request))
    (should-not (process-live-p process))
    (should-not (buffer-live-p buffer))
    (should (equal request [nil nil]))))

(ert-deftest panel-test-late-url-callback-cannot-overwrite-new-request ()
  (let ((panel-latitude 1.0)
        (panel-longitude 2.0)
        (panel-weather-request-timeout 15)
        (panel-weather-max-retries 0)
        panel--weather-request
        panel--weather-retry-timer
        callbacks
        buffers)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url function _args _silent _cookies)
                 (let ((buffer (generate-new-buffer " *panel-test-late*")))
                   (push function callbacks)
                   (push buffer buffers)
                   buffer)))
              ((symbol-function 'run-with-timer) (lambda (&rest _) 'timeout-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'panel--active-p) (lambda () nil)))
      (panel--fetch-weather-data)
      (let ((old-callback (car callbacks)))
        (panel--cleanup-weather)
        (panel--fetch-weather-data)
        (let ((new-request panel--weather-request)
              (late-buffer (generate-new-buffer " *panel-test-late-response*")))
          (setq panel-temperature "new")
          (with-current-buffer late-buffer
            (insert "HTTP/1.1 200 OK\r\n\r\n"
                    "{\"current\":{\"temperature_2m\":99,\"weather_code\":0}}")
            (funcall old-callback nil))
          (should (eq panel--weather-request new-request))
          (should (equal panel-temperature "new"))
          (should-not (buffer-live-p late-buffer))))
      (panel--cleanup-weather))))

(ert-deftest panel-test-manual-refresh-invalidates-image-and-padding-cache ()
  (let ((panel-image-file (make-temp-file "panel-refresh-" nil ".png"))
        cleared
        refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'clear-image-cache)
                   (lambda (file &rest _) (setq cleared file)))
                  ((symbol-function 'panel--init-weather) #'ignore)
                  ((symbol-function 'panel--refresh-screen) (lambda () (setq refreshed t))))
          (panel-refresh)
          (should (equal cleared panel-image-file))
          (should refreshed))
      (delete-file panel-image-file)))
  (let ((recentf-list '("/tmp/a"))
        (panel-title nil)
        (panel-intro-display 'never)
        (panel-image-file "")
        (panel-latitude nil)
        (panel-longitude nil)
        (panel-use-icons nil))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create panel-buffer)
            (setq panel--padding-cache 999))
          (cl-letf (((symbol-function 'panel--ensure-recentf) #'ignore)
                    ((symbol-function 'panel--package-length) (lambda () 0)))
            (panel--refresh-screen))
          (with-current-buffer panel-buffer
            (should-not (equal panel--padding-cache 999))))
      (when (get-buffer panel-buffer)
        (kill-buffer panel-buffer)))))

(provide 'panel-test)
;;; panel-test.el ends here
