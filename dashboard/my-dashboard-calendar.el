;;;; Dashboard: calendar and agenda -*- lexical-binding: t -*-
;;;
;;; Two sections.
;;;
;;; The calendar is the same three-month display that `org-time-stamp' pops
;;; up: `calendar-generate' renders it, here into a temporary buffer rather
;;; than into *Calendar*, with ISO week numbers down the left margin and
;;; today highlighted.
;;;
;;; The agenda is `org-agenda-list' over the next three weeks, again rendered
;;; into a temporary buffer.  Org's own text properties survive the copy, so
;;; the lines keep their colours; their `org-marker' does not survive, so the
;;; location behind each is copied into a `my-dashboard-target' property that
;;; `dashboard-follow' can act on.

(require 'calendar)
(require 'cal-iso)
(require 'cal-move)
(require 'cl-lib)

(declare-function my-dashboard-width "my-dashboard")
(declare-function my-dashboard-insert-heading "my-dashboard")
(declare-function my-dashboard-insert-empty "my-dashboard")
(declare-function my-dashboard-quit "my-dashboard")
(declare-function org-agenda-list "org-agenda")
(declare-function org-fold-show-context "org-fold")
(defvar my-dashboard-agenda-days)
(defvar displayed-month)
(defvar displayed-year)

;; org-agenda is loaded on demand, so its variables have to be declared
;; special here for the `let*' in `my-dashboard--agenda-string' to bind them
;; dynamically.
(defvar org-agenda-block-separator)
(defvar org-agenda-buffer-name)
(defvar org-agenda-buffer-tmp-name)
(defvar org-agenda-remove-tags)
(defvar org-agenda-show-all-dates)
(defvar org-agenda-sticky)
(defvar org-agenda-use-time-grid)
(defvar org-agenda-window-setup)


;;; Three-month calendar

(defun my-dashboard--calendar-mark-today (date)
  "Give DATE the `my-dashboard-today' face in the current calendar buffer."
  (when (calendar-date-is-visible-p date)
    (save-excursion
      (calendar-cursor-to-visible-date date)
      (put-text-property (1- (point)) (1+ (point))
                         'font-lock-face 'my-dashboard-today))))

(defun my-dashboard--calendar-string ()
  "Return a three-month calendar, as a propertized string.
The current month is in the middle, the left margin carries ISO week
numbers, and today is highlighted."
  (let* ((today (calendar-current-date))
         (month (calendar-extract-month today))
         (year (calendar-extract-year today))
         ;; ISO week numbers only line up when the week starts on Monday.
         (calendar-week-start-day 1)
         ;; Leave the layout variables alone: `calendar-month-width' is
         ;; derived from them once, at load time, and is not recomputed here.
         (calendar-intermonth-header
          (propertize "wk" 'font-lock-face 'my-dashboard-week-number))
         (calendar-intermonth-text
          '(propertize
            (format "%2d" (car (calendar-iso-from-absolute
                                (calendar-absolute-from-gregorian
                                 (list month day year)))))
            'font-lock-face 'my-dashboard-week-number))
         displayed-month displayed-year)
    (with-temp-buffer
      ;; `calendar-generate' indents with `indent-to', which would otherwise
      ;; leave tabs in the string and wreck the alignment once it is padded.
      (setq-local indent-tabs-mode nil)
      (let ((calendar-buffer (buffer-name)))
        (calendar-generate month year)
        (my-dashboard--calendar-mark-today today))
      (buffer-string))))

(defun my-dashboard-insert-calendar ()
  "Insert the three-month calendar, centred on the dashboard body."
  (let* ((lines (split-string (my-dashboard--calendar-string) "\n"))
         (widest (apply #'max 0 (mapcar #'string-width lines)))
         (pad (make-string (max 0 (/ (- (my-dashboard-width) widest) 2)) ?\s)))
    (dolist (line lines)
      (unless (string-empty-p (string-trim line))
        (insert pad line "\n")))))


;;; Agenda for the next three weeks

(defconst my-dashboard--agenda-buffer " *dashboard-agenda*"
  "Name of the throwaway buffer the dashboard builds its agenda in.")

(defun my-dashboard--agenda-freeze-markers ()
  "Turn org's agenda markers into plain (FILE . POSITION) properties.
`org-agenda-mode' resets its markers from `kill-buffer-hook', so they are
dead by the time the dashboard would follow one; the locations are copied
out here, while the agenda buffer is still alive."
  (goto-char (point-min))
  (while (not (eobp))
    (let* ((start (line-beginning-position))
           (marker (or (get-text-property start 'org-marker)
                       (get-text-property start 'org-hd-marker))))
      (when (and (markerp marker) (marker-buffer marker))
        (put-text-property start (line-end-position)
                           'my-dashboard-target
                           (cons (buffer-file-name (marker-buffer marker))
                                 (marker-position marker)))))
    (forward-line 1)))

(defun my-dashboard--agenda-string ()
  "Return the org agenda for the next `my-dashboard-agenda-days' days.
Returns nil when the agenda has nothing in that window."
  (require 'org-agenda)
  (save-window-excursion
    ;; `org-agenda-list' overwrites `org-agenda-buffer-name'; the way to name
    ;; the buffer -- and so to leave any agenda of the user's own alone -- is
    ;; `org-agenda-buffer-tmp-name'.
    (let* ((org-agenda-buffer-tmp-name my-dashboard--agenda-buffer)
           (org-agenda-buffer-name my-dashboard--agenda-buffer)
           (org-agenda-sticky nil)
           (org-agenda-window-setup 'current-window)
           (org-agenda-show-all-dates nil)
           (org-agenda-use-time-grid nil)
           (org-agenda-remove-tags t)
           (org-agenda-block-separator nil)
           text)
      (unwind-protect
          (progn
            (org-agenda-list nil nil my-dashboard-agenda-days)
            (with-current-buffer my-dashboard--agenda-buffer
              (my-dashboard--agenda-freeze-markers)
              (setq text (buffer-substring (point-min) (point-max)))))
        (when-let* ((buffer (get-buffer my-dashboard--agenda-buffer)))
          (kill-buffer buffer)))
      ;; Drop org's own "Week-agenda (W37-W39):" banner; the section has a
      ;; heading of its own.
      (let ((lines (cl-remove-if
                    (lambda (line) (string-empty-p (string-trim line)))
                    (cdr (split-string (or text "") "\n")))))
        (when lines
          (string-join lines "\n"))))))

(defun my-dashboard-insert-agenda ()
  "Insert the org agenda for the next few weeks."
  (my-dashboard-insert-heading
   (format "Agenda — next %d days" my-dashboard-agenda-days))
  (let ((agenda (condition-case err
                    (my-dashboard--agenda-string)
                  (error (format "agenda unavailable: %s"
                                 (error-message-string err))))))
    (if (not agenda)
        (my-dashboard-insert-empty "Nothing scheduled.")
      ;; Insert line by line: `split-string' keeps the text properties, and
      ;; those carry the location `dashboard-follow' needs.  Extend that
      ;; location over the indent as well, so the whole line is followable.
      (dolist (line (split-string agenda "\n"))
        (let ((start (point))
              (target (get-text-property 0 'my-dashboard-target line)))
          (insert "  " line "\n")
          (when target
            (put-text-property start (point) 'my-dashboard-target target)))))))


;;; my-dashboard-calendar.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
