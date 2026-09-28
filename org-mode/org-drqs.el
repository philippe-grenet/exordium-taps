;;;; Clickable DRQS references -*- lexical-binding: t -*-
;;;
;;; Makes {DRQS 1234567} and {DRQS 1234567<GO>} clickable links to the DRQS web
;;; app, both with C-c o D and with the ordinary C-c C-o.
;;;
;;; Bloomberg-specific, and only ever useful next to the work notes, so
;;; after-init.el loads this file along with the rest of the org repo features
;;; -- which means not at all on a machine without the repo.

(require 'org)
(require 'thingatpt)

(defconst my/org-drqs-re "{DRQS \\([0-9]+\\)\\(?: *<GO>\\)?}"
  "Match a DRQS reference; group 1 is the ticket number.")

(defun my/org-drqs-open (number)
  "Open DRQS ticket NUMBER in the default browser."
  (browse-url (format "https://drqs.prod.bloomberg.com/ticket/%s" number)))

(defun my/org-drqs-buttonize ()
  "Add font-lock rules to make {DRQS NNN} and {DRQS NNN<GO>} clickable."
  (font-lock-add-keywords
   nil
   `((,my/org-drqs-re (0 'org-link prepend)))
   t))

(defun my/org-drqs-number-at-point ()
  "Return the ticket number of the {DRQS ...} reference at point, or nil."
  (when (thing-at-point-looking-at my/org-drqs-re)
    (match-string-no-properties 1)))

;; Also handle {DRQS ...} via C-c C-o (org-open-at-point)
(defun my/org-drqs-open-at-point ()
  "Open {DRQS ...} at point if any; return non-nil if handled."
  (when-let* ((number (my/org-drqs-number-at-point)))
    (my/org-drqs-open number)
    t))

(defun my/org-drqs-follow-at-point ()
  "If point is on a {DRQS ...} reference, open it in the browser."
  (interactive)
  (unless (my/org-drqs-open-at-point)
    (user-error "No DRQS reference at point")))

(add-hook 'org-mode-hook #'my/org-drqs-buttonize)
(add-hook 'org-open-at-point-functions #'my/org-drqs-open-at-point)
(define-key org-mode-map (kbd "C-c o D") #'my/org-drqs-follow-at-point)


;;; org-drqs.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
