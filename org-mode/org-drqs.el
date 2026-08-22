;;;; Clickable DRQS references -*- lexical-binding: t -*-
;;;
;;; Makes {DRQS 1234567} and {DRQS 1234567<GO>} clickable links to the DRQS web
;;; app, both with C-c o D and with the ordinary C-c C-o.
;;;
;;; Bloomberg-specific, and only ever useful next to the work notes, so
;;; after-init.el loads this file along with the rest of the org repo features
;;; -- which means not at all on a machine without the repo.

(require 'org)

(defun my/org-drqs-open (number)
  "Open DRQS ticket NUMBER in the default browser."
  (browse-url (format "https://drqs.prod.bloomberg.com/ticket/%s" number)))

(defun my/org-drqs-buttonize ()
  "Add font-lock rules to make {DRQS NNN} and {DRQS NNN<GO>} clickable."
  (font-lock-add-keywords
   nil
   '(("{DRQS \\([0-9]+\\)\\(?: *<GO>\\)?}"
      (0 'org-link prepend)))
   t))

(defun my/org-drqs-follow-at-point ()
  "If point is on a {DRQS ...} reference, open it in the browser."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (if (and (string-match "{DRQS \\([0-9]+\\)\\(?: *<GO>\\)?}" line)
             (let ((mbeg (match-beginning 0))
                   (mend (match-end 0))
                   (col  (- (point) (line-beginning-position))))
               (and (>= col mbeg) (<= col mend))))
        (my/org-drqs-open (match-string 1 line))
      (user-error "No DRQS reference at point"))))

(add-hook 'org-mode-hook #'my/org-drqs-buttonize)
(define-key org-mode-map (kbd "C-c o D") #'my/org-drqs-follow-at-point)

;; Also handle {DRQS ...} via C-c C-o (org-open-at-point)
(defun my/org-drqs-open-at-point ()
  "Open {DRQS ...} at point if any; return non-nil if handled."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (and (string-match "{DRQS \\([0-9]+\\)\\(?: *<GO>\\)?}" line)
               (let ((mbeg (match-beginning 0))
                     (mend (match-end 0))
                     (col  (- (point) (line-beginning-position))))
                 (and (>= col mbeg) (<= col mend))))
      (my/org-drqs-open (match-string 1 line))
      t)))

(add-hook 'org-open-at-point-functions #'my/org-drqs-open-at-point)


;;; org-drqs.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
