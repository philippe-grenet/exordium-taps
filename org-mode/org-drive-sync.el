;;;; Copy the todo and catch-up files to Google Drive -*- lexical-binding: t -*-
;;;
;;; PlainOrg, on the phone, reads todo.org and catchup.org from Drive.  The copy
;;; runs hourly and is a no-op when the Drive folder is not mounted.
;;;
;;; Both the source and the destination are machine-specific: the source is
;;; `my/org-repo' (see taps/common/before-init.el), so after-init.el only loads
;;; this file where the repo exists, and the destination is the Drive account of
;;; that machine.

(defvar my/org-repo)
(declare-function my/org-file "before-init")

(defvar my/org-sync-directory
  "/Users/pgrenet/Library/CloudStorage/GoogleDrive-pgrenet@bloomberg.net/My Drive/org/"
  "Google Drive folder PlainOrg reads.  Skipped when it is not mounted.")

(defun org-sync ()
  "Copy todos and catch up notes to Google Drive."
  (interactive)
  (if (file-directory-p my/org-sync-directory)
      (progn
        (copy-file (my/org-file "todo.org")
                   (expand-file-name "todo.org" my/org-sync-directory) t)
        (copy-file (my/org-file "catchup.org")
                   (expand-file-name "catchup.org" my/org-sync-directory) t)
        (message "org-sync: synced"))
    (message "org-sync: Google Drive not mounted, skipping")))

;; Schedule the function every hour. See also run-with-timer and cancel-timer.
(run-at-time "00:00" 3600 'org-sync)

;;; org-drive-sync.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
