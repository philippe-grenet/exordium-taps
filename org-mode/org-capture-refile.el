;;;; Capturing and refiling into the second-brain todo list -*- lexical-binding: t -*-
;;;
;;; The capture templates and the refile commands all write into `todo.org' --
;;; and, through org-catchup-people.el, into `catchup.org' -- so they only mean
;;; anything on a machine that has `my/org-repo' (see
;;; taps/common/before-init.el); after-init.el loads this file only there.
;;;
;;;   C-c o t : refile the current headline to Today  (C-u for the end)
;;;   C-c o w : refile the current headline to Week   (C-u for the end)
;;;   C-c o b : refile the current headline to Backlog (C-u for the end)
;;;
;;; The capture keys themselves (M-F12, F13) and the capture UX live in
;;; after-init.el, which needs no repo.
;;;
;;; See http://orgmode.org/manual/Capture-templates.html#Capture-templates

(require 'org)
(require 'org-capture)

(defvar my/org-repo)
(declare-function my/org-file "before-init")
(declare-function my/org-catchup-capture-templates "org-catchup-people")


;;; Refile
;;
;; Move the task to the beginning of the target section; with a C-u argument,
;; move it to the end instead.

(defun my-org-refile-to (target-headline arg)
  "Refile current headline to TARGET-HEADLINE in todo.org.
Prepend by default; with prefix ARG, append."
  (let* ((target-file (my/org-file "todo.org"))
         (pos (save-excursion
                (find-file target-file)
                (org-find-exact-headline-in-buffer target-headline)))
         (org-reverse-note-order (not arg)))
    (org-refile nil nil (list target-headline target-file nil pos))))

(defun my-org-refile-to-today (arg)
  "Refile current headline to Today." (interactive "P")
  (my-org-refile-to "☕️ Today" arg))

(defun my-org-refile-to-week (arg)
  "Refile current headline to Week." (interactive "P")
  (my-org-refile-to "Week" arg))

(defun my-org-refile-to-backlog (arg)
  "Refile current headline to Backlog." (interactive "P")
  (my-org-refile-to "Backlog" arg))

(define-key org-mode-map (kbd "C-c o t") #'my-org-refile-to-today)
(define-key org-mode-map (kbd "C-c o w") #'my-org-refile-to-week)
(define-key org-mode-map (kbd "C-c o b") #'my-org-refile-to-backlog)


;;; Capture

(setq org-default-notes-file (my/org-file "todo.org"))

;; The per-person entries are generated from catchup.org, so that renaming a
;; section there cannot leave a template pointing at a stale heading.
;; See org-catchup-people.el, which also binds C-c o p and C-c o c.
(load-file "~/.emacs.d/taps/org-mode/org-catchup-people.el")

(setq org-capture-templates
      (append
       `(("i" "📥\tInbox" entry
          (file+headline ,(my/org-file "todo.org") "📥 Inbox")
          "** TODO %?\n  %i\n"
          :empty-lines-after 1)
         ("T" "☕️\tToday" entry
          (file+headline ,(my/org-file "todo.org") "☕️ Today")
          "** TODO %?\n  %i\n"
          :prepend t
          :empty-lines-after 0))
       (my/org-catchup-capture-templates)))

;;; org-capture-refile.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
