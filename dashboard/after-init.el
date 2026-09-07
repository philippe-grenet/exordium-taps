;;;; Package --- summary: The second-brain dashboard -*- lexical-binding: t -*-
;;; Commentary:
;; `M-x dashboard' opens an overview of the org notes repo: a three-month
;; calendar and the agenda for the next three weeks, this week's tasks from
;; todo.org, and the projects git says were touched most recently.
;;
;; The code lives in my-dashboard.el, which loads one file per section.  All
;; of it addresses `my/org-repo' (see taps/common/before-init.el), so it is
;; only loaded on a machine that has the repo.
;;
;; Note that this tap is loaded before the org-mode tap, which is where
;; `open-todo-file' and `open-catchup-file' come from; the dashboard only
;; calls them at runtime, and falls back to the paths themselves when the
;; org-mode tap is absent.
;;; Code:

(defvar my/org-repo)

(if my/org-repo
    (load "~/.emacs.d/taps/dashboard/my-dashboard.el")
  (message "dashboard tap: no org repo on this machine (set ORG_REPO_DIR), \
skipping"))

;;; after-init ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
