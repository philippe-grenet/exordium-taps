;;;; Package --- summary: Machine-dependent locations -*- lexical-binding: t -*-
;;; Commentary:
;; Exordium loads every tap's `before-init.el' ahead of every tap's
;; `after-init.el', so what is defined here is available to all the taps.
;;
;; `my/org-repo' is the root of the second-brain org repo.  That repo only
;; exists on some machines, so the variable is nil elsewhere and the features
;; built on top of it -- the notes, capture and sync of the org-mode tap, the
;; Claude skills of the ai tap -- are skipped rather than left pointing at
;; files that are not there.
;;
;; The path comes from the ORG_REPO_DIR environment variable, exported from the
;; shell.  A GUI Emacs started from Finder or the Dock inherits no shell
;; environment, and `exec-path-from-shell' only runs on `after-init-hook', long
;; after the taps have been loaded; so as a stopgap the machine-local
;; ~/.emacs.d/before-init.el -- gitignored, and loaded before every tap -- can
;; set the variable itself:
;;
;;   (unless (getenv "ORG_REPO_DIR")
;;     (setenv "ORG_REPO_DIR" "~/Documents/org/"))

;;; Code:

(defconst my/org-repo
  (when-let* ((dir (getenv "ORG_REPO_DIR"))
              (dir (file-name-as-directory (expand-file-name dir)))
              ((file-directory-p dir)))
    dir)
  "Root of the second-brain org repo, or nil when it is not on this machine.
Set from the ORG_REPO_DIR environment variable, and nil as well when that
variable points at a directory that does not exist.")

(defun my/org-file (relative)
  "Return the absolute path of RELATIVE inside `my/org-repo'.
Signal an error when there is no org repo on this machine, so that a command
reached through a stale binding fails loudly instead of quietly creating a
file in an unexpected place."
  (unless my/org-repo
    (user-error "No org repo on this machine (set ORG_REPO_DIR)"))
  (expand-file-name relative my/org-repo))

;;; before-init ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
