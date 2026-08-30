;;;; Package --- summary: Code review in Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Two tools:
;;
;;   my-pr-review.el    pull request reviews, via the pr-review package
;;   my-diff-review.el  reviewing uncommitted local changes, and exporting the
;;                      comments as markdown for a coding agent
;;
;; The second is built on the first's diff renderer, so both drive the same
;; way.  See CODE_REVIEWS.md.
;;
;; Nothing here is site-specific.  A GitHub Enterprise instance is added by
;; setting `my/pr-review-extra-forges' from a tap's `before-init.el'.
;;; Code:

;; Pull request reviews.
(load "~/.emacs.d/taps/code-review/my-pr-review.el")

;; Review local (uncommitted) changes and export the comments as markdown for a
;; coding agent.  Autoloaded so it costs nothing at startup; it pulls in
;; pr-review on first use.
(autoload 'my/review-local-changes
  "~/.emacs.d/taps/code-review/my-diff-review"
  "Review the local changes in the current repository." t)
(global-set-key (kbd "C-c M-l") #'my/review-local-changes)

;; Let a coding agent request a review and block until it is done.  Loaded
;; eagerly: it installs a `server-visit-hook', which has to be in place before
;; the agent's emacsclient arrives.
(load "~/.emacs.d/taps/code-review/my-review-server.el")

;;; after-init ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
