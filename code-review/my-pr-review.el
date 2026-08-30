;;;; my-pr-review.el --- Pull request reviews in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configures https://github.com/blahgeek/emacs-pr-review.
;;
;; Forge (modules/init-forge.el) can approve and request changes, but has no
;; support for inline comments on diff lines, replying to review threads, or
;; resolving them.  pr-review does all three, and batches them into a single
;; pending review.
;;
;; Authentication goes through ghub, which reads a token from ~/.authinfo.
;; See CODE_REVIEWS.md for the setup, the key bindings and a walkthrough.
;;
;; A self-hosted GitHub Enterprise instance is added by setting
;; `my/pr-review-extra-forges' from a tap's `before-init.el' -- see the
;; docstring below.
;;
;; File is named "my-pr-review.el" rather than "pr-review-something.el" so it
;; can never shadow one of the package's own pr-review-*.el files.

;;; Code:

(defvar my/pr-review-extra-forges nil
  "Site-specific forges, prepended to `pr-review-forges-alist'.

Each entry is (HOST . (FORGE-TYPE API-HOST USERNAME)), the format
`pr-review-forges-alist' expects.  The first entry of that alist is the
default forge, the one `pr-review-notification' and `pr-review-search' use
when no host is given, so anything set here takes precedence over
github.com.

Set it from a tap's `before-init.el', which Exordium loads ahead of every
tap's `after-init.el'.  A `defvar' here will not clobber a value already
set that way.  For example:

  (defvar my/pr-review-extra-forges
    \\='((\"github.example.com\"
       . (github \"github.example.com/api/v3\" \"your-username\"))))")

(use-package pr-review
  :ensure t
  :defer t
  :commands (pr-review
             pr-review-open
             pr-review-notification
             pr-review-search
             pr-review-search-open)
  :custom
  (pr-review-forges-alist
   (append my/pr-review-extra-forges
           '(("github.com" . (github nil nil))))))

;; pr-review renders comment bodies as HTML through shr, whose `shr-text' face
;; inherits `variable-pitch' -- spec'd as the generic family "Sans Serif",
;; which the platform resolves, and which on macOS can land on a serif.
;; Proportional prose is deliberate in shr (it also keeps `shr-code' fixed
;; pitch, like a browser), but a review comment sits among code, so matching
;; the buffer's own font keeps indentation honest and needs no font name.
;;
;; Buffer-local rather than a theme setting: `shr-text' is shared with eww and
;; HTML mail, which should keep their proportional text.  Not `shr-use-fonts'
;; either -- that switches shr from pixel- to character-based measurement,
;; while pr-review sets `shr-indentation' in pixels.
;;
;; To go back to shr's own proportional font, remove the hook below -- that is
;; the whole change.  To keep proportional text but avoid whatever "Sans Serif"
;; resolves to, remap to a real family instead of `default':
;;
;;   (face-remap-add-relative 'shr-text :family "Helvetica Neue" :height 130)
;;
;; using `add-relative' rather than `set-base' there, so `shr-text' keeps its
;; own attributes and only the family and size are overridden.
(defun my/pr-review-use-buffer-font ()
  "Render shr text in this buffer using the buffer's own font."
  (face-remap-set-base 'shr-text 'default))

(add-hook 'pr-review-mode-hook #'my/pr-review-use-buffer-font)

(defun my/pr-review-visit-pullreq ()
  "Open the pull request at point in a `pr-review' buffer."
  (interactive)
  (if-let* ((topic (forge-current-topic))
            ((forge-pullreq-p topic))
            (url (forge-get-url topic)))
      (pr-review url)
    (user-error "Nothing at point that is a pull request")))

;; C-c M-v is free in both maps; exordium's forge bindings use C-c M-{p,r,d,c}.
(use-package forge
  :defer t
  :bind
  (:map forge-topic-mode-map
   ("C-c M-v" . #'my/pr-review-visit-pullreq)))

(use-package magit
  :defer t
  :bind
  (:map magit-status-mode-map
   ("C-c M-v" . #'my/pr-review-visit-pullreq)))

(provide 'my-pr-review)

;;; my-pr-review.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
