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
           '(("github.com" . (github nil nil)))))
  ;; `pr-review-notification' fetches GET /notifications with all=true when
  ;; this is non-nil, which is the package default.  That returns every thread
  ;; the API still knows about, and the API has no notion of the web UI's
  ;; "Done" -- marking a notification done there only marks it read, so it
  ;; keeps coming back and the buffer accumulates months of them.  Unread-only
  ;; matches what the web inbox shows.  `C-c C-t' toggles it back per buffer.
  (pr-review-notification-include-read nil))

;; Dired-style single-key marks in the notification list.
;;
;; The notification buffer is a `tabulated-list-mode' derivative and read-only,
;; so bare letters are free -- and the package already defines exactly this set
;; for evil users (`pr-review--notification-mode-map-setup-for-evil'), leaving
;; plain Emacs with only the `C-c C-<key>' forms.  The mark/execute split is
;; already dired's: `d' and `r' only set a mark, `x' is what talks to the API.
;;
;; The feature is `pr-review-notification', not `pr-review' -- nothing in
;; pr-review.el requires it, it arrives via the autoload on
;; `pr-review-notification', so an eval-after-load on `pr-review' would run too
;; early.  `:ensure nil' because this is a feature inside the pr-review
;; package, not a package of its own, and Exordium sets
;; `use-package-always-ensure'.
;;
;; `x' is confirmed for `D' marks by the advice below.
(defvar pr-review--notification-marks)  ; buffer-local, in pr-review-notification.el

(defun my/pr-review-notification-confirm-delete (orig-fun &rest args)
  "Confirm pending `D' marks, then apply ORIG-FUN to ARGS.

Advice for `pr-review-notification-execute-mark', the one command in the
notification buffer that reaches the API.  A `D' mark is not dired's
delete: executing it calls DELETE on the thread's subscription, which
unsubscribes from the pull request itself, so a stray `d' silently stops
future notifications for a PR you are reviewing.  `-' (read) marks are
harmless and pass through unprompted, which keeps the common case at one
keystroke."
  (let ((n (seq-count (lambda (mark) (eq (nth 1 mark) 'delete))
                      pr-review--notification-marks)))
    (if (and (> n 0)
             (not (yes-or-no-p
                   (format "Unsubscribe from %d pull request%s (D mark%s)? "
                           n (if (= n 1) "" "s") (if (= n 1) "" "s")))))
        (message "Nothing executed; marks kept")
      (apply orig-fun args))))

(use-package pr-review-notification
  :ensure nil
  :defer t
  :bind
  (:map pr-review-notification-mode-map
   ("r" . #'pr-review-notification-mark-read)
   ("d" . #'pr-review-notification-mark-delete)
   ("u" . #'pr-review-notification-remove-mark)
   ("x" . #'pr-review-notification-execute-mark)
   ("o" . #'pr-review-notification-open-in-browser)
   ("t" . #'pr-review-notification-toggle-filter))
  ;; Advice rather than a wrapper command bound to `x', so the prompt also
  ;; covers `C-c C-s' and evil's `x', not just the binding added above.
  :config
  (advice-add 'pr-review-notification-execute-mark :around
              #'my/pr-review-notification-confirm-delete))

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
