;;;; Package --- summary : Local extensions to Exordium: Org Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-element)

(setq org-hide-leading-stars t)
(setq org-fontify-quote-and-verse-blocks t)

;; Disable logbook drawers (no timestamps on state changes)
(setq org-log-done nil)
(setq org-log-into-drawer nil)
(setq org-log-repeat nil)
(setq org-todo-keywords
      '((sequence "TODO" "WORK" "WAIT" "|" "STOP" "BLOCKED" "POSTPONED" "QUESTIONED" "DONE")
        (sequence "READY" "REVIEW" "|" "REJECT" "PROCEED")))

;;; Remove the hook added by init-org-mode.el
(remove-hook 'org-mode-hook 'turn-on-visual-line-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 100)))

;; Enable 'eval' in a file's local variables. This prevents Emacs for asking
;; for confirmation each time such as file is loaded.
(setq org-confirm-babel-evaluate nil)


;;; Keys
;;
;; The C-c o bindings and the small commands behind them live in org-util.el.

(load-file "~/.emacs.d/taps/org-mode/org-util.el")


;;; Look

(defvar exordium-theme)
(defvar exordium-catppuccin-flavor)
(eval-when-compile
  (require 'color-theme-catppuccin))

(when (member exordium-theme '(catppuccin-mocha))
  (require 'color-theme-catppuccin)
  (with-catppuccin-colors
   exordium-catppuccin-flavor
   (set-face-attribute 'org-headline-done nil :foreground overlay0)))

;; Put one line before each header-1
(setq org-cycle-separator-lines -1)

(setq org-ellipsis "⤵")  ;; or "…"

;; org-fontify-todo-headline -> TODO => face org-headline-todo
;;(setq org-not-done-regexp "STOP")
;;(setq org-fontify-todo-headline t)

;; Spell check
(add-hook 'org-mode-hook 'flyspell-prog-mode)

;; Default width for images
;; Use:  #+attr_html: :width 800px
(setq org-image-actual-width nil)



;;; svg-tag-mode
;;
;; The status pills, date tags and progress cookies live in org-svg-tags.el.

(load-file "~/.emacs.d/taps/org-mode/org-svg-tags.el")


;;; Overline startup option

(defvar my-org-mode-overline nil)
(add-to-list 'org-startup-options '("overline" my-org-mode-overline t))
(when (eq exordium-theme 'tomorrow-night)
  (add-hook 'org-mode-hook
            (lambda ()
              (when my-org-mode-overline
                (face-remap-add-relative 'org-level-1
                                         (with-tomorrow-colors 'night `(:overline ,green)))))))
(when (eq exordium-theme 'catppuccin-mocha)
  (add-hook 'org-mode-hook
            (lambda ()
              (when my-org-mode-overline
                (face-remap-add-relative 'org-level-1
                                         (with-catppuccin-colors 'mocha `(:overline ,blue)))))))


;;; Task list

(require 'cl-lib)

;;; Capture task
;;; See http://orgmode.org/manual/Capture-templates.html#Capture-templates

;;(setq org-src-window-setup 'slit-window-right) ; does not work

;; Auto-select the name placeholder in capture buffers so typing replaces it.
;; This works because delete-selection-mode is enabled (in init-look-and-feel.el).
(defvar my/org-capture--select-end nil
  "End position for word to select in capture buffer.")

(defun my/org-capture--activate-selection ()
  "Activate selection and remove self from `post-command-hook'."
  (push-mark my/org-capture--select-end nil t)
  (setq deactivate-mark nil)
  (remove-hook 'post-command-hook #'my/org-capture--activate-selection t))

(defun my/org-capture-select-word-at-point ()
  "Select the word at point in a capture buffer so typing replaces it."
  (when (looking-at "\\w+")
    (setq my/org-capture--select-end (match-end 0))
    (add-hook 'post-command-hook #'my/org-capture--activate-selection nil t)))

(add-hook 'org-capture-mode-hook #'my/org-capture-select-word-at-point)

;; Org Capture = Meta-F12 + F13
(define-key global-map [(meta f12)] #'org-capture)

;; An attempt to make it work on Emacs-Plus, which unfortunately does not work.
;; I think Emacs-Plus (NS/Cocoa build) translates <f13> into <delete>.
(define-key input-decode-map [f13] nil)
(define-key local-function-key-map [f13] nil)

(define-key global-map [(f13)] #'org-capture)
(define-key global-map [(delete)] #'org-capture)

;; Capture from anywhere in the macOS desktop, via Hammerspoon and emacsclient.
;; See org-capture-frame.el and ~/dotfiles/init.lua.
(load-file "~/.emacs.d/taps/org-mode/org-capture-frame.el")


;;; File location

;; The `org:' link abbreviation points at the org repo; see org-notes.el.

;; Archive
(setq org-archive-location "%s_archive::datetree/")

(setf (alist-get 'file org-link-frame-setup) #'find-file)


;;; Make Tab bring up Company for file or image links, in addition to C-.

(defun my/org-complete-file-in-link ()
  "Trigger file completion when point is inside a [[file-path link."
  (when (and (looking-back "\\[\\[[^][\n]*" (line-beginning-position))
             (not (looking-back "\\]\\[" (- (point) 2))))
    (company-complete)
    t))

(add-hook 'org-cycle-tab-first-hook #'my/org-complete-file-in-link)


;;; Backticks for code snippets

;; Highlight single backticks as inline code in Org buffers
(defun my/org-add-markdown-inline-code ()
  "Highlight `inline code` in org-mode like Markdown."
  (font-lock-add-keywords
   nil
   '(("\\(^\\|[^\\]\\)\\(`\\([^`\n]+\\)`\\)"
      (2 'org-code t)))))

(add-hook 'org-mode-hook #'my/org-add-markdown-inline-code)

;; Make exporters handle backticks
;; By default, Org’s exporters won’t interpret backticks as inline code.
;; We can add a custom link-like syntax parser for `code` before export.
(defun my/org-md-inline-code-filter (text backend info)
  "Convert `code` to ~code~ for Org exporters."
  (when (org-export-derived-backend-p backend 'html 'latex 'ascii 'md)
    (replace-regexp-in-string
     "\\(^\\|[^\\]\\)`\\([^`\n]+\\)`"
     "\\1~\\2~"
     text t nil)))

(add-to-list 'org-export-filter-plain-text-functions
             #'my/org-md-inline-code-filter)


;;; Calfw mode
;;; https://github.com/kiwanami/emacs-calfw

(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday

(use-package calfw
  :ensure t
  :config
  (progn
    ;; Theme
    (when (member exordium-theme '(catppuccin-mocha))
      (require 'color-theme-catppuccin)
      (with-catppuccin-colors
       exordium-catppuccin-flavor
       (set-face-attribute 'calfw-title-face nil :foreground blue)
       (set-face-attribute 'calfw-toolbar-button-off-face nil :foreground blue :background surface1)
       (set-face-attribute 'calfw-toolbar-button-on-face nil :foreground text :background surface1)
       (set-face-attribute 'calfw-header-face nil :foreground sky)
       (set-face-attribute 'calfw-saturday-face nil :foreground lavender)
       (set-face-attribute 'calfw-sunday-face nil :foreground lavender)
       (set-face-attribute 'calfw-holiday-face nil :foreground peach :background surface0)

       (set-face-attribute 'calfw-today-face nil :background surface0 :foreground text)
       (set-face-attribute 'calfw-today-title-face nil :background green :foreground base)
       (set-face-attribute 'calfw-annotation-face nil :foreground red)))
    ;; Table display
    (setq calfw-fchar-junction ?╋
          calfw-fchar-vertical-line ?┃
          calfw-fchar-horizontal-line ?━
          calfw-fchar-left-junction ?┣
          calfw-fchar-right-junction ?┫
          calfw-fchar-top-junction ?┯
          calfw-fchar-top-left-corner ?┏
          calfw-fchar-top-right-corner ?┓)))

(use-package calfw-org
  :ensure t
  :config
  (progn
    (defun open-todos-calendar-view ()
      "Open the calendar view."
      (interactive)
      (split-window-vertically)
      ;; Fix the bug where it takes a little too much width
      (let ((w (frame-width (selected-frame))))
        (set-frame-width (selected-frame) (- w 4))
        (calfw-org-open-calendar)
        ;;(cfw:open-calendar-buffer :contents-sources (list (cfw:org-create-source "#8abeb7")))
        (set-frame-width (selected-frame) w)))

    (defun close-todos-calendar-view ()
      "Close the calendar view."
      (interactive)
      (kill-this-buffer)
      (delete-other-windows))

    (with-eval-after-load 'org
      (bind-key [(f9)] #'open-todos-calendar-view org-mode-map))
    (with-eval-after-load 'calfw
      (bind-key [(f9)] #'close-todos-calendar-view calfw-calendar-mode-map))))


;; Org modern indent

;; org-modern-indent uses org-indent, and expects it to be enabled to achieve
;; its formatting. To activate org-indent-mode by default in all org files, set
;; org-startup-indented=t.
(setq org-startup-indented t)

(load "~/.emacs.d/taps/org-mode/org-modern-indent.el")

;; Enable it for all files
(add-hook 'org-mode-hook #'org-modern-indent-mode)


;; Org to markdown converter
(load "~/.emacs.d/taps/org-mode/org-to-markdown.el")


;; Mermaid
;; Mermaid syntax files (.mmd)
(use-package mermaid-mode
  :ensure t
  :mode "\\.mmd\\'")

;; Org-Babel Mermaid
;; Mermaid config schema documentation: https://mermaid.js.org/config/schema-docs/config.html
(use-package ob-mermaid
  :ensure t
  :after org
  :config
  ;; Point to the Mermaid CLI binary
  (setq ob-mermaid-cli-path (or (executable-find "mmdc")
                                "/opt/homebrew/bin/mmdc"))
  (setenv "PUPPETEER_EXECUTABLE_PATH" "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  ;; Enable Mermaid in Org-Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)))
  ;; Auto-refresh inline images after executing a block
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images))


;;; Per-file line number control via #+STARTUP: directive

;; Declare external variables from init-prefs.el to avoid free variable warnings
(defvar exordium-inhibit-line-numbers-modes)
(defvar exordium-inhibit-line-numbers-star-buffers)
(defvar exordium-inhibit-line-numbers-buffer-size)

;; Buffer-local variable to track line number preference
(defvar-local exordium-buffer-line-numbers nil
  "Buffer-local override for line number display in Org-Mode.
When set to 'show, always show line numbers in this buffer.
When set to 'hide, never show line numbers in this buffer.
When nil (default), use global settings from `exordium-inhibit-line-numbers-modes'.")

;; Register org-mode startup options: #+STARTUP: showlinenum / hidelinenum
(add-to-list 'org-startup-options '("showlinenum" exordium-buffer-line-numbers show))
(add-to-list 'org-startup-options '("hidelinenum" exordium-buffer-line-numbers hide))

;; Redefine the inhibit function to check buffer-local override
;; This overrides the function from init-linum.el
(defun exordium--inhibit-line-numbers-p ()
  "Return non nil if line numbers should be inhibited in current buffer.
Otherwise return nil.
This redefinition adds support for buffer-local override in Org-Mode."
  (or (minibufferp)
      ;; Check buffer-local override first (for Org-Mode)
      (eq exordium-buffer-line-numbers 'hide)
      ;; If explicitly set to show, don't inhibit
      (and (not (eq exordium-buffer-line-numbers 'show))
           (or (and exordium-inhibit-line-numbers-modes
                    (cl-find-if (lambda (mode)
                                  (derived-mode-p mode))
                                exordium-inhibit-line-numbers-modes))
               (and exordium-inhibit-line-numbers-star-buffers
                    (string-match (rx string-start "*") (buffer-name)))
               (and exordium-inhibit-line-numbers-buffer-size
                    (> (buffer-size) exordium-inhibit-line-numbers-buffer-size))))))

;; Hook to refresh line numbers when opening an org file
(defun exordium-org-refresh-line-numbers ()
  "Refresh line number display based on buffer-local setting."
  (cond
   ((eq exordium-buffer-line-numbers 'show)
    (display-line-numbers-mode 1))
   ((eq exordium-buffer-line-numbers 'hide)
    (display-line-numbers-mode -1))
   ;; Otherwise let the global mode decide
   (t nil)))

(add-hook 'org-mode-hook #'exordium-org-refresh-line-numbers)

;; C-c o T: Toggle table format (standard <-> box-drawing)
(load-file "~/.emacs.d/taps/common/table-format.el")
(define-key org-mode-map (kbd "C-c o T") #'my/org-table-toggle-format)

;; C-c o R: Resize table to fit fill-column
(define-key org-mode-map (kbd "C-c o R") #'my/org-table-resize-to-fill-column)

;; C-c o #: Insert/update the file description (#+description: keyword)
(load-file "~/.emacs.d/taps/common/description-field.el")
(define-key org-mode-map (kbd "C-c o #") #'my/update-description)


;; Live HTML preview in an xwidget (C-c o v / C-c o V). See org-preview.el.
(load-file "~/.emacs.d/taps/org-mode/org-preview.el")


;;; The second brain
;;
;; Everything above is about Org Mode itself and works on any machine.  What
;; follows addresses the notes repo, which only some machines have: browsing it
;; (F12), capturing and refiling into it (M-F12, C-c o t/w/b), syncing it to
;; Google Drive, and the DRQS references its work notes are full of.
;; `my/org-repo' is nil elsewhere -- see taps/common/before-init.el for where
;; the path comes from.

(defvar my/org-repo)

(if my/org-repo
    (progn
      (load-file "~/.emacs.d/taps/org-mode/org-notes.el")
      (load-file "~/.emacs.d/taps/org-mode/org-capture-refile.el")
      (load-file "~/.emacs.d/taps/org-mode/org-drive-sync.el")
      (load-file "~/.emacs.d/taps/org-mode/org-drqs.el"))
  (message "org-mode tap: no org repo on this machine (set ORG_REPO_DIR), \
skipping notes, capture, sync and DRQS"))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
