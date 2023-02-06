;;; Utility functions

;; Visual bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#cc6666")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Disable backtick pair
(defun exordium-electric-mode-add-back-tick ()
  nil)

;; C-u M-q unfill-paragraph
(defun fill-unfill-paragraph (arg)
  (interactive "P")
  (if arg
      (unfill-paragraph)
    (fill-paragraph)))
(define-key global-map (kbd "M-q") 'fill-unfill-paragraph)

(defalias 'repunctuate 'bde-repunctuate)
(defun depunctuate ()
  "Put ONE space at the end of sentences in the selected region
or comment block. See also `repunctuate-sentences'."
  (interactive)
  (let (beginning end)
    (cond ((region-active-p)
           (setq beginning (region-beginning)
                 end (region-end)))
          ((bde-in-comment-p)
           (setq beginning (bde-comment-beginning)
                 end (bde-comment-end))))
    (if (and beginning end)
        (save-excursion
          (goto-char beginning)
          (while (re-search-forward "\\([]\"')]?\\)\\([.?!]\\)\\([]\"')]?\\) +" end t)
            (replace-match "\\1\\2\\3 " nil nil))
          (fill-paragraph))
      (message "No region or comment"))))

(defun msg ()
  "Create a new scratch bufer to edit markdown, that does not
  need to be safed."
  (interactive)
  (let ((buffer (generate-new-buffer (make-temp-name "scratch-"))))
    (switch-to-buffer buffer)
    (setq-local default-directory "/Users/pgrenet/Documents/scratch")
    (markdown-mode)))


;; Meta and Super key for the BB keyboard

(defun pc-mode ()
  "Binds Alt to Meta and the Windows key to Super."
  (interactive)
  (setq mac-option-modifier  'super
        mac-command-modifier 'meta)
  (message "PC keyboard mode (ALT is meta)"))

(defun mac-mode ()
  "Binds Option to Meta and Command to Super."
  (interactive)
  (setq mac-option-modifier  'meta
        mac-command-modifier 'alt)
  (message "Mac keyboard mode (OPTION is meta)"))

(pc-mode)


;; Scroll

(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key (kbd "C-M-<down>") 'gcm-scroll-down)
(global-set-key (kbd "C-M-<up>") 'gcm-scroll-up)


;; Helm
(setq helm-autoresize-max-height 35
      helm-autoresize-min-height 35)
(helm-autoresize-mode t)

;; C-x b replacement for switch-to-buffer
(define-key global-map (kbd "C-x b") 'helm-buffers-list)

;; (setq helm-boring-buffer-regexp-list '("\\*helm.+\\*"
;;                                        "\\*Messages\\*"
;;                                        "\\*Compile-Log\\*"
;;                                        "\\*Minibuf.+\\*"))


;; Keys
(global-set-key [(f6)] #'exordium-highlight-symbol)
(global-set-key [(end)] #'move-end-of-line)
(global-set-key [(home)] #'move-beginning-of-line)

;; Emojis on mac: add shift to Meta-Ctrl-Space for selection
(global-set-key (kbd "C-M-s-<SPC>") #'mark-sexp)

;; Super(Option)-q for Unicode characters
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; Symbols
(define-key key-translation-map (kbd "s-q s") (kbd "★")) ; star
(define-key key-translation-map (kbd "s-q c") (kbd "✓")) ; checkmark
(define-key key-translation-map (kbd "s-q h") (kbd "❤")) ; heart
(define-key key-translation-map (kbd "s-q u") (kbd "☂")) ; umbrella
(define-key key-translation-map (kbd "s-q b") (kbd "▌")) ; block
(define-key key-translation-map (kbd "s-q SPC") (kbd "█")) ; black space
;; Modal logic
(define-key key-translation-map (kbd "s-q n") (kbd "□")) ; necessary
(define-key key-translation-map (kbd "s-q p") (kbd "♢")) ; possible
(define-key key-translation-map (kbd "s-q a") (kbd "∀")) ; all
(define-key key-translation-map (kbd "s-q e") (kbd "∃")) ; there exists
;; Arrows
;; https://www.key-shortcut.com/en/writing-systems/35-symbols/arrows
(define-key key-translation-map (kbd "s-q <right>") (kbd "⮕"))
(define-key key-translation-map (kbd "s-q <left>") (kbd "⬅"))
(define-key key-translation-map (kbd "s-q <up>") (kbd "⬆"))
(define-key key-translation-map (kbd "s-q <down>") (kbd "⬇"))
(define-key key-translation-map (kbd "s-q S-<right>") (kbd "⇒"))
(define-key key-translation-map (kbd "s-q S-<left>") (kbd "⇐"))
(define-key key-translation-map (kbd "s-q =") (kbd "⇔"))
;; Greeks
(define-key key-translation-map (kbd "s-q A") (kbd "α")) ; alpha
(define-key key-translation-map (kbd "s-q B") (kbd "β")) ; beta
(define-key key-translation-map (kbd "s-q D") (kbd "Δ")) ; delta
(define-key key-translation-map (kbd "s-q C") (kbd "ε")) ; epsilon
(define-key key-translation-map (kbd "s-q L") (kbd "λ")) ; lambda
(define-key key-translation-map (kbd "s-q S") (kbd "Σ")) ; sigma
(define-key key-translation-map (kbd "s-q m") (kbd "ɸ")) ; phi
;; Set theory
(define-key key-translation-map (kbd "s-q 0") (kbd "∅")) ; empty set
(define-key key-translation-map (kbd "s-q i") (kbd "∩")) ; empty set
(define-key key-translation-map (kbd "s-q u") (kbd "∪")) ; empty set

;; Tab for autocomplete of directory path with Helm (default is C-j)
(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)


;;; Symbols, see https://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/

(defconst my-prettify-symbol-alist '(("=>" . ?⇒)
                                     ("->" . ?⮕)))
(add-hook 'org-mode-hook (lambda ()
                           (setq prettify-symbols-alist my-prettify-symbol-alist)))
(add-hook 'markdown-mode-hook (lambda ()
                           (setq prettify-symbols-alist my-prettify-symbol-alist)))
(global-prettify-symbols-mode +1)


;;; Company word autocomplete

;; Prevent automatic downcase of completions
(setq company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil)


;; Git

;; Git Gutter Fringe
(if (fboundp 'fringe-mode) (fringe-mode '8))
;; places the git gutter outside the margins.
(setq-default fringes-outside-margins t)
;; thin fringe bitmaps
(define-fringe-bitmap 'git-gutter-fr:added [224]
  nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224]
  nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
  nil nil 'bottom)

;; Diffs
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq magit-diff-refine-hunk t)

(add-hook 'magit-status-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

(setq interpreter-mode-alist (append interpreter-mode-alist '(("ksh93" . shell-script-mode))))

;; gfm-mode by default
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; 2-way diff in ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq magit-ediff-dwim-show-on-hunks t)


;; Treemacs

(defun treemacs-current ()
  "Open treemacs for the current buffer"
  (interactive)
  (when (eq (treemacs-current-visibility) 'none)
    (treemacs)
    (treemacs-display-current-project-exclusively)))


;; Tabs
;; See https://github.com/ema2159/centaur-tabs

(use-package all-the-icons
  :if (display-graphic-p))

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (progn
;;     (centaur-tabs-headline-match)
;;     (setq centaur-tabs-set-icons t)
;;     (setq centaur-tabs-set-bar 'over)
;;     (setq centaur-tabs-set-modified-marker t)
;;     (setq centaur-tabs-modified-marker "●")
;;     ;;
;;     (defun centaur-tabs-buffer-groups ()
;;       "`centaur-tabs-buffer-groups' control buffers' group rules."
;;       (list
;;        (cond
;; 	    ((or (string-equal "*" (substring (buffer-name) 0 1))
;; 	         (memq major-mode '(magit-process-mode
;; 				                magit-status-mode
;; 				                magit-diff-mode
;; 				                magit-log-mode
;; 				                magit-file-mode
;; 				                magit-blob-mode
;; 				                magit-blame-mode)))
;; 	     "Emacs")
;; 	    ((derived-mode-p 'prog-mode)
;; 	     "Editing")
;; 	    ((derived-mode-p 'dired-mode)
;; 	     "Dired")
;; 	    ((memq major-mode '(helpful-mode
;; 			                help-mode))
;; 	     "Help")
;; 	    ((memq major-mode '(org-mode
;; 			                org-agenda-clockreport-mode
;; 			                org-src-mode
;; 			                org-agenda-mode
;; 			                org-beamer-mode
;; 			                org-indent-mode
;; 			                org-bullets-mode
;; 			                org-cdlatex-mode
;; 			                org-agenda-log-mode
;; 			                diary-mode
;;                             markdown-mode
;;                             gfm-mode))
;; 	     "Notes")
;; 	    (t
;; 	     (centaur-tabs-get-group-name (current-buffer))))))
;;     ;;
;;     (centaur-tabs-mode t))
;;   :bind
;;   ("M-<prior>" . centaur-tabs-backward)
;;   ("M-<next>" . centaur-tabs-forward))



;; Multiple cursors
;; (use-package multiple-cursors
;;   :bind (("C->"     . mc/mark-next-like-this)
;;          ("C-<"     . mc/mark-previous-like-this)
;;          ("C-c C-<" . mc/mark-all-like-this)))


;; Atomic chrome
(require 'atomic-chrome)
(setq atomic-chrome-default-major-mode 'markdown-mode
      atomic-chrome-buffer-open-style 'full)
(with-demoted-errors "Can't start atomic-chrome: %S"
  (atomic-chrome-start-server))
