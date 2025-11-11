;;; Utility functions

(setq-default indicate-empty-lines t)

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

;; ;; Change the order of buffers in switch-to-buffer: show non-system buffers first
;; (defun pg-around-helm-buffers-sort-transformer (candidates source)
;;   candidates)

;; (advice-add 'helm-buffers-sort-transformer
;;             :override #'pg-around-helm-buffers-sort-transformer)

;; C-x f is better than C-x h
(define-key global-map (kbd "C-c f") 'helm-projectile)

;; (setq helm-boring-buffer-regexp-list '("\\*helm.+\\*"
;;                                        "\\*Messages\\*"
;;                                        "\\*Compile-Log\\*"
;;                                        "\\*Minibuf.+\\*"))


;; Flyspell
(defun my/flyspell-correct-word-mouse (event)
  "Correct the word at mouse EVENT using flyspell."
  (interactive "e")
  (mouse-set-point event)
  (let ((flyspell-auto-correct-binding nil))
    (flyspell-correct-word-before-point)))

;; Bind to mouse-3 globally or in specific modes
(global-set-key [mouse-3] #'my/flyspell-correct-word-mouse)

;; Ignore uppercase words
;; (defun my/flyspell-ignore-uppercase-word (word)
;;   "Return nil if WORD is entirely uppercase, t otherwise."
;;   (not (string-match-p "^[[:upper:]]+$" word)))

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (setq-local flyspell-generic-check-word-predicate 'my/flyspell-ignore-uppercase-word)))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq-local flyspell-generic-check-word-predicate 'my/flyspell-ignore-uppercase-word)))




;; Keys
(global-set-key [(f6)] #'symbol-overlay-put)
(global-set-key [(f7)] #'flyspell-auto-correct-previous-word)
(global-set-key [(end)] #'move-end-of-line)
(global-set-key [(home)] #'move-beginning-of-line)

;; Emojis on mac: add shift to Meta-Ctrl-Space for selection
(global-set-key (kbd "C-M-s-<SPC>") #'mark-sexp)

;; Super(Option)-q for Unicode characters
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Math
(global-set-key (kbd "s-q m n") "□") ; necessary
(global-set-key (kbd "s-q m p") "♢") ; possible
(global-set-key (kbd "s-q m a") "∀") ; all
(global-set-key (kbd "s-q m e") "∃") ; there exists
(global-set-key (kbd "s-q m I") "∞")

(global-set-key (kbd "s-q m 0") "∅") ; empty set
(global-set-key (kbd "s-q m i") "∩") ; intersection
(global-set-key (kbd "s-q m u") "∪") ; union
(global-set-key (kbd "s-q m [") "⊂")
(global-set-key (kbd "s-q m ]") "⊃")

;; Arrows
;; https://www.key-shortcut.com/en/writing-systems/35-symbols/arrows
;; (define-key key-translation-map (kbd "s-q <right>") (kbd "⮕"))
;; (define-key key-translation-map (kbd "s-q <left>") (kbd "⬅"))
;; (define-key key-translation-map (kbd "s-q <up>") (kbd "⬆"))
;; (define-key key-translation-map (kbd "s-q <down>") (kbd "⬇"))
;; (define-key key-translation-map (kbd "s-q S-<right>") (kbd "⇒"))
;; (define-key key-translation-map (kbd "s-q S-<left>") (kbd "⇐"))
;; (define-key key-translation-map (kbd "s-q =") (kbd "⇔"))

;; Greeks
(global-set-key (kbd "s-q g a") "α")
(global-set-key (kbd "s-q g b") "β")
(global-set-key (kbd "s-q g g") "γ")
(global-set-key (kbd "s-q g d") "δ")
(global-set-key (kbd "s-q g e") "ε")
(global-set-key (kbd "s-q g z") "ζ")
(global-set-key (kbd "s-q g h") "η")
(global-set-key (kbd "s-q g q") "θ")
(global-set-key (kbd "s-q g i") "ι")
(global-set-key (kbd "s-q g k") "κ")
(global-set-key (kbd "s-q g l") "λ")
(global-set-key (kbd "s-q g m") "μ")
(global-set-key (kbd "s-q g n") "ν")
(global-set-key (kbd "s-q g x") "ξ")
(global-set-key (kbd "s-q g o") "ο")
(global-set-key (kbd "s-q g p") "π")
(global-set-key (kbd "s-q g r") "ρ")
(global-set-key (kbd "s-q g s") "σ")
(global-set-key (kbd "s-q g t") "τ")
(global-set-key (kbd "s-q g u") "υ")
(global-set-key (kbd "s-q g f") "ϕ")
(global-set-key (kbd "s-q g j") "φ")
(global-set-key (kbd "s-q g c") "χ")
(global-set-key (kbd "s-q g y") "ψ")
(global-set-key (kbd "s-q g w") "ω")
(global-set-key (kbd "s-q g A") "Α")
(global-set-key (kbd "s-q g B") "Β")
(global-set-key (kbd "s-q g G") "Γ")
(global-set-key (kbd "s-q g D") "Δ")
(global-set-key (kbd "s-q g E") "Ε")
(global-set-key (kbd "s-q g Z") "Ζ")
(global-set-key (kbd "s-q g H") "Η")
(global-set-key (kbd "s-q g Q") "Θ")
(global-set-key (kbd "s-q g I") "Ι")
(global-set-key (kbd "s-q g K") "Κ")
(global-set-key (kbd "s-q g L") "Λ")
(global-set-key (kbd "s-q g M") "Μ")
(global-set-key (kbd "s-q g N") "Ν")
(global-set-key (kbd "s-q g X") "Ξ")
(global-set-key (kbd "s-q g O") "Ο")
(global-set-key (kbd "s-q g P") "Π")
(global-set-key (kbd "s-q g R") "Ρ")
(global-set-key (kbd "s-q g S") "Σ")
(global-set-key (kbd "s-q g T") "Τ")
(global-set-key (kbd "s-q g U") "Υ")
(global-set-key (kbd "s-q g F") "Φ")
(global-set-key (kbd "s-q g J") "Φ")
(global-set-key (kbd "s-q g C") "Χ")
(global-set-key (kbd "s-q g Y") "Ψ")
(global-set-key (kbd "s-q g W") "Ω")

;; Flags
(global-set-key (kbd "s-q f g") "🟩") ; green
(global-set-key (kbd "s-q f a") "🟧") ; amber
(global-set-key (kbd "s-q f r") "🟥") ; red

;; Tab for autocomplete of directory path with Helm (default is C-j)
(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)

;; insert date
(defun exordium-insert-today ()
  "Insert today's date as mm/dd/yyyy."
  (interactive)
  (insert (format-time-string "%m/%d/%Y")))

(global-set-key (kbd "C-c C-.") #'exordium-insert-today)


;;; Symbols, see https://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/

(defconst my-prettify-symbol-alist '(("=>" . ?⇒)
                                     ("->" . ?⮕)
                                     ("<=" . ?⇐)
                                     ("<-" . ?⬅)
                                     ("<->" . ?⬌)))
(add-hook 'org-mode-hook (lambda ()
                           (setq prettify-symbols-alist my-prettify-symbol-alist)))
(add-hook 'markdown-mode-hook (lambda ()
                           (setq prettify-symbols-alist my-prettify-symbol-alist)))
(global-prettify-symbols-mode +1)


;;; Company word autocomplete

;; Prevent automatic downcase of completions
(setq company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil)

;; Start completing only after 3 characters
;; Don't try to complete numbers
(setq company-minimum-prefix-length 3
      company-dabbrev-char-regexp "\\(?:[[:alpha:]]\\|\\s_\\)")

;; By default RET inserts the selected candidate (company-complete-selection)
;; and TAB inserts the common part of all completion candidates (company-complete-common).
;; RET isn't great when your word is finished, but has possible completions, and you just want to
;; go to the next line. => Let's just use TAB to complete. That also works like zsh.
(with-eval-after-load 'company
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection))


;; Git

;; Git Gutter
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

;; Make git gutter refresh after save
(add-hook 'after-save-hook #'git-gutter:update-all-windows)

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

(global-set-key (kbd "C-c t") #'treemacs-select-window)


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



;; On the Spaces screen, click the copy-to-clipboard button while hovering over the SSH icon
;; on the active workspace you want to access.
;;
;; In emacs, use M-x open-devx-space-ssh
;; When prompted for the ssh string, paste it in from the clipboard and press enter.

;; DevX Spaces
(defun open-devx-space-ssh ()
  (interactive)
  (setq ssh-string (read-string "Spaces ssh string: " nil nil ""))
  (save-match-data
    (and (string-match "ssh -t\s\\([-a-z0-9]+\\).* -it \\([a-z0-9]+\\) bash\"" ssh-string)
         (setq spaces-host (match-string 1 ssh-string)
               docker-id (match-string 2 ssh-string))))
  (setq space (format "/ssh:%s.bloomberg.com|docker:%s:.."
                      spaces-host
                      docker-id))
  (message space)
  (dired space))

;;(global-set-key (kbd "C-c SPC") 'bb-open-devx-space-ssh)


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


;; Ultra scroll

;; Not in Elpa yet. Installation:
;;(package-vc-install '(ultra-scroll :vc-backend Git :url  "https://github.com/jdtsmith/ultra-scroll"))

;; Activation:
(use-package ultra-scroll
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))


;; Vterm
(use-package vterm
  :ensure t)

;;; after-init ends here
