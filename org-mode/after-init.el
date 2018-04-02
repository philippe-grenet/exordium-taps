;;;; Local extensions to Exordium: Org mode

(require 'org)

;;; Colors --------------------------------------------------------------------

(when (eq exordium-theme 'tomorrow-night)
  (setq org-priority-faces
        '((?A :foreground "#1d1f21" :background "#cc6666" :weight bold)
          (?B :foreground "#1d1f21" :background "#de935f" :weight bold)
          (?C :foreground "#1d1f21" :background "#b5bd68" :weight bold)))

  (setq org-cycle-separator-lines -1)

  (with-tomorrow-colors
   (tomorrow-mode-name)
   (setq org-todo-keyword-faces
         `(("TODO"     . (:foreground ,red
                          :weight bold :box nil))
           ("DONE"     . (:foreground ,green
                          :weight bold :box nil))
           ("WORK"     . (:foreground ,yellow
                          :weight bold :box nil))
           ("WAIT"     . (:foreground ,orange
                          :weight bold :box nil))
           ("PAUSE"    . (:foreground ,blue
                          :weight bold :box nil))
           ;; for hire:
           ("HIRED"    . (:background ,blue :foreground ,background
                          :weight bold :box nil))
           ("GOOD"     . (:background ,green :foreground ,background
                          :weight bold :box nil))
           ("MEDIUM"   . (:background ,orange :foreground ,background
                          :weight bold :box nil))
           ("REJECTED" . (:background ,red :foreground ,background
                          :weight bold :box nil))
           ("WITHDREW" . (:background ,red :foreground ,background
                          :weight bold :box nil))))))

;;; Task list -----------------------------------------------------------------

(define-key org-mode-map (kbd "C-c t") #'exordium-org-move-to-today)

(defun open-todos ()
  (interactive)
  (find-file "~/Documents/org/todo.org"))

(global-set-key [(f12)] #'open-todos)

(defun open-candidates ()
  (interactive)
  (find-file "~/Documents/hire.org/notes/candidates.org"))

(global-set-key [(shift f12)] #'open-candidates)

;;; Capture task --------------------------------------------------------------
;;; See http://orgmode.org/manual/Capture-templates.html#Capture-templates

(setq org-default-notes-file "/Users/pgrenet/Documents/org/todo.org")

(setq org-capture-templates
      '(("t" "Today" entry
         (file+headline org-default-notes-file "Today")
         "*** TODO %?"
         :kill-buffer)
        ("w" "Week" entry
         (file+headline org-default-notes-file "Week")
         "*** TODO %?"
         :kill-buffer)
        ("n" "Next" entry
         (file+headline org-default-notes-file "Next")
         "*** TODO %?"
         :kill-buffer)
        ("m" "Team meeting" entry
         (file+headline org-default-notes-file "Team meeting")
         "*** %?"
         :kill-buffer)))

(define-key global-map [(ctrl f12)] #'org-capture)

;;; Move task -----------------------------------------------------------------

(defun exordium-org-move-to-today ()
  "Move the current subtree to the end of Tasks/Today"
  (interactive)
  (org-cut-subtree)
  (beginning-of-buffer)
  (org-forward-heading-same-level 1)
  (outline-next-visible-heading 1)
  (org-forward-heading-same-level 1)
  (backward-char 1)
  (org-paste-subtree))

(define-key org-mode-map [(ctrl c) (t)] #'exordium-org-move-to-today)

;;; Org agenda ----------------------------------------------------------------

(setq org-agenda-files '("/Users/pgrenet/Documents/org/"))
(setq org-agenda-custom-commands
      '(("c" "Philippe's agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority items:")))
          (agenda "")
          (alltodo "")))))
(add-to-list 'org-emphasis-alist '("*" (:foreground "#de935f")))
