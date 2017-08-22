;;;; Local extensions to Exordium: Org mode

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

;;; Colors --------------------------------------------------------------------

(setq org-priority-faces
      '((?A :foreground "#1d1f21" :background "#cc6666" :weight bold)
        (?B :foreground "#1d1f21" :background "#de935f" :weight bold)
        (?C :foreground "#1d1f21" :background "#b5bd68" :weight bold)))

(setq org-cycle-separator-lines -1)

(with-tomorrow-colors
 (tomorrow-mode-name)
 (setq org-todo-keyword-faces
       `(("WORK" . (;;:background ,yellow
                    :foreground ,yellow :weight bold :box nil))
         ("WAIT" . (;;:background ,orange
                    :foreground ,orange :weight bold :box nil))
         ("GOOD" . (:background ,green :foreground ,background
                    :weight bold :box nil))
         ("MEDIUM" . (:background ,orange :foreground ,background
                      :weight bold :box nil))
         ("REJECTED" . (:background ,red :foreground ,background
                        :weight bold :box nil))
         ("WITHDREW" . (:background ,red :foreground ,background
                        :weight bold :box nil)))))

;;; TODO list -----------------------------------------------------------------

(defun exordium-org-move-to-today ()
  "Move the current item to the today's tree"
  (interactive)
  (org-mark-subtree)
  (kill-region (region-beginning) (region-end))
  (beginning-of-buffer)
  (forward-line 3)
  (org-forward-heading-same-level 1)
  (forward-line -1)
  (yank))

(define-key org-mode-map (kbd "C-c t") #'exordium-org-move-to-today)

(defun open-todos ()
  (interactive)
  (find-file "~/Documents/org/todo.org"))

(global-set-key [(f12)] #'open-todos)

(defun open-candidates ()
  (interactive)
  (find-file "~/Documents/hire.org/notes/candidates.org"))

(global-set-key [(shift f12)] #'open-candidates)

;;; Capture -------------------------------------------------------------------

(setq org-default-notes-file "/Users/pgrenet/Documents/org/todo.org")
(define-key global-map "\C-cc" 'org-capture)
