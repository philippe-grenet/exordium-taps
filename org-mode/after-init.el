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
         `(("TODO"      . (:foreground ,red
                           :weight bold :box nil))
           ("DONE"      . (:foreground ,green
                           :weight bold :box nil))
           ("WORK"      . (:foreground ,yellow
                           :weight bold :box nil))
           ("WAIT"      . (:foreground ,orange
                           :weight bold :box nil))
           ("STOP"      . (:foreground ,comment
                           :weight bold :box nil))

           ;; for project plan
           ("SOON"      . (:foreground ,red
                           :weight bold :box nil))
           ("LATER"     . (:foreground ,yellow
                           :weight bold :box nil))
           ("MAYBE"     . (:foreground ,comment
                           :weight bold :box nil))
           ("REVIEW"    . (:foreground ,orange
                           :weight bold :box nil))
           ("Q3"        . (:foreground ,red
                           :weight bold :box nil))
           ("Q4"        . (:foreground ,yellow
                           :weight bold :box nil))

           ;; for hire:
           ("HOLD"      . (:background ,comment :foreground ,background
                           :weight bold :box nil))
           ("SCHEDULED" . (:foreground ,blue
                           :weight bold :box nil))
           ("MEDIUM"    . (:background ,orange :foreground ,background
                           :weight bold :box nil))
           ("GOOD"      . (:background ,green :foreground ,background
                           :weight bold :box nil))
           ("REJECTED"  . (:background ,red :foreground ,background
                           :weight bold :box nil))
           ("WITHDREW"  . (:background ,purple :foreground ,background
                           :weight bold :box nil))
           ("HIRED"     . (:background ,aqua :foreground ,background
                           :weight bold :box nil))))))

;;; Task list -----------------------------------------------------------------

(define-key org-mode-map (kbd "C-c t") #'exordium-org-move-to-today)

(defconst available-todos '(("todo"       . "~/Documents/org/todo.org")
                            ("catchup"    . "~/Documents/org/catchup.org")
                            ("meetings"   . "~/Documents/org/meetings.md")
                            ("roadmap"    . "~/Documents/org/pacehocs-roadmap.org")
                            ("candidates" . "~/Documents/hire.org/pace.org")
                            ("pmap"       . "~/Documents/org/pmap-spark.md")
                            ("bdgt"       . "~/Documents/org/bdgt.org")
                            ("tech-notes" . "~/Documents/org/tech-notes.md")))

(require 'cl-lib)
(cl-flet ((document-name-and-path (file)
            (cons (file-name-sans-extension (file-name-nondirectory file))
                  file)))
  (defconst available-notes (append (mapcar #'document-name-and-path
                                            (directory-files "~/Documents/org/notes/" :match-regexp "^.*\.md"))
                                    (mapcar #'document-name-and-path
                                            (directory-files "~/Documents/org/notes/" :match-regexp "^.*\.org")))))

(defconst all-todos (append available-todos
                            (sort available-notes #'(lambda (a b)
                                                      (string< (car a) (car b))))))

(defun open-todos (file)
  (interactive
   (list
    (completing-read "Open: " all-todos)))
  (find-file (cdr (assoc file all-todos))))

(global-set-key [(f12)] #'open-todos)

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
