;;;; Local extensions to Exordium: Org mode

(require 'org)

;;; Look ----------------------------------------------------------------------

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

(setq org-ellipsis "⤵")  ;; or "…"

;;; Task list -----------------------------------------------------------------

(require 'cl-lib)

(defconst top-level-notes '(("todo"     . "~/Documents/org/todo.org")
                            ("catchup"  . "~/Documents/org/catchup.org")
                            ("meetings" . "~/Documents/org/meetings.md")))

(defconst notes-directories '("~/Documents/org/pace/"
                              "~/Documents/org/bql/"
                              "~/Documents/org/equity/"
                              "~/Documents/org/other/"
                              "~/Documents/org/hire"))

(defun list-notes-in-directory (dir)
  ;; Return a alist of (file-name . path) for all org and markdown files in 'dir'.
  ;; file-name includes the last sub-directory.
  ;; The list is sorted by file-name ascending.
  (cl-flet ((note-name-and-path (file)
              (cons (concat (car (last (delete "" (split-string dir "/"))))
                            "/"
                            (file-name-sans-extension (file-name-nondirectory file)))
                    file)))
    (sort (append (mapcar #'note-name-and-path
                          (directory-files dir :match-regexp "^.*\.org"))
                  (mapcar #'note-name-and-path
                          (directory-files dir :match-regexp "^.*\.md")))
          #'(lambda (a b)
              (string< (downcase (car a)) (downcase (car b)))))))

(defun list-all-notes ()
  ;; Return the full alist of notes (file-name . path)
  (append top-level-notes
          (mapcan #'list-notes-in-directory notes-directories)))

(defun open-todos (file)
  "Open a note from the list of active notes in Documents/org"
   (interactive
   (list
    (completing-read "Open: " (list-all-notes))))
  (find-file (cdr (assoc file (list-all-notes)))))

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
