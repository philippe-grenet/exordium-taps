;;;; Local extensions to Exordium: Org mode

(require 'org)

(setq org-hide-leading-stars t)

;;; Remove the hook added by init-org-mode.el
(remove-hook 'org-mode-hook 'turn-on-visual-line-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 100)))


;;; Look

(when (eq exordium-theme 'tomorrow-night)
  (setq org-priority-faces
        '((?A :foreground "#1d1f21" :background "#cc6666" :weight bold)
          (?B :foreground "#1d1f21" :background "#de935f" :weight bold)
          (?C :foreground "#1d1f21" :background "#b5bd68" :weight bold)))

  (setq org-cycle-separator-lines -1)

  (with-tomorrow-colors
   (tomorrow-mode-name)
   (setq org-emphasis-alist
         `(("*" (:foreground ,red))                         ; ("*" bold)
           ("/" (:foreground ,green))                       ; ("/" italic)
           ("_" (:background ,red :foreground ,background)) ; ("_" underline)
           ("=" org-verbatim verbatim)
           ("~" org-code verbatim)
           ("+" (:strike-through t)))))

  (with-tomorrow-colors
   (tomorrow-mode-name)
   (setq org-todo-keyword-faces
         `(("TODO"      . (:foreground ,red :weight bold :box nil))
           ("DONE"      . (:foreground ,green :weight bold :box nil))
           ("WORK"      . (:foreground ,yellow :weight bold :box nil))
           ("WAIT"      . (:foreground ,orange :weight bold :box nil))
           ("STOP"      . (:foreground ,comment :weight bold :box nil))

           ;; for catch up:
           ("NEXT"      . (:background ,red :foreground ,background :weight bold :box nil))

           ;; for BDGT:
           ("SUBMITTED" . (:foreground ,orange :weight bold :box nil))
           ("APPROVED"  . (:foreground ,yellow :weight bold :box nil))
           ("PARTIAL"   . (:background ,yellow :foreground ,background :weight bold :box nil))
           ("COMPLETE"  . (:background ,green :foreground ,background :weight bold :box nil))
           ("CANCELED"  . (:background ,comment :foreground ,background :weight bold :box nil))

           ;; for hire:
           ("HOLD"      . (:background ,comment :foreground ,background :weight bold :box nil))
           ("SCHEDULED" . (:foreground ,blue :weight bold :box nil))
           ("MEDIUM"    . (:background ,orange :foreground ,background :weight bold :box nil))
           ("GOOD"      . (:background ,green :foreground ,background :weight bold :box nil))
           ("REJECTED"  . (:background ,red :foreground ,background :weight bold :box nil))
           ("WITHDREW"  . (:background ,purple :foreground ,background :weight bold :box nil))
           ("HIRED"     . (:background ,aqua :foreground ,background :weight bold :box nil))))))

(setq org-ellipsis "⤵")  ;; or "…"

;; org-fontify-todo-headline -> TODO => face org-headline-todo
;;(setq org-not-done-regexp "STOP")
;;(setq org-fontify-todo-headline t)

;; Spell check
(add-hook 'org-mode-hook 'flyspell-prog-mode)


;;; Task list

(require 'cl-lib)

(defun colorize-note-extension (file-name)
  (let ((file-ext (file-name-extension file-name t)))
    (concat (file-name-base file-name)
            (propertize file-ext 'face (if (string= file-ext ".org")
                                           'helm-ff-truename
                                         'helm-ff-file-extension)))))


(defconst top-level-notes `((,(colorize-note-extension "todo.org")    . "~/Documents/org/todo.org")
                            (,(colorize-note-extension "catchup.org") . "~/Documents/org/catchup.org")
                            (,(colorize-note-extension "roadmap.org") . "~/Documents/org/roadmap.org")
                            (,(colorize-note-extension "meetings.md") . "~/Documents/org/meetings.md")))

(defconst notes-directories '("~/Documents/org/parquet/"
                              "~/Documents/org/spark-platform/"
                              "~/Documents/org/bql/"
                              "~/Documents/org/equity/"
                              "~/Documents/org/other/"
                              "~/Documents/org/tech/"
                              "~/Documents/org/hire"))

(defun list-notes-in-directory (dir)
  ;; Return a alist of (file-name . path) for all org and markdown files in 'dir'.
  ;; file-name includes the last sub-directory.
  ;; The list is sorted by file-name ascending.
  (cl-flet ((note-name-and-path (file)
              (let* ((file-name (file-name-base file))
                     (file-ext  (file-name-extension file t)))
                (cons (concat (propertize (car (last (delete "" (split-string dir "/"))))
                                          'face 'helm-ff-directory)
                              "/" file-name
                              (propertize file-ext 'face (if (string= file-ext ".org")
                                                             'helm-ff-truename
                                                           'helm-ff-file-extension)))
                      file))))
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

;; Quick access

(defun open-todo-file ()
  (interactive)
  (find-file "~/Documents/org/todo.org"))

(global-set-key [(meta f12)] #'open-todo-file)

(defun open-catchup-file ()
  (interactive)
  (find-file "~/Documents/org/catchup.org"))

(global-set-key [(shift meta f12)] #'open-catchup-file)


;;; Capture task
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

;;; Move task

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


;;; Org agenda

(setq org-agenda-files '("/Users/pgrenet/Documents/org/"))
(setq org-agenda-custom-commands
      '(("c" "Philippe's agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority items:")))
          (agenda "")
          (alltodo "")))))


;;; Calfw mode
;;; https://github.com/kiwanami/emacs-calfw

(require 'calfw)
(require 'calfw-org)
;;(defalias 'open-calendar 'cfw:open-org-calendar)

(defun open-todos-calendar-view ()
  (interactive)
  (split-window-vertically)
  ;; Fix the bug where it takes a little too much width
  (let ((w (frame-width (selected-frame))))
    (set-frame-width (selected-frame) (- w 4))
    (cfw:open-org-calendar)
    (set-frame-width (selected-frame) w)))

(defun close-todos-calendar-view ()
  (interactive)
  (kill-this-buffer)
  (delete-other-windows))

(with-eval-after-load 'org
  (bind-key [(f9)] #'open-todos-calendar-view org-mode-map))
(with-eval-after-load 'calfw
  (bind-key [(f9)] #'close-todos-calendar-view cfw:calendar-mode-map))

(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday

(require 'color-theme-tomorrow)
(with-tomorrow-colors 'night
 (custom-set-faces
  `(cfw:face-title ((t (:foreground ,green :weight bold :height 2.0))))
  `(cfw:face-header ((t (:foreground ,yellow :weight bold))))
  `(cfw:face-sunday ((t :foreground ,orange :background ,background :weight bold)))
  `(cfw:face-saturday ((t :foreground ,orange :background ,background :weight bold)))
  `(cfw:face-holiday ((t :background ,orange :foreground ,background :weight bold)))
  `(cfw:face-grid ((t :foreground ,selection)))
  `(cfw:face-default-content ((t :foreground ,purple)))
  `(cfw:face-periods ((t :foreground "cyan")))
  `(cfw:face-day-title ((t :background "grey10")))
  `(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
  `(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
  `(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
  `(cfw:face-today-title ((t :foreground ,background :background ,green :weight bold)))
  `(cfw:face-today ((t :background: ,green :weight bold)))
  `(cfw:face-select ((t :background "#2f2f2f")))
  `(cfw:face-toolbar ((t :foreground ,foreground :background ,selection)))
  `(cfw:face-toolbar-button-off ((t :foreground ,aqua :background ,selection :weight bold)))
  `(cfw:face-toolbar-button-on ((t :foreground ,foreground :background ,selection :weight bold)))))
