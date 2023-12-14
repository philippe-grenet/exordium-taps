;;;; Local extensions to Exordium: Org mode
;;; Code:

(require 'org)

(setq org-hide-leading-stars t)
(setq org-fontify-quote-and-verse-blocks t)

;;; Remove the hook added by init-org-mode.el
(remove-hook 'org-mode-hook 'turn-on-visual-line-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 100)))


;;; Keys

(define-key org-mode-map [(super down)] #'org-forward-heading-same-level)
(define-key org-mode-map [(super up)]   #'org-backward-heading-same-level)


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
   (set-face-attribute 'org-headline-done nil :foreground comment))

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
           ("BAD"       . (:background ,red :foreground ,background :weight bold :box nil))
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

;; Images
;; Use:  #+attr_html: :width 800px
(setq org-image-actual-width nil)



;;; svg-tag-mode: https://github.com/rougier/svg-tag-mode/

(require 'svg-tag-mode)

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(setq svg-tag-tags
      `(
        ;; Plain TODO statuses
        ("\\(TODO\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-warning-face :inverse t))))
        ("\\(DONE\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-string-face :inverse t))))
        ("\\(WORK\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-type-face :inverse t))))
        ("\\(STOP\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-comment-face :inverse t))))
        ("\\(READY\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-function-name-face :inverse t))))
        ("\\(WAIT\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-function-name-face :inverse t))))
        ;; Rectangles with plain words: [Something] or :Something:
        ;; ("\\(\\[[A-Za-z]+\\]\\)" . ((lambda (tag)
        ;;                               (svg-tag-make tag
        ;;                                             :face 'font-lock-type-face
        ;;                                             :beg 1 :end -1 :inverse nil))))
        ("\\(:[A-Za-z]+:\\)" . ((lambda (tag)
                                  (svg-tag-make tag
                                                :face 'font-lock-comment-face
                                                :beg 1 :end -1 :inverse nil))))
        ;;
        ;; Pills with 1 letter or one or 2 numbers: (A) (10)
        ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                (svg-tag-make tag :beg 1 :end -1 :radius 12))))
        ("\([0-9][0-9]\)" . ((lambda (tag)
                               (svg-tag-make tag :beg 1 :end -1 :radius 8))))
        ;;
        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
        ;; Inactive date  (with or without day name, with or without time)
        (,(format "\\(\\[%s\\]\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
        (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
        (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
        ;;
        ;; Progress: [1/3] or [42%]
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))))

(add-hook 'org-mode-hook 'svg-tag-mode)



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
                            (,(colorize-note-extension "status.org")   . "~/Documents/org/status.org")
                            (,(colorize-note-extension "roadmap.org") . "~/Documents/org/roadmap.org")))

(defconst notes-directories '("~/Documents/org/notes/"
                              "~/Documents/org/arr/"
                              "~/Documents/org/spark-platform/"
                              "~/Documents/org/bql/"
                              "~/Documents/org/bqnt/"
                              "~/Documents/org/equity/"
                              "~/Documents/org/other/"
                              "~/Documents/org/tech/"))

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
  "Return the full alist of notes (file-name . path)."
  (append top-level-notes
          (mapcan #'list-notes-in-directory notes-directories)))

(defun open-todos (file)
  "Open a note as FILE from the list of active notes in Documents/org."
  (interactive
   (list
    (completing-read "Open: " (list-all-notes))))
  (find-file (cdr (assoc file (list-all-notes)))))

(global-set-key [(f12)] #'open-todos)

;; Quick access

(defun open-todo-file ()
  "Open my todo.org file."
  (interactive)
  (find-file "~/Documents/org/todo.org"))

(global-set-key [(shift f12)] #'open-todo-file)

(defun open-catchup-file ()
  "Open my catch up file."
  (interactive)
  (find-file "~/Documents/org/catchup.org"))

(global-set-key [(control f12)] #'open-catchup-file)


;;; Capture task
;;; See http://orgmode.org/manual/Capture-templates.html#Capture-templates

;;(setq org-src-window-setup 'slit-window-right) ; does not work

(setq org-default-notes-file "/Users/pgrenet/Documents/org/todo.org")

(setq org-capture-templates
      '(("i" "📥 Inbox" entry
         (file+headline "~/Documents/org/todo.org" "📥 Inbox")
         "** TODO %?\n  %i\n"
         :empty-lines-after 1)
        ("S" "Shabbir" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Shabbir")
         "** TODO Shabbir %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("A" "Anthony" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Anthony")
         "** TODO AC %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("k" "Abhishek" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Abhishek")
         "** TODO AG %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("s" "Sathya" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Sathya")
         "** TODO Sathya %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("c" "Conway" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Conway")
         "** TODO Conway %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("t" "Tom" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Tom")
         "** TODO Tom %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("a" "Amey" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Amey")
         "** TODO Amey %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("m" "Mike" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Mike")
         "** TODO Mike %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("g" "Gino" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Gino")
         "** TODO Gino %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("p" "Prash" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Pranil")
         "** TODO Pranil %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)))

(define-key global-map [(meta f12)] #'org-capture)
(define-key global-map [(f13)] #'org-capture)

;;; Move task

(defun exordium-org-move-to-today ()
  "Move the current subtree to the end of Tasks/Today."
  (interactive)
  (org-cut-subtree)
  (beginning-of-buffer)
  (org-forward-heading-same-level 1)
  (outline-next-visible-heading 1)
  (org-forward-heading-same-level 1)
  (backward-char 1)
  (org-paste-subtree))

(define-key org-mode-map [(ctrl c) (t)] #'exordium-org-move-to-today)


;;; Archive
(setq org-archive-location "%s_archive::datetree/")


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
  "Open the calendar view."
  (interactive)
  (split-window-vertically)
  ;; Fix the bug where it takes a little too much width
  (let ((w (frame-width (selected-frame))))
    (set-frame-width (selected-frame) (- w 4))
    (cfw:open-org-calendar)
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


;; Sync todos to Google Drive for PlainOrg

(defun org-sync ()
  "Copy todos and catch up notes to Google Drive."
  (interactive)
  (copy-file "~/Documents/org/todo.org"
             "/Users/pgrenet/Library/CloudStorage/GoogleDrive-pgrenet@bloomberg.net/My Drive/org/todo.org" t)
  (copy-file "~/Documents/org/catchup.org"
             "/Users/pgrenet/Library/CloudStorage/GoogleDrive-pgrenet@bloomberg.net/My Drive/org/catchup.org" t)
  (message "todo.org and catchup.org synced"))

;;; after-init.el ends here
