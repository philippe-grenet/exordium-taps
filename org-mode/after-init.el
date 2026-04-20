;;;; Package --- summary : Local extensions to Exordium: Org Mode
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-element)

(setq org-hide-leading-stars t)
(setq org-fontify-quote-and-verse-blocks t)

;;; Remove the hook added by init-org-mode.el
(remove-hook 'org-mode-hook 'turn-on-visual-line-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 100)))

;; Enable 'eval' in a file's local variables. This prevents Emacs for asking
;; for confirmation each time such as file is loaded.
(setq org-confirm-babel-evaluate nil)


;;; Keys

;; Super + Arrow: navigate through the tree (same level and up heading)
(define-key org-mode-map [(super down)] #'org-forward-heading-same-level)
(define-key org-mode-map [(super up)]   #'org-backward-heading-same-level)
(define-key org-mode-map [(super left)] #'outline-up-heading)

;; C-c o d: Mark a task as DONE
(defun my-org-todo-toggle ()
  "Mark the task as DONE if it wasn't, mark it as TODO otherwise."
  (interactive)
  (let ((state (org-get-todo-state))
        post-command-hook)
    (if (not (string= state "DONE"))
        (org-todo "DONE")
      (org-todo "TODO"))
    (run-hooks 'post-command-hook)
    (org-fold-subtree t)))

(define-key org-mode-map (kbd "C-c o d") 'my-org-todo-toggle)

;; C-c o l: Paste link a Jira epic (assuming the URL is in hte clipboard)
(defun my-org-paste-epic-link ()
  "Paste a Jira epic link in short form."
  (interactive)
  (let* ((url      (current-kill 0 t))
         (filename (url-filename (url-generic-parse-url url))))
    (insert "[[" url "][")
    (insert (substring filename (+ 1 (string-match "-" filename))))
    (insert "]]")))

(define-key org-mode-map (kbd "C-c o l") 'my-org-paste-epic-link)

;; C-c o d/w/b: refile to Today, Week or Backlog.
;; Move task to the beginning of the section.
;; With argument C-u, move task to the end of the section.
(defun my-org-refile-to-today (arg)
  "Refile current headline to specific file and heading."
  (interactive "P")
  (let* ((target-file "~/Documents/org/todo.org")
         (target-headline "☕️ Today")
         ;; Find the position of the headline in the target file
         (pos (save-excursion
                (find-file target-file)
                (org-find-exact-headline-in-buffer target-headline)))
         ;; Move to the beginning by default. Prefix with C-u to add at the end
         (org-reverse-note-order (not arg)))
    ;; org-refile arguments:
    ;; 1. prefix-arg: nil
    ;; 2. default-buffer: nil
    ;; 3. rfloc: (headline-name target-file nil pos)
    (org-refile nil nil (list target-headline target-file nil pos))))

(defun my-org-refile-to-week (arg)
  "Refile current headline to specific file and heading."
  (interactive "P")
  (let* ((target-file "~/Documents/org/todo.org")
         (target-headline "Week")
         ;; Find the position of the headline in the target file
         (pos (save-excursion
                (find-file target-file)
                (org-find-exact-headline-in-buffer target-headline)))
         ;; Move to the beginning by default. Prefix with C-u to add at the end
         (org-reverse-note-order (not arg)))
    ;; org-refile arguments:
    ;; 1. prefix-arg: nil
    ;; 2. default-buffer: nil
    ;; 3. rfloc: (headline-name target-file nil pos)
    (org-refile nil nil (list target-headline target-file nil pos))))

(defun my-org-refile-to-backlog (arg)
  "Refile current headline to specific file and heading."
  (interactive "P")
  (let* ((target-file "~/Documents/org/todo.org")
         (target-headline "Backlog")
         ;; Find the position of the headline in the target file
         (pos (save-excursion
                (find-file target-file)
                (org-find-exact-headline-in-buffer target-headline)))
         ;; Move to the beginning by default. Prefix with C-u to add at the end
         (org-reverse-note-order (not arg)))
    ;; org-refile arguments:
    ;; 1. prefix-arg: nil
    ;; 2. default-buffer: nil
    ;; 3. rfloc: (headline-name target-file nil pos)
    (org-refile nil nil (list target-headline target-file nil pos))))

(define-key org-mode-map (kbd "C-c o t") #'my-org-refile-to-today)
(define-key org-mode-map (kbd "C-c o w") #'my-org-refile-to-week)
(define-key org-mode-map (kbd "C-c o b") #'my-org-refile-to-backlog)

;;; C-c o i: insert image
(defun my-org-insert-image ()
  (interactive)
  (insert "#+attr_html: :width 900px\n")
  (insert "[[./img/.png]]\n")
  (backward-char 7))

(define-key org-mode-map (kbd "C-c o i") #'my-org-insert-image)

;;; C-c o m: insert Mermaid class diagram
(defun my-org-insert-mermaid-diagram ()
  (interactive)
  (insert "#+attr_org: :width 900\n")
  (insert "#+begin_src mermaid :file diagrams/example.png :theme dark :background-color transparent :width 1800\n")
  (insert "classDiagram\n")
  (insert "    note \"see https://mermaid.js.org/syntax/classDiagram.html\"\n")
  (insert "#+end_src\n")
  (backward-char 73))

(define-key org-mode-map (kbd "C-c o m") #'my-org-insert-mermaid-diagram)

;;; C-c o o: Open image link at point in macOS native app
;;; Note that the other way is C-c C-o which opens the image in a new buffer.
(defun my-org-open-image-externally ()
  "Open the image link at point using macOS's default application."
  (interactive)
  (let ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (let* ((path (org-element-property :path context))
             (full-path (expand-file-name path (file-name-directory (buffer-file-name)))))
        (if (file-exists-p full-path)
            (start-process "open-image" nil "open" full-path)
          (user-error "File not found: %s" full-path))))))

(define-key org-mode-map (kbd "C-c o o") #'my-org-open-image-externally)

;; C-c o x: Tag old entries with :ARCHIVE:
(defun my-org-mark-old-entries (months)
  "Tag org headings older than MONTHS months with :ARCHIVE:.
Searches for headings containing a [YYYY-MM-DD] date and adds the
ARCHIVE tag if the entry is older than MONTHS months from today.
Archived entries are dimmed and excluded from agenda views.
Use `org-archive-subtree' (C-c C-x C-a) to move them to the archive file.
Use C-u C-c C-x C-s (`org-archive-subtree-default' with C-u for batch archiving)
to move them all to the archive file in one shot."
  (interactive "nMark entries older than how many months? ")
  (let* ((cutoff (time-subtract (current-time)
                                (days-to-time (* months 30))))
         (count 0)
         (lines 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^\\*+\\s-+.*\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\]"
              nil t)
        (let* ((date-str (match-string 1))
               (entry-time (encode-time (parse-time-string
                                         (concat date-str " 00:00:00")))))
          (when (and (time-less-p entry-time cutoff)
                     (not (member "ARCHIVE" (org-get-tags nil t))))
            (let ((beg (line-beginning-position)))
              (save-excursion
                (org-end-of-subtree t)
                (setq lines (+ lines (count-lines beg (point))))))
            (org-toggle-archive-tag)
            (setq count (1+ count))))))
    (message "Tagged %d entries (%d lines) older than %d months with :ARCHIVE:"
             count lines months)))

(define-key org-mode-map (kbd "C-c o x") #'my-org-mark-old-entries)


;;; Look

;; (when (eq exordium-theme 'tomorrow-night)
;;   ;; (setq org-priority-faces
;;   ;;       '((?A :foreground "#1d1f21" :background "#cc6666" :weight bold)
;;   ;;         (?B :foreground "#1d1f21" :background "#de935f" :weight bold)
;;   ;;         (?C :foreground "#1d1f21" :background "#b5bd68" :weight bold)))

;;   (setq org-cycle-separator-lines -1)

;;   (with-tomorrow-colors
;;    (tomorrow-mode-name)
;;    (setq org-emphasis-alist
;;          `(("*" (:foreground ,red))                         ; ("*" bold)
;;            ("/" (:foreground ,green))                       ; ("/" italic)
;;            ("_" (:background ,red :foreground ,background)) ; ("_" underline)
;;            ("=" org-verbatim verbatim)
;;            ("~" org-code verbatim)
;;            ("+" (:strike-through t)))))

;;   (with-tomorrow-colors
;;    (tomorrow-mode-name)
;;    (set-face-attribute 'org-headline-done nil :foreground comment))

;;   (with-tomorrow-colors
;;    (tomorrow-mode-name)
;;    (setq org-todo-keyword-faces
;;          `(("TODO"      . (:foreground ,red :weight bold :box nil))
;;            ("DONE"      . (:foreground ,green :weight bold :box nil))
;;            ("WORK"      . (:foreground ,yellow :weight bold :box nil))
;;            ("WAIT"      . (:foreground ,orange :weight bold :box nil))
;;            ("STOP"      . (:foreground ,comment :weight bold :box nil))

;;            ;; for catch up:
;;            ("NEXT"      . (:background ,red :foreground ,background :weight bold :box nil))

;;            ;; for BDGT:
;;            ("SUBMITTED" . (:foreground ,orange :weight bold :box nil))
;;            ("APPROVED"  . (:foreground ,yellow :weight bold :box nil))
;;            ("PARTIAL"   . (:background ,yellow :foreground ,background :weight bold :box nil))
;;            ("COMPLETE"  . (:background ,green :foreground ,background :weight bold :box nil))
;;            ("CANCELED"  . (:background ,comment :foreground ,background :weight bold :box nil))

;;            ;; for hire:
;;            ("BAD"       . (:background ,red :foreground ,background :weight bold :box nil))
;;            ("MEDIUM"    . (:background ,orange :foreground ,background :weight bold :box nil))
;;            ("GOOD"      . (:background ,green :foreground ,background :weight bold :box nil))
;;            ("REJECTED"  . (:background ,red :foreground ,background :weight bold :box nil))
;;            ("WITHDREW"  . (:background ,purple :foreground ,background :weight bold :box nil))
;;            ("HIRED"     . (:background ,green :foreground ,background :weight bold :box nil))))))

;;(require 'init-themes)
(when (member exordium-theme '(tomorrow-night))
  (require 'color-theme-tomorrow)
  (setq org-priority-faces
        '((?A :foreground "#1d1f21" :background "#cc6666" :weight bold)
         (?B :foreground "#1d1f21" :background "#de935f" :weight bold)
         (?C :foreground "#1d1f21" :background "#b5bd68" :weight bold)))
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
   (set-face-attribute 'org-headline-done nil :foreground comment)))

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
        ("\\(WAIT\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-function-name-face :inverse t))))
        ("\\(BLOCKED\\)" . ((lambda (tag)
                             (svg-tag-make tag :face 'dired-flagged :inverse t))))
        ("\\(READY\\)" . ((lambda (tag)
                           (svg-tag-make tag :face 'font-lock-function-name-face :inverse t))))
        ("\\(REVIEW\\)" . ((lambda (tag)
                            (svg-tag-make tag :face 'font-lock-variable-name-face :inverse t))))
        ("\\(QUESTIONED\\)" . ((lambda (tag)
                               (svg-tag-make tag :face 'font-lock-comment-face :inverse t))))
        ("\\(POSTPONED\\)" . ((lambda (tag)
                               (svg-tag-make tag :face 'font-lock-comment-face :inverse t))))
        ("\\(PROCEED\\)" . ((lambda (tag)
                              (svg-tag-make tag :face 'font-lock-string-face :inverse t))))
        ("\\(REJECT\\)" . ((lambda (tag)
                             (svg-tag-make tag :face 'font-lock-warning-face :inverse t))))
        ;; Priorities
        ("\\(\\[#A\\]\\)" . ((lambda (tag)
                             (svg-tag-make tag
                                           :face 'font-lock-warning-face :inverse t
                                           :beg 1 :end -1))))
        ("\\(\\[#B\\]\\)" . ((lambda (tag)
                             (svg-tag-make tag
                                           :face 'font-lock-type-face :inverse t
                                           :beg 1 :end -1))))
        ("\\(\\[#C\\]\\)" . ((lambda (tag)
                             (svg-tag-make tag
                                           :face 'font-function-name-face :inverse t
                                           :beg 1 :end -1))))
        ;; Rectangles with plain words: {:Something:}
        ;; Consider expending to "\\({:[A-Za-z0-9]+\\(?:[ ][A-Za-z0-9]+\\)*:}\\)"
        ("\\({:[A-Za-z]+:}\\)" . ((lambda (tag)
                                   (svg-tag-make tag
                                                 :face 'font-lock-type-face
                                                 :beg 2 :end -2 :inverse nil))))
        ;; Rectangles with plain words: {{Something}}
        ("\\({{[A-Za-z]+}}\\)" . ((lambda (tag)
                                   (svg-tag-make tag
                                                 :face 'font-lock-comment-face
                                                 :beg 1 :end -1 :inverse nil))))
        ;; Pills with 1 letter or one or 2 numbers: ((A)) ((10))
        ("\(\([0-9a-zA-Z]\)\)" . ((lambda (tag)
                                   (svg-tag-make tag :beg 1 :end -1 :radius 12))))
        ("\(\([0-9][0-9]\)\)" . ((lambda (tag)
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

(defun colorize-note-extension (file-name)
  (let ((file-ext (file-name-extension file-name t)))
    (concat (file-name-base file-name)
            (propertize file-ext 'face (if (string= file-ext ".org")
                                           'helm-ff-truename
                                         'helm-ff-file-extension)))))


(defconst top-level-notes `((,(colorize-note-extension "todo.org")    . "~/Documents/org/todo.org")
                            (,(colorize-note-extension "catchup.org") . "~/Documents/org/catchup.org")
                            (,(colorize-note-extension "datasets.org")   . "~/Documents/org/datasets.org")
                            (,(colorize-note-extension "roadmap.org") . "~/Documents/org/roadmap.org")
                            (,(colorize-note-extension "requirements.org") . "~/Documents/org/requirements.org")))

(defconst notes-directories '("~/Documents/org/bql/"
                              "~/Documents/org/bql/bqe/"
                              "~/Documents/org/bql/bqnt/"
                              "~/Documents/org/bql/data-tier/"
                              "~/Documents/org/bql/dfl/"
                              "~/Documents/org/bql/engine/"
                              "~/Documents/org/bql/equity/"
                              "~/Documents/org/bql/gateway/"
                              "~/Documents/org/bql/language/"
                              "~/Documents/org/bql/metadata/"
                              "~/Documents/org/bql/onboarding/"
                              "~/Documents/org/ap/"
                              "~/Documents/org/architecture/"
                              "~/Documents/org/ai/"
                              "~/Documents/org/notes/"
                              "~/Documents/org/planning/"
                              "~/Documents/org/management/"
                              "~/Documents/org/tech/bloomberg/"
                              "~/Documents/org/tech/general/"))

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
      '(("i" "📥\tInbox" entry
         (file+headline "~/Documents/org/todo.org" "📥 Inbox")
         "** TODO %?\n  %i\n"
         :empty-lines-after 1)
        ("T" "☕️\tToday" entry
         (file+headline "~/Documents/org/todo.org" "☕️ Today")
         "** TODO %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("S" "\tShabbir" entry
         (file+headline "~/Documents/org/catchup.org" "✨ Shabbir")
         "** TODO Shabbir %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("A" "\tAnthony" entry
         (file+headline "~/Documents/org/catchup.org"
                        "✨ Anthony ([[https://docs.google.com/document/d/11epTeSzu5tgxVOf19rn88C41d6JvLyKN4YTpERd6ZW0/edit?tab=t.0][topics]])")
         "** TODO AC %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("M" "\tManish" entry
         (file+headline "~/Documents/org/catchup.org"
                        "✨ Manish ([[https://docs.google.com/document/d/1ObXxx36mXhHcKsnYiKc5jMAfDyDeFzIqQdR2dS7sI9E/edit?tab=t.0#heading=h.px0s0g92opc8][topics]])")
         "** TODO Manish %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("s" "\tSathya" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Sathya")
         "** TODO Sathya %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("t" "\tTom" entry
         (file+headline "~/Documents/org/catchup.org"
                        "⭐️ Tom ([[https://docs.google.com/document/d/102GWuRqH-sYFMNw9DPKu9npDsGoZXNFW0-mKDWbuRJk/edit#heading=h.2cjiyf4l29p1][topics]])")
         "** TODO Tom %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("g" "\tGino" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Gino")
         "** TODO Gino %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("y" "\tYogesh" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Yogesh")
         "** TODO Yogesh %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("r" "\tRishi" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Rishi")
         "** TODO Rishi %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("p" "\tPranil" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Pranil")
         "** TODO Pranil %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("j" "\tJas" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Jas")
         "** TODO Jas %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("m" "\tMike" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Mike")
         "** TODO Mike %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("a" "\tAbhishek, Amey, Alicija, Alex")
        ("ag" "\tAbhishek" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Abhishek")
         "** TODO AG %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("am" "\tAmey" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Amey")
         "** TODO Amey %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("al" "\tAlicija" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Alicija ([[https://docs.google.com/document/d/13rZFsJHeT8UjChTZMPnRT65Oq1pKILwocxjcma5mkoE/edit?tab=t.3eojide0clcg#heading=h.p6lx1dt4yzzc][topics]])")
         "** TODO Alicija %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("as" "\tAlex" entry
         (file+headline "~/Documents/org/catchup.org" "⭐️ Alex")
         "** TODO Alex %?\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ))

(define-key global-map [(meta f12)] #'org-capture)
(define-key global-map [(f13)] #'org-capture)
(define-key global-map [(delete)] #'org-capture)


;;; File location

;; Link abbreviations.
;; For example, this allows for absolute links like [[repo:docs/guide.org][Guide]]
(setq org-link-abbrev-alist
      '(("org" . "~/Documents/org/")))

;; Archive
(setq org-archive-location "%s_archive::datetree/")

(setf (alist-get 'file org-link-frame-setup) #'find-file)


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

;; (when (eq exordium-theme 'tomorrow-night)
;;   (with-tomorrow-colors 'night
;;     (custom-set-faces
;;      `(cfw:face-title ((t (:foreground ,green :weight bold :height 2.0))))
;;      `(cfw:face-header ((t (:foreground ,yellow :weight bold))))
;;      `(cfw:face-sunday ((t :foreground ,orange :background ,background :weight bold)))
;;      `(cfw:face-saturday ((t :foreground ,orange :background ,background :weight bold)))
;;      `(cfw:face-holiday ((t :background ,orange :foreground ,background :weight bold)))
;;      `(cfw:face-grid ((t :foreground ,selection)))
;;      `(cfw:face-default-content ((t :foreground ,purple)))
;;      `(cfw:face-periods ((t :foreground "cyan")))
;;      `(cfw:face-day-title ((t :background "grey10")))
;;      `(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
;;      `(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
;;      `(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
;;      `(cfw:face-today-title ((t :foreground ,background :background ,green :weight bold)))
;;      `(cfw:face-today ((t :background: ,green :weight bold)))
;;      `(cfw:face-select ((t :background "#2f2f2f")))
;;      `(cfw:face-toolbar ((t :foreground ,foreground :background ,selection)))
;;      `(cfw:face-toolbar-button-off ((t :foreground ,aqua :background ,selection :weight bold)))
;;      `(cfw:face-toolbar-button-on ((t :foreground ,foreground :background ,selection :weight bold))))))

(when (member exordium-theme '(catppuccin-mocha))
  (require 'color-theme-catppuccin)
  (with-catppuccin-colors
   exordium-catppuccin-flavor
   (set-face-attribute 'cfw:face-title nil :foreground blue)
   (set-face-attribute 'cfw:face-today nil :background lavender)
   (set-face-attribute 'cfw:face-today-title nil :background blue)
   (set-face-attribute 'cfw:face-annotation nil :foreground red)
   (set-face-attribute 'cfw:face-toolbar-button-off nil :foreground green)))


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


;; Sync todos to Google Drive for PlainOrg

(defun org-sync ()
  "Copy todos and catch up notes to Google Drive."
  (interactive)
  (copy-file "~/Documents/org/todo.org"
             "/Users/pgrenet/Library/CloudStorage/GoogleDrive-pgrenet@bloomberg.net/My Drive/org/todo.org" t)
  (copy-file "~/Documents/org/catchup.org"
             "/Users/pgrenet/Library/CloudStorage/GoogleDrive-pgrenet@bloomberg.net/My Drive/org/catchup.org" t)
  (message "todo.org and catchup.org synced"))

;; Schedule the function every hour. See also run-with-timer and cancel-timer.
(run-at-time "00:00" 3600 'org-sync)


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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
