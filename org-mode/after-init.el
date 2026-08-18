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

;; Super + Arrow: navigate through the tree (same level and up heading)
(define-key org-mode-map [(super down)] #'org-forward-heading-same-level)
(define-key org-mode-map [(super up)]   #'org-backward-heading-same-level)
(define-key org-mode-map [(super left)] #'outline-up-heading)
(define-key org-mode-map (kbd "C-c l") #'org-store-link) ; C-c C-l to org-insert-link

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

;; C-c o t/w/b: refile to Today, Week or Backlog.
;; Move task to the beginning of the section.
;; With argument C-u, move task to the end of the section.
(defun my-org-refile-to (target-headline arg)
  "Refile current headline to TARGET-HEADLINE in todo.org.
Prepend by default; with prefix ARG, append."
  (let* ((target-file "~/Documents/org/todo.org")
         (pos (save-excursion
                (find-file target-file)
                (org-find-exact-headline-in-buffer target-headline)))
         (org-reverse-note-order (not arg)))
    (org-refile nil nil (list target-headline target-file nil pos))))

(defun my-org-refile-to-today (arg)
  "Refile current headline to Today." (interactive "P")
  (my-org-refile-to "☕️ Today" arg))

(defun my-org-refile-to-week (arg)
  "Refile current headline to Week." (interactive "P")
  (my-org-refile-to "Week" arg))

(defun my-org-refile-to-backlog (arg)
  "Refile current headline to Backlog." (interactive "P")
  (my-org-refile-to "Backlog" arg))

(define-key org-mode-map (kbd "C-c o t") #'my-org-refile-to-today)
(define-key org-mode-map (kbd "C-c o w") #'my-org-refile-to-week)
(define-key org-mode-map (kbd "C-c o b") #'my-org-refile-to-backlog)

;;; C-c o i: insert image
(defun my-org-insert-image ()
  "Insert an image link with HTML width attribute."
  (interactive)
  (let ((file (read-file-name "Image: " nil nil nil "img/")))
    (insert (format "#+attr_html: :width 900px\n[[%s]]\n" file))))

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



;;; svg-tag-mode: https://github.com/rougier/svg-tag-mode/

(use-package svg-tag-mode
  :ensure t
  :config
  (progn
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
    ))


;;; Auto svg-tags for per-file TODO keywords
;;
;; Files may declare extra states via a "#+todo:" line (e.g. DROPPED, GOAL)
;; that have no rule in the global `svg-tag-tags'.  For any such keyword we
;; generate a buffer-local pill on the fly.  Colours are auto-cycled, but a
;; file can pin them explicitly with one or more directives:
;;
;;   #+svg_todo: DROPPED font-lock-comment-face
;;   #+svg_todo: GOAL    font-lock-warning-face

(defvar my/svg-todo-auto-palette
  '(font-lock-keyword-face font-lock-constant-face
    font-lock-preprocessor-face font-lock-doc-face)
  "Faces cycled through for auto-generated TODO svg tags.")

(defun my/svg-todo-covered-p (kw)
  "Non-nil if KW already matches an existing `svg-tag-tags' rule."
  (cl-some (lambda (entry) (string-match-p (car entry) kw)) svg-tag-tags))

(defun my/svg-todo-overrides ()
  "Return an alist (KEYWORD . FACE) parsed from #+svg_todo: directives."
  (let (alist)
    (dolist (val (cdr (assoc "SVG_TODO" (org-collect-keywords '("SVG_TODO")))))
      (pcase-let ((`(,kw ,face) (split-string (string-trim val) nil t)))
        (when (and kw face (facep (intern face)))
          (push (cons kw (intern face)) alist))))
    alist))

(defun my/svg-todo-auto-tags ()
  "Give any file-local TODO keyword without an svg rule a default pill.
Explicit colours from #+svg_todo: directives take precedence over the
auto-cycled palette; done-type keywords otherwise fall back to a dimmed face."
  (when (bound-and-true-p svg-tag-mode)
    (let ((overrides (my/svg-todo-overrides))
          (extra '())
          (i 0))
      (dolist (kw (and (boundp 'org-todo-keywords-1) org-todo-keywords-1))
        (unless (my/svg-todo-covered-p kw)
          (let ((face (or (cdr (assoc kw overrides))
                          (if (member kw org-done-keywords)
                              'font-lock-comment-face
                            (prog1 (nth (mod i (length my/svg-todo-auto-palette))
                                        my/svg-todo-auto-palette)
                              (setq i (1+ i)))))))
            (push (cons (format "\\(%s\\)" (regexp-quote kw))
                        (list (lambda (tag)
                                (svg-tag-make tag :face face :inverse t))))
                  extra))))
      (when extra
        ;; Buffer-local copy = new rules + global rules; global list untouched.
        (setq-local svg-tag-tags (append extra svg-tag-tags))
        (svg-tag-mode -1)
        (svg-tag-mode 1)))))

;; Append so it runs AFTER svg-tag-mode has been enabled by its own hook.
(add-hook 'org-mode-hook #'my/svg-todo-auto-tags t)


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
                            (,(colorize-note-extension "roadmap.org") . "~/Documents/org/roadmap.org")))

(defconst notes-root-directories
  '("~/Documents/org/bql/"
    "~/Documents/org/calcrt/"
    "~/Documents/org/planning/"
    "~/Documents/org/datasets/"
    "~/Documents/org/projects/"
    "~/Documents/org/architecture/"
    "~/Documents/org/notes/"
    "~/Documents/org/ap/"
    "~/Documents/org/ai/")
  "Root directories to scan for notes. Each root and its subdirectories, down
to two levels below the root, are included.")

(defun notes--subdirectories (dir depth)
  "Return DIR followed by its subdirectories down to DEPTH levels below DIR.
DEPTH of 0 returns just DIR, DEPTH of 1 returns DIR and its immediate
subdirectories, and so on. Dot-directories are skipped."
  (cons dir
        (when (> depth 0)
          (cl-mapcan (lambda (sub) (notes--subdirectories sub (1- depth)))
                     (cl-remove-if-not
                      #'file-directory-p
                      (directory-files dir t "^[^.]"))))))

(defun notes-directories ()
  "Return all notes directories: each root plus its subdirectories, down to
two levels below each root (three levels including the root)."
  (cl-remove-duplicates
   (cl-loop for root in notes-root-directories
            when (file-directory-p root)
            append (notes--subdirectories root 2))
   :test #'string=))

(defun notes--dir-label (dir)
  "Return DIR's path label starting from its matching notes root.
For example ~/Documents/org/bql/subA/subB/ yields \"bql/subA/subB\"."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (root (cl-find-if
                (lambda (r)
                  (string-prefix-p (file-name-as-directory (expand-file-name r)) dir))
                notes-root-directories)))
    (if root
        (let ((parent (file-name-directory
                       (directory-file-name (file-name-as-directory
                                             (expand-file-name root))))))
          (directory-file-name (file-relative-name dir parent)))
      (file-name-nondirectory (directory-file-name dir)))))

(defun list-notes-in-directory (dir)
  ;; Return a alist of (file-name . path) for all org and markdown files in 'dir'.
  ;; file-name includes the sub-directory path relative to the notes root.
  ;; The list is sorted by file-name ascending.
  (cl-flet ((note-name-and-path (file)
              (let* ((file-name (file-name-base file))
                     (file-ext  (file-name-extension file t)))
                (cons (concat (propertize (notes--dir-label dir)
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
          (mapcan #'list-notes-in-directory (notes-directories))))

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
         (file+headline "~/Documents/org/catchup.org" "✨ Shabbir Dahodwala")
         "** TODO %?Shabbir\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("A" "\tAnthony" entry
         (file+headline "~/Documents/org/catchup.org" "✨ AC (Anthony Comerico)")
         "** TODO %?AC\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("M" "\tManish" entry
         (file+headline "~/Documents/org/catchup.org" "✨ Manish Nair")
         "** TODO %?Manish\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("s" "\tSathya" entry
         (file+headline "~/Documents/org/catchup.org" "🌟 Sathya (Sathyanarayana Chintapalli)")
         "** TODO %?Sathya\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("t" "\tTom" entry
         (file+headline "~/Documents/org/catchup.org" "🌟 Tom Walsh")
         "** TODO %?Tom\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("g" "\tGino" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Gino (Ilougino Rocha)")
         "** TODO %?Gino\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("y" "\tYogesh" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Yogesh Arora")
         "** TODO %?Yogesh\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("r" "\tRishi" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Rishi Raj")
         "** TODO %?Rishi\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("p" "\tPranil" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Pranil Gupta")
         "** TODO %?Pranil\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("j" "\tJas" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Jas (Jaskiran Sodhi)")
         "** TODO %?Jas\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("m" "\tMike" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Weez (Mike Wiesemann)")
         "** TODO %?Mike\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("a" "\tAbhishek, Amey, Alicija, Alex")
        ("ag" "\tAbhishek" entry
         (file+headline "~/Documents/org/catchup.org" "🌟 AG (Abhishek Gupta)")
         "** TODO %?AG\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("am" "\tAmey" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Amey  Purandare")
         "** TODO %?Amey\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("al" "\tAlicija" entry
         (file+headline "~/Documents/org/catchup.org" "👤 Alicija Bulota")
         "** TODO %?Alicija\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ("as" "\tAlex" entry
         (file+headline "~/Documents/org/catchup.org" "✨ Alex Serov")
         "** TODO %?Alex\n  %i\n"
         :prepend t
         :empty-lines-after 0)
        ))

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


;;; Catch-up TODOs
;;; C-c o C: pick a person (same keys as org-capture) and pop up a small buffer
;;; listing the open discussion items (TODO/WORK/WAIT) to raise at the next meeting.

(defvar my/org-catchup-todo-keywords '("TODO" "WORK" "WAIT")
  "TODO keywords considered open discussion items for catch-up popups.")

(defun my/org-catchup--people-table ()
  "Build an `org-mks' table of catchup people from `org-capture-templates'.
Prefix groups (2-element templates, e.g. the \"a\" submenu) are kept; full
templates that target a file+headline (catch-up people plus Inbox/Today) are
kept, producing (KEY DESC HEADLINE FILE).  Prefix groups with no surviving
child are pruned."
  (let (table)
    (dolist (tmpl org-capture-templates)
      (pcase tmpl
        (`(,key ,desc) (push (list key desc) table))              ; prefix group
        (`(,key ,desc ,_type ,target . ,_)
         (when (and (consp target)
                    (eq (car target) 'file+headline))
           (push (list key desc (nth 2 target) (nth 1 target))    ; usable entry
                 table)))))
    (setq table (nreverse table))
    ;; Drop prefix groups (length 2) that no usable entry extends.
    (cl-remove-if (lambda (e)
                    (and (= (length e) 2)
                         (not (cl-some (lambda (o)
                                         (and (> (length o) 2)
                                              (not (equal o e))
                                              (string-prefix-p (car e) (car o))))
                                       table))))
                  table)))

(defun my/org-catchup--collect (headline file)
  "Return list of subtree strings for open items directly under HEADLINE in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (let ((pos (org-find-exact-headline-in-buffer headline)))
          (unless pos (user-error "Headline not found: %s" headline))
          (goto-char pos)
          (let ((child-level (1+ (org-current-level))))
            (org-narrow-to-subtree)
            (delq nil
                  (org-map-entries
                   (lambda ()
                     (when (and (= (org-current-level) child-level)
                                (member (org-get-todo-state)
                                        my/org-catchup-todo-keywords))
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (save-excursion (org-end-of-subtree t t)))))
                   t 'tree))))))))

(defun my/org-catchup--show (name headline file)
  "Display the open items under HEADLINE in FILE in a small, dismissable buffer.
NAME is used for the buffer name and header line."
  (let ((items (my/org-catchup--collect headline file)))
    (if (null items)
        (message "No open (TODO/WORK/WAIT) items for %s" name)
      (let ((buf (get-buffer-create (format "*Catch-up: %s*" name))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (mapconcat (lambda (s) (string-trim-right s)) items "\n")))
          (goto-char (point-min))
          (org-mode)                         ; fontification + svg-tag TODO pills
          (setq-local header-line-format
                      (format " Catch-up — %s   (q to dismiss)" name))
          (view-mode 1))                     ; read-only; q = quit-window
        (let ((win (display-buffer
                    buf '((display-buffer-below-selected display-buffer-at-bottom)))))
          (when win
            (fit-window-to-buffer win (floor (* 0.5 (frame-height))) 5)
            (ignore-errors (window-resize win 1))  ; one blank line of breathing room
            (select-window win)))))))        ; focus so q dismisses immediately

(defun my/org-catchup-todos ()
  "Pick a catch-up person (same keys as `org-capture') and show their open items."
  (interactive)
  (let* ((entry (org-mks (my/org-catchup--people-table)
                         "Select person for catch-up TODOs" "Person: "))
         ;; DESC may contain a leading/interior tab (e.g. "📥\tInbox"); normalise.
         (name (string-trim (replace-regexp-in-string "\t+" " " (nth 1 entry))))
         (headline (nth 2 entry))
         (file (nth 3 entry)))
    (my/org-catchup--show name headline file)))

(define-key global-map (kbd "C-c o c") #'my/org-catchup-todos)


;;; File location

;; Link abbreviations.
;; For example, this allows for absolute links like [[repo:docs/guide.org][Guide]]
(setq org-link-abbrev-alist
      '(("org" . "~/Documents/org/")))

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


;;; DRQS links
;; Make {DRQS 1234567} and {DRQS 1234567<GO>} clickable links to the DRQS web app.
;; C-c o D: open DRQS ticket at point in the browser.

(defun my/org-drqs-open (number)
  "Open DRQS ticket NUMBER in the default browser."
  (browse-url (format "https://drqs.prod.bloomberg.com/ticket/%s" number)))

(defun my/org-drqs-buttonize ()
  "Add font-lock rules to make {DRQS NNN} and {DRQS NNN<GO>} clickable."
  (font-lock-add-keywords
   nil
   '(("{DRQS \\([0-9]+\\)\\(?: *<GO>\\)?}"
      (0 'org-link prepend)))
   t))

(defun my/org-drqs-follow-at-point ()
  "If point is on a {DRQS ...} reference, open it in the browser."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (if (and (string-match "{DRQS \\([0-9]+\\)\\(?: *<GO>\\)?}" line)
             (let ((mbeg (match-beginning 0))
                   (mend (match-end 0))
                   (col  (- (point) (line-beginning-position))))
               (and (>= col mbeg) (<= col mend))))
        (my/org-drqs-open (match-string 1 line))
      (user-error "No DRQS reference at point"))))

(add-hook 'org-mode-hook #'my/org-drqs-buttonize)
(define-key org-mode-map (kbd "C-c o D") #'my/org-drqs-follow-at-point)

;; Also handle {DRQS ...} via C-c C-o (org-open-at-point)
(defun my/org-drqs-open-at-point ()
  "Open {DRQS ...} at point if any; return non-nil if handled."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (and (string-match "{DRQS \\([0-9]+\\)\\(?: *<GO>\\)?}" line)
               (let ((mbeg (match-beginning 0))
                     (mend (match-end 0))
                     (col  (- (point) (line-beginning-position))))
                 (and (>= col mbeg) (<= col mend))))
      (my/org-drqs-open (match-string 1 line))
      t)))

(add-hook 'org-open-at-point-functions #'my/org-drqs-open-at-point)


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


;; Sync todos to Google Drive for PlainOrg

(defun org-sync ()
  "Copy todos and catch up notes to Google Drive."
  (interactive)
  (let ((dest-dir "/Users/pgrenet/Library/CloudStorage/GoogleDrive-pgrenet@bloomberg.net/My Drive/org/"))
    (if (file-directory-p dest-dir)
        (progn
          (copy-file "~/Documents/org/todo.org" (concat dest-dir "todo.org") t)
          (copy-file "~/Documents/org/catchup.org" (concat dest-dir "catchup.org") t)
          (message "org-sync: synced"))
      (message "org-sync: Google Drive not mounted, skipping"))))

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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
