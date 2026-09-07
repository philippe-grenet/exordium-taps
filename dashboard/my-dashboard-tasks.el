;;;; Dashboard: the tasks that matter this week -*- lexical-binding: t -*-
;;;
;;; Read straight out of todo.org, which is organised as top-level sections
;;; (Inbox, Today, Week, Backlog...) with one task per second-level heading.
;;; Two rules pick what lands on the dashboard:
;;;
;;;   - every task in the *first block* of the Week section.  That section is
;;;     written as blocks separated by blank lines, oldest concerns further
;;;     down; only the first block is this week's;
;;;   - every task in the Today or Week sections carrying a priority cookie,
;;;     a ⭐ (important) or a 🔥 (urgent), wherever it sits in the section.
;;;
;;; Done tasks are dropped.  Parsing happens in a temporary buffer in Org
;;; mode, so the file-local `#+todo:' keywords decide what "done" means,
;;; without paying for the org-mode-hook of a 50k file.

(require 'org)
(require 'cl-lib)

(declare-function my-dashboard-insert-heading "my-dashboard")
(declare-function my-dashboard-insert-empty "my-dashboard")
(declare-function my-dashboard-set-target "my-dashboard")
(declare-function my-dashboard-svg-tag "my-dashboard")
(declare-function my-dashboard--org-file "my-dashboard")

(defconst my-dashboard--flag-regexp "[⭐🔥]"
  "Regexp matching the emoji that flag a task as important or urgent.")


;;; Parsing todo.org

(defun my-dashboard--section-region (title)
  "Return (BEG . END) for the top-level section headed TITLE, or nil.
BEG is the start of the line after the heading; END is the start of the
next top-level heading, or point-max."
  (save-excursion
    (goto-char (point-min))
    ;; Only the right-hand boundary is constrained: the headings start with an
    ;; emoji, whose syntax class makes `\\_<' unreliable there.
    (when (re-search-forward
           (concat "^\\* .*" (regexp-quote title) "\\(?:[^[:word:]\n]\\|$\\)")
           nil t)
      (goto-char (match-beginning 0))
      (forward-line 1)
      (let ((beg (point)))
        (cons beg (if (re-search-forward "^\\* " nil t)
                      (match-beginning 0)
                    (point-max)))))))

(defun my-dashboard--first-block-end (beg end)
  "Return the end of the first blank-line-delimited block in BEG..END."
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \t\n" end)
    (if (re-search-forward "^[ \t]*$" end t)
        (match-beginning 0)
      end)))

(defun my-dashboard--task-at-point ()
  "Return a plist describing the task on the heading line at point.
The plist has :todo, :priority, :title, :flagged and :line, where :line is
the raw heading line used later to find the task again in the real file.
Returns nil for a done heading."
  (pcase-let ((`(,_level ,_reduced ,todo ,priority ,title ,_tags)
               (org-heading-components)))
    (unless (member todo org-done-keywords)
      (let ((title (or title "")))
        (list :todo todo
              :priority priority
              :title (org-link-display-format title)
              :flagged (and (string-match-p my-dashboard--flag-regexp title) t)
              :line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))))))

(defun my-dashboard--tasks-in-region (beg end all)
  "Return the tasks on second-level headings between BEG and END.
With ALL non-nil, return every task; otherwise only those with a priority
cookie or a ⭐/🔥 flag."
  (save-excursion
    (goto-char beg)
    (let (tasks)
      (while (re-search-forward "^\\*\\* " end t)
        (beginning-of-line)
        (when-let* ((task (my-dashboard--task-at-point)))
          (when (or all (plist-get task :priority) (plist-get task :flagged))
            (push task tasks)))
        (forward-line 1))
      (nreverse tasks))))

(defun my-dashboard--tasks ()
  "Return this week's tasks from todo.org, in the order they appear there."
  (let ((file (my-dashboard--org-file "todo.org")))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (let* ((today (my-dashboard--section-region "Today"))
               (week (my-dashboard--section-region "Week"))
               (tasks
                (append
                 (when today
                   (my-dashboard--tasks-in-region (car today) (cdr today) nil))
                 (when week
                   (let ((block-end (my-dashboard--first-block-end
                                     (car week) (cdr week))))
                     (append
                      (my-dashboard--tasks-in-region (car week) block-end t)
                      (my-dashboard--tasks-in-region
                       block-end (cdr week) nil)))))))
          (cl-remove-duplicates tasks
                                :key (lambda (task) (plist-get task :line))
                                :test #'string=
                                :from-end t))))))


;;; Rendering

(defconst my-dashboard--task-title-column 12
  "Buffer column the task titles start at, past the status pills.
The indent is 2, a keyword pill is 5 columns wide and a priority pill 3,
so 12 is the first column that clears both with a space in between.")

(defun my-dashboard--task-label (task)
  "Return the propertized one-line label for TASK.
The TODO keyword and the priority cookie are rendered as the same SVG
pills an Org buffer shows, and the titles are aligned past them."
  (let* ((todo (plist-get task :todo))
         (priority (plist-get task :priority))
         (title (plist-get task :title))
         (tags (delq nil
                     (list (when todo
                             (my-dashboard-svg-tag todo
                                                   (org-get-todo-face todo)))
                           (when priority
                             (my-dashboard-svg-tag (format "[#%c]" priority)
                                                   'org-priority))))))
    (concat
     (string-join tags " ")
     ;; A stretch space rather than padding: the pills are images, and their
     ;; width has nothing to do with the length of the text behind them.  The
     ;; literal space before it guarantees a gap even for a keyword long
     ;; enough to overrun the column.
     " "
     (propertize " " 'display `(space :align-to
                                      ,my-dashboard--task-title-column))
     (if (plist-get task :flagged)
         (propertize title 'font-lock-face 'my-dashboard-important)
       title))))

(defun my-dashboard-insert-tasks ()
  "Insert this week's tasks, each one a link into todo.org."
  (my-dashboard-insert-heading "This week")
  (let ((file (my-dashboard--org-file "todo.org"))
        (failure nil)
        (tasks nil))
    (condition-case err
        (setq tasks (my-dashboard--tasks))
      (error (setq failure (error-message-string err))))
    (cond
     (failure
      (my-dashboard-insert-empty (format "todo.org unavailable: %s" failure)))
     ((null tasks)
      (my-dashboard-insert-empty "Nothing flagged for this week."))
     (t
      ;; Not buttons: these are read as a list, not as links, and the `button'
      ;; face would underline every one of them.  RET still opens them, via
      ;; the target property.
      (dolist (task tasks)
        (let ((start (point)))
          (insert "  " (my-dashboard--task-label task) "\n")
          (my-dashboard-set-target start (point) file
                                   (plist-get task :line))))))))

;;; my-dashboard-tasks.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
