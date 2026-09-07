;;;; Dashboard: the projects worked on most recently -*- lexical-binding: t -*-
;;;
;;; `projects/' in the org repo holds one project per entry: either a single
;;; file (projects/23x5-trading.org) or a directory (projects/metadata/).
;;; Which ones are live is a question git can answer better than the
;;; filesystem can -- mtimes move for reasons that have nothing to do with
;;; working on something -- so the ordering comes from `git log'.
;;;
;;; Each project links to its README when it has one, and otherwise to the
;;; file that was touched last.  Projects whose files have since been renamed
;;; away or deleted are skipped.

(require 'cl-lib)

(declare-function my-dashboard-insert-heading "my-dashboard")
(declare-function my-dashboard-insert-empty "my-dashboard")
(declare-function my-dashboard-insert-file-button "my-dashboard")
(declare-function my-dashboard-relative-age "my-dashboard")
(declare-function my-dashboard-width "my-dashboard")
(defvar my/org-repo)
(defvar my-dashboard-projects)

(defconst my-dashboard--projects-directory "projects"
  "Directory of the org repo holding the projects, relative to its root.")

(defconst my-dashboard--git-commits 400
  "How far back in `git log' to look for project activity.")


;;; Asking git what was touched, and when

(defun my-dashboard--git-log ()
  "Return the recent commits touching the projects directory.
The result is a list of (SECONDS . RELATIVE-PATH), newest first, one entry
per file per commit."
  (let ((default-directory my/org-repo)
        (timestamp nil)
        (entries nil))
    (with-temp-buffer
      (when (eq 0 (process-file "git" nil t nil
                                "log" "--no-merges"
                                "-n" (number-to-string
                                      my-dashboard--git-commits)
                                "--name-only" "--pretty=format:@%ct"
                                "--" my-dashboard--projects-directory))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-prefix-p "@" line)
              (setq timestamp (string-to-number (substring line 1))))
             ((and timestamp (not (string-empty-p line)))
              (push (cons timestamp line) entries))))
          (forward-line 1))))
    (nreverse entries)))

(defun my-dashboard--project-id (path)
  "Return the project PATH belongs to, as a path relative to the repo root.
PATH is relative to the repo root too.  Returns nil for anything outside
the projects directory."
  (let ((prefix (concat my-dashboard--projects-directory "/")))
    (when (string-prefix-p prefix path)
      (let* ((rest (substring path (length prefix)))
             (slash (string-search "/" rest)))
        (concat prefix (if slash (substring rest 0 slash) rest))))))


;;; Turning a project into something to click on

(defun my-dashboard--project-target (project fallback)
  "Return the file to open for PROJECT, a path relative to the repo root.
FALLBACK is the project file touched most recently.  Returns nil when
nothing of the project is left on disk."
  (let ((absolute (expand-file-name project my/org-repo)))
    (cond
     ((file-directory-p absolute)
      (or (cl-find-if #'file-readable-p
                      (mapcar (lambda (name) (expand-file-name name absolute))
                              '("README.org" "README.md" "index.org")))
          (let ((file (expand-file-name fallback my/org-repo)))
            (and (file-readable-p file) file))))
     ((file-readable-p absolute) absolute))))

(defun my-dashboard--file-title (file)
  "Return the title declared at the top of FILE, or nil."
  (with-temp-buffer
    (ignore-errors (insert-file-contents file nil 0 4096))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (cond
       ((re-search-forward "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" nil t)
        (match-string 1))
       ((re-search-forward "^#[ \t]+\\(.+?\\)[ \t]*$" nil t)
        (match-string 1))
       ((re-search-forward "^\\*[ \t]+\\(.+?\\)[ \t]*$" nil t)
        (match-string 1))))))

(defun my-dashboard--project-name (project target)
  "Return the display name of PROJECT, whose file to open is TARGET."
  (or (and target (my-dashboard--file-title target))
      (capitalize
       (string-replace "-" " "
                       (file-name-base (directory-file-name project))))))

(defun my-dashboard--recent-projects (count)
  "Return up to COUNT recently updated projects, newest first.
Each element is a plist with :name, :file and :time."
  (let ((seen (make-hash-table :test #'equal))
        (projects nil))
    (cl-loop for (timestamp . path) in (my-dashboard--git-log)
             while (< (length projects) count)
             for project = (my-dashboard--project-id path)
             when (and project (not (gethash project seen)))
             do (puthash project t seen)
                (when-let* ((target (my-dashboard--project-target project path)))
                  (push (list :name (my-dashboard--project-name project target)
                              :file target
                              :time (seconds-to-time timestamp))
                        projects)))
    (nreverse projects)))


;;; Rendering

(defun my-dashboard-insert-projects ()
  "Insert the most recently updated projects, each one a link."
  (my-dashboard-insert-heading
   (format "Recent projects — top %d" my-dashboard-projects))
  (let ((failure nil)
        (projects nil))
    (condition-case err
        (setq projects (my-dashboard--recent-projects my-dashboard-projects))
      (error (setq failure (error-message-string err))))
    (cond
     (failure
      (my-dashboard-insert-empty (format "git unavailable: %s" failure)))
     ((null projects)
      (my-dashboard-insert-empty "No recent project activity."))
     (t
      (let ((column (- (my-dashboard-width) 14)))
        (dolist (project projects)
          (let ((name (plist-get project :name))
                (age (my-dashboard-relative-age (plist-get project :time))))
            (insert "  ")
            (my-dashboard-insert-file-button name (plist-get project :file))
            (insert (propertize " " 'display
                                `(space :align-to ,(max (+ (current-column) 2)
                                                        column)))
                    (propertize age 'font-lock-face 'my-dashboard-detail)
                    "\n"))))))))

;;; my-dashboard-projects.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
