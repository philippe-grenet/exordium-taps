;;;; Org Mode keys and small editing commands -*- lexical-binding: t -*-
;;;
;;; The C-c o prefix and the handful of commands behind it.  Nothing here knows
;;; about the notes repo, so after-init.el loads this file on every machine.
;;;
;;;   super-<arrow> : navigate the tree     C-c o i : insert an image
;;;   C-c l         : store a link          C-c o m : insert a Mermaid diagram
;;;   C-c o d       : toggle TODO/DONE      C-c o o : open the image at point
;;;   C-c o l       : paste a Jira link     C-c o x : tag old entries :ARCHIVE:

(require 'org)
(require 'org-element)

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


;;; org-util.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
