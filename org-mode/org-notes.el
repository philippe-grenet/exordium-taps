;;;; Locating and opening the second-brain notes -*- lexical-binding: t -*-
;;;
;;; Everything here addresses files inside `my/org-repo' (see
;;; taps/common/before-init.el), so after-init.el only loads this file on a
;;; machine that actually has the repo.
;;;
;;;   F12   : pick any note in the repo, by name, with completion
;;;   S-F12 : open todo.org
;;;   C-F12 : open catchup.org
;;;
;;; It also points `org-agenda-files' and the `org:' link abbreviation at the
;;; repo, so that absolute links like [[org:resources/guide.org][Guide]] work
;;; from any file.

(require 'org)
(require 'cl-lib)

(defvar my/org-repo)
(defvar org-agenda-custom-commands)      ; org-agenda.el, loaded on demand
(declare-function my/org-file "before-init")


;;; Task list

(defun colorize-note-extension (file-name)
  (let ((file-ext (file-name-extension file-name t)))
    (concat (file-name-base file-name)
            (propertize file-ext 'face (if (string= file-ext ".org")
                                           'helm-ff-truename
                                         'helm-ff-file-extension)))))


(defconst top-level-notes
  `((,(colorize-note-extension "todo.org")    . ,(my/org-file "todo.org"))
    (,(colorize-note-extension "catchup.org") . ,(my/org-file "catchup.org"))
    (,(colorize-note-extension "roadmap.org") . ,(my/org-file "roadmap.org"))))

(defconst notes-root-directories
  (list (my/org-file "areas/")
        (my/org-file "projects/")
        (my/org-file "resources/"))
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
For example <org repo>/projects/subA/subB/ yields \"projects/subA/subB\"."
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
  "Open a note as FILE from the list of active notes in the org repo."
  (interactive
   (list
    (completing-read "Open: " (list-all-notes))))
  (find-file (cdr (assoc file (list-all-notes)))))

(global-set-key [(f12)] #'open-todos)

;; Quick access

(defun open-todo-file ()
  "Open my todo.org file."
  (interactive)
  (find-file (my/org-file "todo.org")))

(global-set-key [(shift f12)] #'open-todo-file)

(defun open-catchup-file ()
  "Open my catch up file."
  (interactive)
  (find-file (my/org-file "catchup.org")))

(global-set-key [(control f12)] #'open-catchup-file)


;;; File location

;; Link abbreviations.
;; For example, this allows for absolute links like [[org:docs/guide.org][Guide]]
(setq org-link-abbrev-alist `(("org" . ,my/org-repo)))


;;; Org agenda

(setq org-agenda-files (list my/org-repo))
(setq org-agenda-custom-commands
      '(("c" "Philippe's agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority items:")))
          (agenda "")
          (alltodo "")))))

;;; org-notes.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
