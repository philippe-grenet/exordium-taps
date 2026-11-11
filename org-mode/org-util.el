;;;; Org Mode keys and small editing commands -*- lexical-binding: t -*-
;;;
;;; The C-c o prefix and the handful of commands behind it.  Nothing here knows
;;; about the notes repo, so after-init.el loads this file on every machine.
;;;
;;;   super-<arrow> : navigate the tree     C-c o i : insert an image
;;;   C-c l         : store a link          C-c o m : insert a Mermaid diagram
;;;   C-c o d       : toggle TODO/DONE      C-c o o : open the image at point
;;;   C-c o l       : paste a Jira link     C-c o x : tag old entries :ARCHIVE:
;;;   C-c o y       : paste TSV as a table  C-c o C : color a numeric column

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

;;; C-c o y: Paste TSV (e.g. cells copied from Excel) as an Org table
(load-file "~/.emacs.d/taps/common/table-format.el")

(defun org-table-paste-tsv (&optional no-header)
  "Insert the clipboard, in TSV format, as an Org table at point.
This is the format Excel (and most spreadsheets) put on the
clipboard when copying a block of cells.  The first row is treated
as a header and followed by a horizontal rule; with prefix argument
NO-HEADER, omit the rule."
  (interactive "P")
  (my/table-paste-tsv "\\vert{}" (not no-header) #'org-table-align))

(define-key org-mode-map (kbd "C-c o y") #'org-table-paste-tsv)

;;; C-c o C: Color a numeric table column by value (like Excel's color scales)
(require 'cl-lib)
(require 'color)

(defgroup my-org-table-color nil
  "Coloring numeric Org table columns by value (`org-table-color-column')."
  :group 'org-table)

(defcustom my-org-table-color-low "#f38ba8"
  "Cell color for the lowest value of the default scale (Catppuccin red)."
  :type 'color)

(defcustom my-org-table-color-high "#a6e3a1"
  "Cell color for the highest value of both scales (Catppuccin green)."
  :type 'color)

(defcustom my-org-table-color-negative "#f38ba8"
  "Cell color for the most negative value of the zero-centered scale.
The default is Catppuccin red."
  :type 'color)

(defcustom my-org-table-color-zero "#cdd6f4"
  "Cell color for zero in the zero-centered scale (Catppuccin text)."
  :type 'color)

(defun my-org-table-color-rgb (color)
  "Return the (R G B) components, from 0 to 1, of COLOR.
A \"#rrggbb\" COLOR is parsed directly, so the result does not depend
on the display; other color names go through `color-name-to-rgb'."
  (if (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" color)
      (mapcar (lambda (i)
                (/ (string-to-number (substring color i (+ i 2)) 16) 255.0))
              '(1 3 5))
    (or (color-name-to-rgb color)
        (user-error "Unknown color: %s" color))))

(defun my-org-table-color-blend (from to ratio)
  "Return the hex color at RATIO (0 to 1) between colors FROM and TO."
  (apply #'color-rgb-to-hex
         (append (cl-mapcar (lambda (a b) (+ a (* ratio (- b a))))
                            (my-org-table-color-rgb from)
                            (my-org-table-color-rgb to))
                 '(2))))

(defun my-org-table-color-cells (col)
  "Return (BEG END VALUE) for each numeric cell of column COL in the table.
BEG and END span the whole cell between its two vertical bars."
  (let ((cells '()))
    (save-excursion
      (goto-char (org-table-begin))
      (while (< (point) (org-table-end))
        (unless (org-at-table-hline-p)
          (let ((bars '())
                (eol (line-end-position)))
            (while (search-forward "|" eol t)
              (push (point) bars))
            (setq bars (nreverse bars))
            (when (> (length bars) col)
              (let* ((beg (nth (1- col) bars))
                     (end (1- (nth col bars)))
                     (text (string-trim (buffer-substring-no-properties beg end))))
                (when (string-match-p
                       "\\`[-+]?\\([0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?%?\\'"
                       text)
                  (push (list beg end (string-to-number text)) cells))))))
        (forward-line 1)))
    (nreverse cells)))

(defun my-org-table-color-remove (col)
  "Remove the color overlays of column COL in the table at point.
Return non-nil if there were any."
  (let ((found nil))
    (dolist (ov (overlays-in (org-table-begin) (org-table-end)))
      (when (eql (overlay-get ov 'my-org-table-color) col)
        (delete-overlay ov)
        (setq found t)))
    found))

(defun org-table-color-column (&optional centered)
  "Color the background of the numeric cells of the table column at point.
The color depends on the cell value: by default it goes from
`my-org-table-color-low' for the lowest value of the column to
`my-org-table-color-high' for the highest.  With prefix argument
CENTERED, use a scale centered on zero instead: negative values go
from `my-org-table-color-zero' to `my-org-table-color-negative' and
positive ones to `my-org-table-color-high', each side relative to its
own extreme.  Non-numeric cells, such as the header, are skipped.

Run again on the same column to remove the colors.  They are only
displayed, not saved, and they disappear when realigning changes the
table."
  (interactive "P")
  (unless (org-at-table-p)
    (user-error "Not in an Org table"))
  (let ((col (org-table-current-column)))
    (if (my-org-table-color-remove col)
        (message "Removed colors from column %d" col)
      (let* ((cells (or (my-org-table-color-cells col)
                        (user-error "No numbers in column %d" col)))
             (values (mapcar #'caddr cells))
             (lo (apply #'min values))
             (hi (apply #'max values)))
        (pcase-dolist (`(,beg ,end ,value) cells)
          (let* ((bg (cond
                      (centered
                       (cond ((and (< value 0) (< lo 0))
                              (my-org-table-color-blend
                               my-org-table-color-zero my-org-table-color-negative
                               (/ value (float lo))))
                             ((and (> value 0) (> hi 0))
                              (my-org-table-color-blend
                               my-org-table-color-zero my-org-table-color-high
                               (/ value (float hi))))
                             (t my-org-table-color-zero)))
                      ((= lo hi) my-org-table-color-high)
                      (t (my-org-table-color-blend
                          my-org-table-color-low my-org-table-color-high
                          (/ (- value lo) (float (- hi lo)))))))
                 (fg (if (color-dark-p (my-org-table-color-rgb bg)) "white" "black"))
                 (ov (make-overlay beg end)))
            (overlay-put ov 'my-org-table-color col)
            (overlay-put ov 'face `(:background ,bg :foreground ,fg))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'priority 10)))
        (message "Colored %d cells in column %d (range %s to %s)"
                 (length cells) col lo hi)))))

(define-key org-mode-map (kbd "C-c o C") #'org-table-color-column)


;;; org-util.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
