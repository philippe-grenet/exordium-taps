;;; table-format.el --- Toggle org tables between standard and box-drawing format -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Convert org-mode tables between two formats:
;;
;; Format 1 (standard org):
;;   | Header | Col2 |
;;   |--------|------|
;;   | data1  | data2|
;;
;; Format 2 (box-drawing):
;;   ┌────────┬──────┐
;;   │ Header │ Col2 │
;;   ├────────┼──────┤
;;   │ data1  │ data2│
;;   └────────┴──────┘
;;
;; Bound to C-c o T (toggle).  C-u C-c o T forces Format 2 -> Format 1.

;;; Code:

(require 'cl-lib)

;;;; Box-drawing character constants

(defconst my/org-table-box-h   ?─ "Horizontal line (U+2500).")
(defconst my/org-table-box-v   ?│ "Vertical line (U+2502).")
(defconst my/org-table-box-tl  ?┌ "Top-left corner (U+250C).")
(defconst my/org-table-box-tr  ?┐ "Top-right corner (U+2510).")
(defconst my/org-table-box-bl  ?└ "Bottom-left corner (U+2514).")
(defconst my/org-table-box-br  ?┘ "Bottom-right corner (U+2518).")
(defconst my/org-table-box-tt  ?┬ "Top tee (U+252C).")
(defconst my/org-table-box-bt  ?┴ "Bottom tee (U+2534).")
(defconst my/org-table-box-lt  ?├ "Left tee (U+251C).")
(defconst my/org-table-box-rt  ?┤ "Right tee (U+2524).")
(defconst my/org-table-box-x   ?┼ "Cross (U+253C).")

(defconst my/org-table-line-regexp
  "^[ \t]*\\(?:|\\|[┌│├└]\\)"
  "Regexp matching any line that is part of a table in either format.")

;;;; Format detection

(defun my/org-table-detect-format ()
  "Detect the table format at point.
Return `format-1' for standard org tables, `format-2' for
box-drawing tables, or nil if point is not in a table."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at-p "^[ \t]*[┌│├└]")
      'format-2)
     ((looking-at-p "^[ \t]*|")
      'format-1)
     (t nil))))

;;;; Table boundary detection

(defun my/org-table-begin ()
  "Return the position of the first line of the table at point."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (looking-at-p my/org-table-line-regexp))
      (forward-line -1))
    (unless (looking-at-p my/org-table-line-regexp)
      (forward-line 1))
    (point)))

(defun my/org-table-end ()
  "Return the position just after the last line of the table at point."
  (save-excursion
    (beginning-of-line)
    (while (and (not (eobp))
                (looking-at-p my/org-table-line-regexp))
      (forward-line 1))
    (point)))

;;;; Parsing

(defun my/org-table--split-cells-format-1 (line)
  "Split a Format 1 data LINE into a list of trimmed cell strings."
  (let ((stripped (string-trim line)))
    ;; Remove leading and trailing |
    (when (string-prefix-p "|" stripped)
      (setq stripped (substring stripped 1)))
    (when (string-suffix-p "|" stripped)
      (setq stripped (substring stripped 0 -1)))
    (mapcar #'string-trim (split-string stripped "|"))))

(defun my/org-table--split-cells-format-2 (line)
  "Split a Format 2 data LINE into a list of trimmed cell strings."
  (let ((stripped (string-trim line))
        (delim (string my/org-table-box-v)))
    ;; Remove leading and trailing │
    (when (string-prefix-p delim stripped)
      (setq stripped (substring stripped 1)))
    (when (string-suffix-p delim stripped)
      (setq stripped (substring stripped 0 -1)))
    (mapcar #'string-trim (split-string stripped delim))))

(defun my/org-table--separator-p (line format)
  "Return non-nil if LINE is a separator line for the given FORMAT."
  (let ((trimmed (string-trim line)))
    (pcase format
      ('format-1
       (string-match-p "^|[-+|]+|?$" trimmed))
      ('format-2
       (string-match-p "^[┌├└]" trimmed)))))

(defun my/org-table-parse (beg end)
  "Parse the table between BEG and END.
Return a list (FORMAT ROWS INDENT) where FORMAT is `format-1' or
`format-2', ROWS is a list of lists of trimmed cell strings (first
element is the header), and INDENT is the leading whitespace string."
  (let* ((text (buffer-substring-no-properties beg end))
         (lines (split-string text "\n" t))
         (format (save-excursion
                   (goto-char beg)
                   (my/org-table-detect-format)))
         (indent (if (string-match "^\\([ \t]*\\)" (car lines))
                     (match-string 1 (car lines))
                   ""))
         (rows '()))
    (dolist (line lines)
      (unless (my/org-table--separator-p line format)
        (let ((cells (pcase format
                       ('format-1 (my/org-table--split-cells-format-1 line))
                       ('format-2 (my/org-table--split-cells-format-2 line)))))
          (when cells
            (push cells rows)))))
    (list format (nreverse rows) indent)))

;;;; Width computation

(defun my/org-table--compute-widths (rows)
  "Compute the max content width per column across all ROWS.
Return a list of integers."
  (let* ((ncols (length (car rows)))
         (widths (make-list ncols 0)))
    (dolist (row rows)
      (cl-loop for cell in row
               for i from 0
               do (setf (nth i widths)
                        (max (nth i widths) (string-width cell)))))
    widths))

;;;; Rendering helpers

(defun my/org-table--separator-line (left mid right widths)
  "Build a separator line from box-drawing characters.
LEFT is the left edge char, MID the junction char, RIGHT the right
edge char.  WIDTHS is a list of column widths (including padding)."
  (concat (string left)
          (mapconcat (lambda (w) (make-string w my/org-table-box-h))
                     widths
                     (string mid))
          (string right)))

(defun my/org-table--center-string (text width)
  "Center TEXT within WIDTH characters using spaces."
  (let* ((text-width (string-width text))
         (total-pad (max 0 (- width text-width)))
         (left-pad (/ total-pad 2))
         (right-pad (- total-pad left-pad)))
    (concat (make-string left-pad ?\s)
            text
            (make-string right-pad ?\s))))

(defun my/org-table--left-pad-string (text width)
  "Left-align TEXT within WIDTH characters, 1-space left padding."
  (let* ((text-width (string-width text))
         (right-pad (max 0 (- width 1 text-width))))
    (concat " " text (make-string right-pad ?\s))))

(defun my/org-table--data-line (delim cells col-widths center-p)
  "Build a data line using DELIM as the vertical separator.
CELLS is a list of cell strings, COL-WIDTHS the column widths
\(including padding).  If CENTER-P is non-nil, center the text;
otherwise left-align with 1-space padding."
  (concat (string delim)
          (mapconcat
           (lambda (pair)
             (let ((cell (car pair))
                   (width (cdr pair)))
               (if center-p
                   (my/org-table--center-string cell width)
                 (my/org-table--left-pad-string cell width))))
           (cl-mapcar #'cons cells col-widths)
           (string delim))
          (string delim)))

;;;; Renderers

(defun my/org-table-render-format-1 (rows indent)
  "Render ROWS as a Format 1 (standard org) table.
INDENT is prepended to each line.  First row is the header."
  (let* ((content-widths (my/org-table--compute-widths rows))
         (col-widths (mapcar (lambda (w) (+ w 2)) content-widths))
         (header (car rows))
         (data (cdr rows))
         (lines '()))
    ;; Header row
    (push (concat indent (my/org-table--data-line ?| header col-widths nil))
          lines)
    ;; Separator
    (push (concat indent "|"
                  (mapconcat (lambda (w) (make-string w ?-))
                             col-widths "|")
                  "|")
          lines)
    ;; Data rows
    (dolist (row data)
      (push (concat indent (my/org-table--data-line ?| row col-widths nil))
            lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun my/org-table-render-format-2 (rows indent)
  "Render ROWS as a Format 2 (box-drawing) table.
INDENT is prepended to each line.  First row is the header (centered)."
  (let* ((content-widths (my/org-table--compute-widths rows))
         (col-widths (mapcar (lambda (w) (+ w 2)) content-widths))
         (header (car rows))
         (data (cdr rows))
         (lines '()))
    ;; Top border
    (push (concat indent
                  (my/org-table--separator-line
                   my/org-table-box-tl my/org-table-box-tt my/org-table-box-tr
                   col-widths))
          lines)
    ;; Header row (centered)
    (push (concat indent
                  (my/org-table--data-line my/org-table-box-v
                                          header col-widths t))
          lines)
    ;; Header separator
    (push (concat indent
                  (my/org-table--separator-line
                   my/org-table-box-lt my/org-table-box-x my/org-table-box-rt
                   col-widths))
          lines)
    ;; Data rows with separators between them
    (cl-loop for (row . rest) on data do
             (push (concat indent
                           (my/org-table--data-line my/org-table-box-v
                                                   row col-widths nil))
                   lines)
             (when rest
               (push (concat indent
                             (my/org-table--separator-line
                              my/org-table-box-lt my/org-table-box-x
                              my/org-table-box-rt col-widths))
                     lines)))
    ;; Bottom border
    (push (concat indent
                  (my/org-table--separator-line
                   my/org-table-box-bl my/org-table-box-bt my/org-table-box-br
                   col-widths))
          lines)
    (mapconcat #'identity (nreverse lines) "\n")))

;;;; Resize / word-wrap support

(defun my/org-table--wrap-text (text width)
  "Wrap TEXT to fit within WIDTH characters.
Break at word boundaries.  If a single word exceeds WIDTH, it is
placed alone on its line (overflow allowed).
Return a list of line strings."
  (if (string-empty-p text)
      (list "")
    (let ((words (split-string text " " t))
          (lines '())
          (current ""))
      (dolist (word words)
        (if (string-empty-p current)
            (setq current word)
          (if (<= (+ (string-width current) 1 (string-width word)) width)
              (setq current (concat current " " word))
            (push current lines)
            (setq current word))))
      (push current lines)
      (nreverse lines))))

(defun my/org-table--compute-min-widths (rows)
  "Compute the minimum viable content width per column across all ROWS.
`my/org-table--wrap-text' never breaks inside a word, so a column
narrower than its longest word would overflow and misalign the table.
Return a list of integers."
  (let* ((ncols (length (car rows)))
         (mins (make-list ncols 0)))
    (dolist (row rows)
      (cl-loop for cell in row
               for i from 0
               while (< i ncols)
               do (dolist (word (split-string cell " " t))
                    (setf (nth i mins)
                          (max (nth i mins) (string-width word))))))
    mins))

(defun my/org-table--indices-by-desc (values)
  "Return the indices of VALUES, sorted by descending value."
  (sort (number-sequence 0 (1- (length values)))
        (lambda (a b) (> (nth a values) (nth b values)))))

(defun my/org-table--compute-proportional-widths (content-widths budget
                                                                &optional
                                                                min-widths)
  "Distribute BUDGET across columns proportional to CONTENT-WIDTHS.
MIN-WIDTHS, when non-nil, is a list of per-column lower bounds (see
`my/org-table--compute-min-widths'): no column is allocated less than
its bound, so wrapped cells never overflow their column.  No column is
allocated more than its content width either.  Return a list of
integers summing to BUDGET, except when the lower bounds do not fit
within BUDGET, in which case the sum is larger and the caller's table
ends up wider than requested."
  (let* ((ncols (length content-widths))
         (caps (mapcar (lambda (w) (max 1 w)) content-widths))
         (mins (if min-widths
                   (cl-mapcar (lambda (m c) (max 1 (min m c))) min-widths caps)
                 (make-list ncols 1)))
         (total (apply #'+ caps))
         (raw (cl-mapcar
               (lambda (c m)
                 (min c (max m (floor (* budget (/ (float c) total))))))
               caps mins))
         (remainder (- budget (apply #'+ raw))))
    ;; Grow the widest columns that are still narrower than their content.
    (while (and (> remainder 0)
                (cl-some (lambda (i) (< (nth i raw) (nth i caps)))
                         (number-sequence 0 (1- ncols))))
      (cl-loop for i in (my/org-table--indices-by-desc caps)
               while (> remainder 0)
               when (< (nth i raw) (nth i caps))
               do (cl-incf (nth i raw)) (cl-decf remainder)))
    ;; Shrink the widest columns that are still above their minimum.
    (while (and (< remainder 0)
                (cl-some (lambda (i) (> (nth i raw) (nth i mins)))
                         (number-sequence 0 (1- ncols))))
      (cl-loop for i in (my/org-table--indices-by-desc raw)
               while (< remainder 0)
               when (> (nth i raw) (nth i mins))
               do (cl-decf (nth i raw)) (cl-incf remainder)))
    raw))

(defun my/org-table--wrap-rows (rows content-widths)
  "Wrap cell text in ROWS to fit CONTENT-WIDTHS.
Return a list of wrapped-rows, each being a list of cell-line
lists of uniform height."
  (mapcar
   (lambda (row)
     (let* ((ncols (length content-widths))
            (padded-row (if (< (length row) ncols)
                            (append row (make-list (- ncols (length row)) ""))
                          row))
            (wrapped (cl-mapcar #'my/org-table--wrap-text
                                padded-row content-widths))
            (max-h (apply #'max (mapcar #'length wrapped))))
       (mapcar (lambda (cell-lines)
                 (let ((pad (- max-h (length cell-lines))))
                   (if (> pad 0)
                       (append cell-lines (make-list pad ""))
                     cell-lines)))
               wrapped)))
   rows))

(defun my/org-table--render-multiline-row (delim cell-lines col-widths center-p)
  "Render one multi-line row as a list of line strings.
DELIM is the vertical border character.
CELL-LINES is a list of lists (one per column) of visual-line strings.
COL-WIDTHS includes padding.  CENTER-P centers text if non-nil."
  (let ((height (length (car cell-lines)))
        (result '()))
    (dotimes (i height)
      (let ((cells (mapcar (lambda (cl) (nth i cl)) cell-lines)))
        (push (my/org-table--data-line delim cells col-widths center-p)
              result)))
    (nreverse result)))

(defun my/org-table--render-resized (format wrapped-rows col-widths indent)
  "Render a resized table with multi-line cells.
FORMAT is `format-1' or `format-2'.
WRAPPED-ROWS is the output of `my/org-table--wrap-rows'.
COL-WIDTHS is the list of column widths (including padding).
INDENT is the leading whitespace string."
  (let ((lines '()))
    (pcase format
      ('format-2
       (push (concat indent
                     (my/org-table--separator-line
                      my/org-table-box-tl my/org-table-box-tt my/org-table-box-tr
                      col-widths))
             lines)
       (dolist (hl (my/org-table--render-multiline-row
                    my/org-table-box-v (car wrapped-rows) col-widths t))
         (push (concat indent hl) lines))
       (push (concat indent
                     (my/org-table--separator-line
                      my/org-table-box-lt my/org-table-box-x my/org-table-box-rt
                      col-widths))
             lines)
       (cl-loop for (row . rest) on (cdr wrapped-rows) do
                (dolist (dl (my/org-table--render-multiline-row
                             my/org-table-box-v row col-widths nil))
                  (push (concat indent dl) lines))
                (when rest
                  (push (concat indent
                                (my/org-table--separator-line
                                 my/org-table-box-lt my/org-table-box-x
                                 my/org-table-box-rt col-widths))
                        lines)))
       (push (concat indent
                     (my/org-table--separator-line
                      my/org-table-box-bl my/org-table-box-bt my/org-table-box-br
                      col-widths))
             lines))

      ('format-1
       (dolist (hl (my/org-table--render-multiline-row
                    ?| (car wrapped-rows) col-widths nil))
         (push (concat indent hl) lines))
       (push (concat indent "|"
                     (mapconcat (lambda (w) (make-string w ?-))
                                col-widths "|")
                     "|")
             lines)
       (dolist (row (cdr wrapped-rows))
         (dolist (dl (my/org-table--render-multiline-row
                      ?| row col-widths nil))
           (push (concat indent dl) lines)))))

    (mapconcat #'identity (nreverse lines) "\n")))

;;;; Interactive command

;;;###autoload
(defun my/org-table-toggle-format (&optional arg)
  "Toggle the table at point between standard and box-drawing format.
Without prefix ARG, auto-detect and toggle: Format 1 becomes Format 2
and vice versa.  With prefix ARG (\\[universal-argument]), force conversion
from Format 2 to Format 1."
  (interactive "P")
  (let ((fmt (my/org-table-detect-format)))
    (unless fmt
      (user-error "Not in a table"))
    (let* ((beg (my/org-table-begin))
           (end (my/org-table-end))
           (offset (- (point) beg))
           (parsed (my/org-table-parse beg end))
           (rows (nth 1 parsed))
           (indent (nth 2 parsed))
           (target (if arg 'format-1
                     (if (eq fmt 'format-1) 'format-2 'format-1)))
           (result (pcase target
                     ('format-1 (my/org-table-render-format-1 rows indent))
                     ('format-2 (my/org-table-render-format-2 rows indent)))))
      (delete-region beg end)
      (goto-char beg)
      (insert result "\n")
      ;; Restore approximate point position
      (goto-char (min (+ beg offset) (point))))))

;;;###autoload
(defun my/org-table-resize-to-fill-column ()
  "Resize the table at point to fit within `fill-column'.
If the table already fits, display a message and do nothing.
Column widths are distributed proportionally to current content
widths.  Long unbreakable words are allowed to overflow rather
than being hard-broken."
  (interactive)
  (let ((fmt (my/org-table-detect-format)))
    (unless fmt
      (user-error "Not in a table"))
    (let* ((beg (my/org-table-begin))
           (end (my/org-table-end))
           (offset (- (point) beg))
           (parsed (my/org-table-parse beg end))
           (rows (nth 1 parsed))
           (indent (nth 2 parsed))
           (content-widths (my/org-table--compute-widths rows))
           (min-widths (my/org-table--compute-min-widths rows))
           (ncols (length content-widths))
           (indent-len (string-width indent))
           (current-width (+ indent-len (1+ ncols)
                             (apply #'+ (mapcar (lambda (w) (+ w 2))
                                                content-widths)))))
      (if (<= current-width fill-column)
          (message "Table already fits within fill-column (%d)" fill-column)
        (let ((budget (- fill-column indent-len (* 3 ncols) 1)))
          (when (< budget ncols)
            (user-error "Table cannot fit: too many columns for fill-column"))
          (let* ((new-content-widths
                  (my/org-table--compute-proportional-widths
                   content-widths budget min-widths))
                 (new-col-widths (mapcar (lambda (w) (+ w 2))
                                        new-content-widths))
                 (wrapped-rows (my/org-table--wrap-rows
                                rows new-content-widths))
                 (result (my/org-table--render-resized
                          fmt wrapped-rows new-col-widths indent))
                 (new-width (+ indent-len (1+ ncols)
                               (apply #'+ new-col-widths))))
            (delete-region beg end)
            (goto-char beg)
            (insert result "\n")
            (goto-char (min (+ beg offset) (point)))
            (when (> new-width fill-column)
              (message (concat "Table resized to %d columns: narrower would "
                               "break words (fill-column is %d)")
                       new-width fill-column))))))))

(provide 'table-format)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; table-format.el ends here
