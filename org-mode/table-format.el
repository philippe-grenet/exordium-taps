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

(provide 'table-format)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; table-format.el ends here
