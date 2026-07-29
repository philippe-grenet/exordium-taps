;;; description-field.el --- Insert/update a one-line file description -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `my/update-description' inserts or updates a one-line file description in the
;; current buffer, choosing the field format based on the major mode.  This is
;; meant for a knowledge-base ("second brain") repo whose index.md is generated
;; by an LLM-wiki indexer: declaring the description in the file itself lets the
;; indexer use it verbatim instead of inventing one.
;;
;; Two formats are supported:
;;
;;   - Org mode: an in-buffer keyword line near the top, before the first
;;     headline, alongside #+title: and friends:
;;
;;         #+description: One factual line.
;;
;;   - Markdown mode (markdown-mode / gfm-mode): a `description:' key inside the
;;     YAML frontmatter, the `---'-fenced block that must be the very first
;;     thing in the file:
;;
;;         ---
;;         description: "One line.  Quoted when it contains a colon."
;;         ---
;;
;; Any other major mode is a no-op (signals a `user-error').
;;
;; Bound to C-c m # (markdown) and C-c o # (org) by the respective taps.

;;; Code:

(require 'subr-x)

;;;; Org

(defconst my/description-org-regexp
  "^[ \t]*#\\+description:[ \t]*\\(.*\\)$"
  "Regexp matching an Org `#+description:' keyword line.
Match group 1 is the value.  Use with `case-fold-search' bound to
t so that `#+DESCRIPTION:' is matched as well.")

(defun my/description--org-first-headline ()
  "Return the position of the first Org headline, or `point-max'.
Org keywords only count when they appear before any headline."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\*+[ \t]" nil t)
        (match-beginning 0)
      (point-max))))

(defun my/description--org-get ()
  "Return the current Org `#+description:' value, trimmed, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (limit (my/description--org-first-headline)))
      (when (re-search-forward my/description-org-regexp limit t)
        (string-trim (match-string 1))))))

(defun my/description--org-set (value)
  "Insert or update the Org `#+description:' keyword to VALUE."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (limit (my/description--org-first-headline)))
      (if (re-search-forward my/description-org-regexp limit t)
          ;; Update in place: replace the value part of the existing line.
          (replace-match value t t nil 1)
        ;; Insert a new keyword line in the top keyword block.
        (goto-char (point-min))
        (cond
         ;; After an existing #+title: line.
         ((re-search-forward "^[ \t]*#\\+title:.*$" limit t)
          (end-of-line)
          (insert "\n#+description: " value))
         ;; After the last contiguous leading #+keyword: line.
         ((looking-at-p "^[ \t]*#\\+[a-zA-Z_]+:")
          (while (looking-at-p "^[ \t]*#\\+[a-zA-Z_]+:")
            (forward-line 1))
          (insert "#+description: " value "\n"))
         ;; No keyword block at all: put it at the very top.
         (t
          (insert "#+description: " value "\n")))))))

;;;; Markdown

(defun my/description--yaml-quote (value)
  "Return VALUE ready to appear after a YAML `description:' key.
Wrap in double quotes (escaping embedded backslashes and double
quotes) when VALUE contains a colon or another character that
would make bare YAML ambiguous; otherwise return it unquoted."
  (if (string-match-p "[][:#\"'{},&*!|>%@`]" value)
      (concat "\""
              (replace-regexp-in-string
               "\\([\"\\]\\)" "\\\\\\1" value)
              "\"")
    value))

(defun my/description--yaml-unquote (value)
  "Return the plain text of a YAML scalar VALUE.
Strip surrounding double quotes and unescape \\\" and \\\\ when
present; otherwise return VALUE trimmed."
  (let ((value (string-trim value)))
    (if (and (> (length value) 1)
             (string-prefix-p "\"" value)
             (string-suffix-p "\"" value))
        (replace-regexp-in-string
         "\\\\\\([\"\\]\\)" "\\1"
         (substring value 1 (1- (length value))))
      value)))

(defun my/description--markdown-frontmatter ()
  "Return the bounds of the YAML frontmatter, or nil if absent.
The value is a list (OPEN-END CLOSE-BEG), where OPEN-END is the
position just after the opening `---' line (start of the block
body) and CLOSE-BEG is the position at the start of the closing
`---' (or `...') line.  Frontmatter must start on line 1."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "^---[ \t]*$")
      (forward-line 1)
      (let ((open-end (point)))
        (when (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" nil t)
          (list open-end (match-beginning 0)))))))

(defun my/description--markdown-get ()
  "Return the current Markdown frontmatter `description:' value, or nil."
  (let ((bounds (my/description--markdown-frontmatter)))
    (when bounds
      (save-excursion
        (goto-char (nth 0 bounds))
        (when (re-search-forward "^[ \t]*description:[ \t]*\\(.*\\)$"
                                 (nth 1 bounds) t)
          (my/description--yaml-unquote (match-string 1)))))))

(defun my/description--markdown-set (value)
  "Insert or update the Markdown frontmatter `description:' to VALUE."
  (let ((bounds (my/description--markdown-frontmatter))
        (yaml (my/description--yaml-quote value)))
    (if bounds
        (save-excursion
          (goto-char (nth 0 bounds))
          (if (re-search-forward "^[ \t]*description:[ \t]*\\(.*\\)$"
                                 (nth 1 bounds) t)
              ;; Update the existing key in place.
              (replace-match yaml t t nil 1)
            ;; Add the key right after the opening `---'.
            (goto-char (nth 0 bounds))
            (insert "description: " yaml "\n")))
      ;; No frontmatter: create a new block at the very top.
      (save-excursion
        (goto-char (point-min))
        (insert "---\ndescription: " yaml "\n---\n\n")))))

;;;; Command

(defun my/description--current-value ()
  "Return the current file description for the buffer's major mode.
Signal a `user-error' for unsupported modes."
  (cond
   ((derived-mode-p 'org-mode) (my/description--org-get))
   ((derived-mode-p 'markdown-mode) (my/description--markdown-get))
   (t (user-error
       "`my/update-description' supports only Org and Markdown buffers"))))

;;;###autoload
(defun my/update-description (description)
  "Insert or update the one-line file DESCRIPTION in the current buffer.
In Org buffers this maintains a `#+description:' keyword; in
Markdown buffers a `description:' key in the YAML frontmatter.
When updating, the prompt is pre-filled with the existing value."
  (interactive
   (list (read-string "Description: " (my/description--current-value))))
  (let ((description (string-trim description)))
    (cond
     ((derived-mode-p 'org-mode) (my/description--org-set description))
     ((derived-mode-p 'markdown-mode) (my/description--markdown-set description))
     (t (user-error
         "`my/update-description' supports only Org and Markdown buffers")))))

(provide 'description-field)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; description-field.el ends here
