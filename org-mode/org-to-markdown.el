;;; org-to-markdown.el --- Minimal Org -> Markdown converter -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Minimal Org -> Markdown converter (pure Elisp), in-place on region.
;; Usage:
;;   Select region (Org), then M-x org-to-markdown

;;; Code:

(require 'cl-lib)

(defun org-to-markdown (&optional beg end)
  "Rewrite Org from region BEG..END (or whole buffer) into Markdown format.
If no region is active, converts the entire buffer in-place."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (org (buffer-substring-no-properties beg end))
         (md  (org-to-markdown--string org)))
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (insert md))))

(defun org-to-markdown--string (s)
  "Convert Org string S to Markdown string."
  (let* ((s        (replace-regexp-in-string "\r\n" "\n" s)) ; normalize newlines
         (lines    (split-string s "\n" nil))
         (in-src   nil)
         (after-results nil)
         (out      '()))
    (cl-labels
        ((pushline (x)
           (push x out))
         (trim (x)
           (string-trim x))
         (src-begin-p (l)
           (string-match-p "^[ \t]*#\\+begin_src" l))
         (src-end-p (l)
           (string-match-p "^[ \t]*#\\+end_src" l))
         (src-lang (l)
           (when (string-match "^[ \t]*#\\+begin_src[ \t]*\\(.*\\)$" l)
             (let ((lang (string-trim (match-string 1 l))))
               (and (not (string-empty-p lang)) lang))))
         (startup-p (l)
           (string-match-p "^[ \t]*#\\+startup:" l))
         (results-p (l)
           (string-match-p "^[ \t]*#\\+RESULTS:" l))
         (babel-image-p (l)
           (string-match-p "^\\[\\[file:" l))
         (title->md (l)
           (when (string-match "^[ \t]*#\\+title:[ \t]*\\(.*\\)$" l)
             (concat "# " (string-trim (match-string 1 l)))))
         (heading->md (l)
           (when (string-match "^\\(\\*+\\)[ \t]+\\(.*\\)$" l)
             (let* ((n     (length (match-string 1 l)))
                    (title (string-trim (match-string 2 l))))
               (concat (make-string (1+ n) ?#) " " title))))
         (hr->md (l)
           (when (string-match-p "^[ \t]*-\\{5,\\}[ \t]*$" l)
             "---"))
         (blockquote->md (l)
           (when (string-match "^: \\(.*\\)$" l)
             (concat "> " (match-string 1 l))))
         (ul-item-p (l)
           (string-match "^[ \t]*-[ \t]+\\(.*\\)$" l))
         (ol-item-p (l)
           (string-match "^[ \t]*[0-9]+\\.[ \t]+\\(.*\\)$" l))
         (convert-links (x)
           ;; [[url][text]] -> [text](url), [[url]] -> ![](url)
           (let ((start 0))
             ;; First convert [[url][text]] links
             (while (string-match "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" x start)
               (let* ((url  (match-string 1 x))
                      (text (match-string 2 x))
                      (rep  (format "[%s](%s)" (string-trim text) (string-trim url))))
                 (setq x (replace-match rep t t x))
                 (setq start (+ (match-beginning 0) (length rep))))))
           (let ((start 0))
             ;; Then convert [[url]] bare links (images in the original conversion)
             (while (string-match "\\[\\[\\([^]]+\\)\\]\\]" x start)
               (let* ((url (match-string 1 x))
                      (rep (format "![](%s)" (string-trim url))))
                 (setq x (replace-match rep t t x))
                 (setq start (+ (match-beginning 0) (length rep))))))
           x)
         (convert-inline-code (x)
           ;; `code` stays as `code` (same in both formats)
           x)
         (convert-strike (x)
           ;; +del+ -> ~~del~~
           (replace-regexp-in-string "\\+\\([^+\n]+\\)\\+" "~~\\1~~" x))
         (convert-emphasis (x)
           ;; *b* -> **b** ; /i/ -> *i*
           ;; Bold first so that italic conversion doesn't interfere.
           ;; Bold: *b* -> **b** (but not already **)
           (setq x (replace-regexp-in-string "\\(^\\|[^*]\\)\\*\\([^*\n]+\\)\\*\\([^*]\\|$\\)" "\\1**\\2**\\3" x))
           ;; Italic: /i/ -> *i*
           (setq x (replace-regexp-in-string "/\\([^/\n]+\\)/" "*\\1*" x))
           x)
         (convert-line (l)
           (let ((x l))
             (setq x (convert-links x))
             (setq x (convert-inline-code x))
             (setq x (convert-strike x))
             (setq x (convert-emphasis x))
             x)))
      (dolist (l lines)
        (cond
         ((src-begin-p l)
          (pushline (format "```%s" (or (src-lang l) "")))
          (setq in-src t))
         ((and in-src (src-end-p l))
          (pushline "```")
          (setq in-src nil))
         (in-src
          (pushline l))
         (after-results
          (setq after-results nil)
          (unless (babel-image-p l)
            ;; Not a babel image line; process normally
            (pushline (convert-line l))))
         ((startup-p l)
          nil) ; drop #+startup: lines (org-only)
         ((results-p l)
          (setq after-results t)) ; drop #+RESULTS: and the following image line
         ((title->md l)
          (pushline (title->md l)))
         ((heading->md l)
          (pushline (heading->md l)))
         ((hr->md l)
          (pushline (hr->md l)))
         ((blockquote->md l)
          (pushline (convert-line (blockquote->md l))))
         ((ul-item-p l)
          (pushline (convert-line l)))
         ((ol-item-p l)
          (pushline (convert-line l)))
         ((string-match-p "^[ \t]*$" l)
          (pushline ""))
         (t
          (pushline (convert-line l))))))
    (when in-src
      (push "```" out))
    (mapconcat #'identity (nreverse out) "\n")))

(provide 'org-to-markdown)

;;; org-to-markdown.el ends here
