;;;; Local extensions to Exordium: Markdown mode -*- lexical-binding: t -*-

(require 'markdown-mode)

(add-hook 'markdown-mode-hook 'flyspell-mode)
(setq markdown-hide-urls nil)
(setq markdown-max-image-size '(800 . 800))
(setq markdown-fontify-code-blocks-natively t)
(setq markdown-make-gfm-checkboxes-buttons t)

;; hide URLs + horizontal line with C-q C-l + 100 char lines
(add-hook 'markdown-mode-hook
          (lambda ()
            (page-break-lines-mode 1)
            (setq fill-column 88)  ; for Tutti
            ;;(exordium-page-break-lines-hook)
            (markdown-toggle-inline-images)))


;; Renderer:
;; markdown rendering or impatient-markdown-mode:
(when exordium-osx
  ;; (setq markdown-command "/Users/pgrenet/Tools/markup/bin/github-markup"
  ;;       markdown-command-needs-filename t)
  (setq markdown-command "/opt/homebrew/bin/multimarkdown"))

;; Utilities
(defun straighten-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))


;; Support for tables (this will be in Elpa one day)
(load-file "~/.emacs.d/taps/markdown-mode/markdown-mode-table.el")
(define-key markdown-mode-map (kbd "s-<tab>") 'markdown-cycle)

;; Table format toggle (standard <-> box-drawing) and resize
(load-file "~/.emacs.d/taps/common/table-format.el")
(define-key markdown-mode-map (kbd "C-c m T") 'my/org-table-toggle-format)
(define-key markdown-mode-map (kbd "C-c m R") 'my/org-table-resize-to-fill-column)


;; Use the Mac's built in dictionary
(when exordium-osx
  (load-file "~/.emacs.d/taps/markdown-mode/osx-dictionary.el")
  (define-key markdown-mode-map (kbd "s-$") 'osx-dictionary))


;;; Minimal Markdown -> Org converter (pure Elisp), in-place on region.
;;; Usage:
;;;   Select region (Markdown), then M-x markdown-to-org

(require 'cl-lib)

(defun markdown-to-org (&optional beg end)
  "Rewrite Markdown from region BEG..END (or whole buffer) into Org format.
If no region is active, converts the entire buffer in-place."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (md  (buffer-substring-no-properties beg end))
         (org (markdown-to-org--string md)))
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (insert org))))

(defun markdown-to-org--string (s)
  "Convert Markdown string S to Org string."
  (let* ((s        (replace-regexp-in-string "\r\n" "\n" s)) ; normalize newlines
         (lines    (split-string s "\n" nil))
         (in-fence nil)
         (out      '()))
    (cl-labels
        ((pushline (x)
           (push x out))
         (trim (x)
           (string-trim x))
         (fence-start-p (l)
           (string-match-p "^[ \t]*```" l))
         (fence-lang (l)
           (when (string-match "^[ \t]*```[ \t]*\\([^ \t]*\\)" l)
             (let ((lang (match-string 1 l)))
               (and lang (not (string-empty-p lang)) lang))))
         (heading->org (l)
           (when (string-match
                  "^[ \t]*\\(#\\{1,6\\}\\)[ \t]+\\(.*\\)$"
                  l)
             (let* ((n     (length (match-string 1 l)))
                    (title (string-trim (match-string 2 l))))
               (if (= n 1)
                   (concat "#+title: " title)
                 (concat (make-string (1- n) ?*) " " title)))))
         (hr->org (l)
           (when (string-match-p "^[ \t]*\\(-\\{3,\\}\\|\\*\\{3,\\}\\|_\\{3,\\}\\)[ \t]*$" l)
             "-----"))
         (blockquote->org (l)
           (when (string-match "^[ \t]*> ?\\(.*\\)$" l)
             ;; Lightweight quote style; change to begin_quote/end_quote if desired.
             (concat ": " (match-string 1 l))))
         (ul-item-p (l)
           (string-match "^[ \t]*[-+*][ \t]+\\(.*\\)$" l))
         (ol-item-p (l)
           (string-match "^[ \t]*[0-9]+\\.[ \t]+\\(.*\\)$" l))
         (ul-item->org (l)
           (when (ul-item-p l) (concat "- " (match-string 1 l))))
         (ol-item->org (l)
           (when (ol-item-p l) (concat "1. " (match-string 1 l))))
         (strip-backslash-escapes (x)
           (replace-regexp-in-string "\\\\\\([`*_{}\\[\\]()#+.!-]\\)" "\\1" x))
         (convert-links (x)
           ;; [text](url) -> [[url][text]], ![alt](url) -> [[url]]
           (let ((start 0))
             (while (string-match "\\(\\(?:!\\)?\\)\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))" x start)
               (let* ((bang (match-string 1 x))
                      (text (match-string 2 x))
                      (url  (match-string 3 x))
                      (rep  (if (and bang (string= bang "!"))
                                (format "[[%s]]" (string-trim url))
                              (format "[[%s][%s]]" (string-trim url) (string-trim text)))))
                 (setq x (replace-match rep t t x))
                 (setq start (+ (match-beginning 0) (length rep)))))
             x))
         (convert-inline-code (x)
           ;; `code` -> `code`. Normally it is ~code~ but I have special org support.
           (let ((start 0))
             (while (string-match "`\\([^`\n]+\\)`" x start)
               (let* ((code (match-string 1 x))
                      (rep (concat "`" code "`")))
                 (setq x (replace-match rep t t x))
                 (setq start (+ (match-beginning 0) (length rep)))))
             x))
         (convert-strike (x)
           ;; ~~del~~ -> +del+
           (replace-regexp-in-string "~~\\([^~\n]+\\)~~" "+\\1+" x))
         (convert-emphasis (x)
           ;; **b**/__b__ -> *b* ; *i*/_i_ -> /i/
           ;; Simplistic; won't handle all edge cases.
           ;; Italic first: convert *i* (but not **b**) and _i_ (but not __b__)
           (setq x (replace-regexp-in-string "\\(^\\|[^*_]\\)\\*\\([^*\n]+\\)\\*\\([^*]\\|$\\)" "\\1/\\2/\\3" x))
           (setq x (replace-regexp-in-string "\\(^\\|[^_]\\)_\\([^_\n]+\\)_\\([^_]\\|$\\)" "\\1/\\2/\\3" x))
           ;; Bold: convert **b** and __b__ to *b*
           (setq x (replace-regexp-in-string "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" x))
           (setq x (replace-regexp-in-string "__\\([^_\n]+\\)__" "*\\1*" x))
           x)
         (convert-line (l)
           (let ((x l))
             (setq x (strip-backslash-escapes x))
             (setq x (convert-links x))
             (setq x (convert-inline-code x))
             (setq x (convert-strike x))
             (setq x (convert-emphasis x))
             x)))
      (dolist (l lines)
        (cond
         ((fence-start-p l)
          (if in-fence
              (progn (pushline "#+end_src") (setq in-fence nil))
            (pushline (format "#+begin_src %s" (or (fence-lang l) "")))
            (setq in-fence t)))
         (in-fence
          (pushline l))
         ((heading->org l)
          (pushline (heading->org l)))
         ((hr->org l)
          (pushline (hr->org l)))
         ((blockquote->org l)
          (pushline (convert-line (blockquote->org l))))
         ((ul-item-p l)
          (pushline (convert-line (ul-item->org l))))
         ((ol-item-p l)
          (pushline (convert-line (ol-item->org l))))
         ((string-match-p "^[ \t]*$" l)
          (pushline ""))
         (t
          (pushline (convert-line l))))))
    (when in-fence
      (push "#+end_src" out))
    (mapconcat #'identity (nreverse out) "\n")))


;; == Snippets ==
;; (add-hook 'markdown-mode-hook
;;           '(lambda ()
;;              (yas-minor-mode)))
;; (define-key markdown-mode-map (kbd "C-c y") 'yas-expand)
;; (define-key markdown-mode-map (kbd "<f2>") 'yas-expand)


;; == flymd ==
;; (require 'flymd)
;; (defun my-flymd-browser-function (url)
;;   (let ((process-environment (browse-url-process-environment)))
;;     (apply 'start-process
;;            (concat "google-chrome " url) nil
;;            "/usr/bin/open"
;;            (list "google-chrome" "--new-window" "--allow-file-access-from-files" url))))

;; (setq flymd-browser-open-function 'my-flymd-browser-function)
