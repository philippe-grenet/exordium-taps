;;;; Inline Mermaid rendering for Markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Render ```mermaid fenced code blocks as images shown in overlays right
;;; below each block.  This is display-only: the Markdown file is never
;;; modified (no image links are inserted, no PNGs are written next to the
;;; notes).  It mirrors the in-buffer experience of `ob-mermaid' in Org.
;;;
;;; Try it with:  M-x load-file  on this file, then in a Markdown buffer:
;;;   C-c m m   render / refresh all mermaid blocks
;;;   C-c m M   clear the rendered diagrams
;;;
;;; Code:

(require 'markdown-mode)
(require 'cl-lib)
(require 'subr-x)

(defvar my/markdown-mermaid-cli
  (or (executable-find "mmdc") "/opt/homebrew/bin/mmdc")
  "Path to the Mermaid CLI (mmdc).")

(defvar my/markdown-mermaid-chrome
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  "Chrome/Chromium binary for Puppeteer, or nil to leave the env untouched.")

(defvar my/markdown-mermaid-theme "dark"
  "Mermaid theme: one of \"default\", \"forest\", \"dark\", \"neutral\".")

(defvar my/markdown-mermaid-background "transparent"
  "Background color passed to mmdc, e.g. \"transparent\" or \"white\".")

(defvar my/markdown-mermaid-width 1800
  "Render width in pixels passed to mmdc (higher = sharper).")

(defvar my/markdown-mermaid-display-width 900
  "Displayed image width in pixels in the buffer.")

(defvar my/markdown-mermaid-cache-dir
  (expand-file-name "md-mermaid/" temporary-file-directory)
  "Directory for cached rendered PNGs.  Keyed on block content + options.")

(defvar-local my/markdown-mermaid-overlays nil
  "Overlays showing rendered mermaid diagrams in this buffer.")

(defun my/markdown-mermaid--render (code)
  "Render mermaid CODE to a PNG and return its path.
Results are cached by a hash of CODE and the render options, so unchanged
blocks are not re-rendered."
  (unless (file-directory-p my/markdown-mermaid-cache-dir)
    (make-directory my/markdown-mermaid-cache-dir t))
  (let* ((key (secure-hash 'sha1
                           (format "%s|%s|%s|%s"
                                   code
                                   my/markdown-mermaid-theme
                                   my/markdown-mermaid-background
                                   my/markdown-mermaid-width)))
         (png (expand-file-name (concat key ".png") my/markdown-mermaid-cache-dir))
         (mmd (expand-file-name (concat key ".mmd") my/markdown-mermaid-cache-dir)))
    (unless (file-exists-p png)
      (with-temp-file mmd (insert code))
      (let ((process-environment
             (if my/markdown-mermaid-chrome
                 (cons (concat "PUPPETEER_EXECUTABLE_PATH=" my/markdown-mermaid-chrome)
                       process-environment)
               process-environment)))
        (unless (zerop (call-process
                        my/markdown-mermaid-cli nil
                        (get-buffer-create "*markdown-mermaid*") nil
                        "-i" mmd
                        "-o" png
                        "-t" my/markdown-mermaid-theme
                        "-b" my/markdown-mermaid-background
                        "-w" (number-to-string my/markdown-mermaid-width)))
          (error "mmdc failed; see the *markdown-mermaid* buffer"))))
    png))

(defun my/markdown-mermaid--blocks ()
  "Return a list of (CODE-END . CODE) for each mermaid fenced block.
CODE-END is the buffer position at the end of the closing fence line."
  (let ((blocks '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*```[ \t]*mermaid[ \t]*$" nil t)
        (forward-line 1)
        (let ((code-beg (point)))
          (when (re-search-forward "^[ \t]*```[ \t]*$" nil t)
            (let ((code (string-trim
                         (buffer-substring-no-properties code-beg
                                                         (match-beginning 0)))))
              (push (cons (match-end 0) code) blocks))))))
    (nreverse blocks)))

(defun my/markdown-mermaid--make-overlay (pos png)
  "Show image PNG in an overlay placed after buffer position POS."
  (let ((image (create-image png 'png nil
                             :max-width my/markdown-mermaid-display-width))
        (ov (make-overlay pos pos)))
    (overlay-put ov 'my/markdown-mermaid t)
    (overlay-put ov 'after-string
                 (concat "\n" (propertize " " 'display image 'rear-nonsticky t) "\n"))
    (push ov my/markdown-mermaid-overlays)))

(defun my/markdown-mermaid-clear ()
  "Remove all inline mermaid diagram overlays in the current buffer."
  (interactive)
  (mapc #'delete-overlay my/markdown-mermaid-overlays)
  (setq my/markdown-mermaid-overlays nil))

(defun my/markdown-mermaid-render-buffer ()
  "Render all mermaid code blocks in the buffer as inline images."
  (interactive)
  (my/markdown-mermaid-clear)
  (let ((n 0))
    (dolist (block (my/markdown-mermaid--blocks))
      (let ((end (car block))
            (code (cdr block)))
        (unless (string-empty-p code)
          (condition-case err
              (progn
                (my/markdown-mermaid--make-overlay end (my/markdown-mermaid--render code))
                (setq n (1+ n)))
            (error (message "Mermaid render error: %s"
                            (error-message-string err)))))))
    (message "Rendered %d mermaid block(s)" n)))

(defun my/markdown-mermaid-toggle ()
  "Toggle inline mermaid rendering in the current buffer."
  (interactive)
  (if my/markdown-mermaid-overlays
      (my/markdown-mermaid-clear)
    (my/markdown-mermaid-render-buffer)))

(define-key markdown-mode-map (kbd "C-c m m") #'my/markdown-mermaid-render-buffer)
(define-key markdown-mode-map (kbd "C-c m M") #'my/markdown-mermaid-clear)

(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here
