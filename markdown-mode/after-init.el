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


;; Support for tables (this will be in Elpa one day)
(load-file "~/.emacs.d/taps/markdown-mode/markdown-mode-table.el")
(define-key markdown-mode-map (kbd "s-<tab>") 'markdown-cycle)

;; Table format toggle (standard <-> box-drawing) and resize
(load-file "~/.emacs.d/taps/common/table-format.el")
(define-key markdown-mode-map (kbd "C-c m T") 'my/org-table-toggle-format)
(define-key markdown-mode-map (kbd "C-c m R") 'my/org-table-resize-to-fill-column)

;; C-c m #: Insert/update the file description (YAML frontmatter `description:')
(load-file "~/.emacs.d/taps/common/description-field.el")
(define-key markdown-mode-map (kbd "C-c m #") #'my/update-description)

;; C-c m i: Insert a Markdown image link, defaulting to the `img/' subdirectory.
(defun my/markdown-insert-image ()
  "Insert a Markdown image link, prompting for the file under `img/'.
Point is left inside the alt-text brackets."
  (interactive)
  (let ((file (read-file-name "Image: " nil nil nil "img/")))
    (insert (format "![](%s)" file))
    (search-backward "](")))

(define-key markdown-mode-map (kbd "C-c m i") #'my/markdown-insert-image)


;; Use the Mac's built in dictionary
(when exordium-osx
  (load-file "~/.emacs.d/taps/markdown-mode/osx-dictionary.el")
  (define-key markdown-mode-map (kbd "s-$") 'osx-dictionary))


;; Inline Mermaid rendering (C-c m m / C-c m M)
(load-file "~/.emacs.d/taps/markdown-mode/markdown-mermaid.el")


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


;; Rendering

;;; Markdown rendering using pandoc (another option is multimarkdown)
(defvar my/markdown-preview-theme 'dark
  "Theme for pandoc live preview: `dark' (Mocha) or `light' (Latte).")

(defun my/markdown-set-preview-command ()
  "Set `markdown-command' according to `my/markdown-preview-theme'."
  (let ((header (if (eq my/markdown-preview-theme 'light)
                    "~/.emacs.d/taps/markdown-mode/pandoc-light.html"
                  "~/.emacs.d/taps/markdown-mode/pandoc-mocha.html")))
    (setq markdown-command
          (concat "/opt/homebrew/bin/pandoc --standalone --mathml"
                  " --include-in-header=" (expand-file-name header)))))

(my/markdown-set-preview-command)

(defun my/markdown-toggle-preview-theme ()
  "Toggle the live preview between dark and light, then re-render."
  (interactive)
  (setq my/markdown-preview-theme
        (if (eq my/markdown-preview-theme 'light) 'dark 'light))
  (my/markdown-set-preview-command)
  (when (bound-and-true-p markdown-live-preview-mode)
    (markdown-live-preview-export))
  (message "Markdown preview theme: %s" my/markdown-preview-theme))

(define-key markdown-mode-map (kbd "C-c m t") 'my/markdown-toggle-preview-theme)

;; C-c m v: toggle the live preview (mirrors C-c o v in org-mode).
(define-key markdown-mode-map (kbd "C-c m v") 'markdown-live-preview-mode)

(defvar-local my/markdown-preview-export-file nil
  "Exported HTML file backing this xwidget preview buffer.")

(defvar my/markdown-preview-xwidget nil
  "Xwidget session used by the markdown live preview.")

(defvar my/markdown-preview--in-teardown nil
  "Non-nil while tearing down a preview, to prevent re-entrant kills.")

(defun my/markdown-preview-teardown (source xwidget file)
  "Tear down a markdown live preview.
Delete FILE, kill the XWIDGET preview buffer, and turn off live
preview in the SOURCE buffer (SOURCE itself is never killed).
Safe to call from the `kill-buffer-hook' of either buffer: the
buffer currently being killed is left alone and re-entrant calls
are ignored, so killing the source and killing the xwidget behave
the same."
  (unless my/markdown-preview--in-teardown
    (let ((my/markdown-preview--in-teardown t)
          (kill-buffer-query-functions nil))
      (when (and file (file-exists-p file))
        (delete-file file))
      (when (buffer-live-p source)
        (with-current-buffer source
          (setq markdown-live-preview-buffer nil) ; avoid re-killing xwidget
          (when (and (bound-and-true-p markdown-live-preview-mode)
                     (not (eq source (current-buffer))))
            (markdown-live-preview-mode -1))))
      (when (and (buffer-live-p xwidget)
                 (not (eq xwidget (current-buffer))))
        (kill-buffer xwidget)))))

(defun my/markdown-preview--on-preview-kill ()
  "Tear down the preview when the preview buffer is killed."
  (my/markdown-preview-teardown
   markdown-live-preview-source-buffer
   (current-buffer)
   my/markdown-preview-export-file))

(defun my/markdown-preview--on-source-kill ()
  "Tear down the preview when the markdown source buffer is killed."
  (my/markdown-preview-teardown
   (current-buffer)
   markdown-live-preview-buffer
   (and (buffer-live-p markdown-live-preview-buffer)
        (buffer-local-value 'my/markdown-preview-export-file
                            markdown-live-preview-buffer))))

(defun my/markdown-preview-window-xwidget (file)
  "Preview FILE with xwidget browser"
  (xwidget-webkit-browse-url (concat "file://" file))
  (setq my/markdown-preview-xwidget (xwidget-webkit-current-session))
  (let ((buf (xwidget-buffer my/markdown-preview-xwidget)))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (with-current-buffer buf
        (setq-local my/markdown-preview-export-file file)
        (add-hook 'kill-buffer-hook #'my/markdown-preview--on-preview-kill nil t))
      (pop-to-buffer buf))))


;;; Preview in the OS default web browser instead of the xwidget.

(defvar my/markdown-preview-target 'xwidget
  "Where `markdown-live-preview-mode' displays the exported HTML.
`xwidget' uses the embedded WebKit browser, `browser' uses the OS
default web browser via `browse-url'.  Toggle with \\[my/markdown-toggle-preview-target].")

(defvar my/markdown-preview-browser-reopen-on-export nil
  "When non-nil, re-open the OS browser after every re-export.
The default is nil: the browser is opened once when the preview
starts, and later saves only rewrite the HTML file, so the page is
updated by refreshing the browser.  Set to t to have every save
hand the file back to the browser (which usually means a new tab).")

(defun my/markdown-preview--on-browser-kill ()
  "Tear down the preview when the browser bookkeeping buffer is killed.
Also turns off `markdown-live-preview-mode' in the source buffer, so
that saving it does not open the browser again."
  (let ((source markdown-live-preview-source-buffer))
    (my/markdown-preview--on-preview-kill)
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (bound-and-true-p markdown-live-preview-mode)
          (markdown-live-preview-mode -1))))))

(defun my/markdown-preview-window-browser (file)
  "Preview FILE in the OS default web browser.
Return the bookkeeping buffer that `markdown-live-preview-mode'
uses to track the preview: killing it (or killing the source
buffer) stops the preview and deletes FILE, mirroring the xwidget
preview."
  (let* ((existing (and (buffer-live-p markdown-live-preview-buffer)
                        markdown-live-preview-buffer))
         (buf (or existing (get-buffer-create "*markdown-preview (browser)*"))))
    (when (or (null existing) my/markdown-preview-browser-reopen-on-export)
      (browse-url (concat "file://" file)))
    (with-current-buffer buf
      (setq-local my/markdown-preview-export-file file)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Markdown live preview in the OS default web browser.\n\n"
                "HTML file: " file "\n\n"
                "Saving the Markdown buffer re-renders this file; refresh the\n"
                "browser page to see the changes.  Kill this buffer to stop the\n"
                "preview and delete the HTML file.\n"))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (unless existing
        (add-hook 'kill-buffer-hook #'my/markdown-preview--on-browser-kill nil t)))
    buf))

(defun my/markdown-preview-window (file)
  "Preview FILE according to `my/markdown-preview-target'."
  (if (eq my/markdown-preview-target 'browser)
      (my/markdown-preview-window-browser file)
    (my/markdown-preview-window-xwidget file)))

(setq markdown-live-preview-window-function #'my/markdown-preview-window)

(defun my/markdown-toggle-preview-target ()
  "Toggle the live preview between the xwidget and the OS web browser.
When a live preview is running, it is restarted in the new target."
  (interactive)
  (setq my/markdown-preview-target
        (if (eq my/markdown-preview-target 'browser) 'xwidget 'browser))
  (when (bound-and-true-p markdown-live-preview-mode)
    (markdown-live-preview-mode -1)
    (markdown-live-preview-mode 1))
  (message "Markdown preview target: %s" my/markdown-preview-target))

(define-key markdown-mode-map (kbd "C-c m B") #'my/markdown-toggle-preview-target)

(defun my/markdown-preview-in-browser ()
  "Render this buffer with pandoc and open it in the OS default web browser.
This is a one-shot export, independent of `markdown-live-preview-mode':
the HTML file is written next to the Markdown file (so relative image
links keep working) and is left in place."
  (interactive)
  (let ((filename (markdown-live-preview-get-filename)))
    (unless filename
      (user-error "Buffer %s does not visit a file" (buffer-name)))
    (let ((file (markdown-export filename)))
      (browse-url (concat "file://" file))
      (message "Opened %s in the default browser" file))))

(define-key markdown-mode-map (kbd "C-c m b") #'my/markdown-preview-in-browser)


;;; Scroll sync: keep the preview centered near point in the source buffer.
(defvar my/markdown-preview-sync-scroll-enabled t
  "When non-nil, scroll the xwidget preview to track point in the source.")

(defvar-local my/markdown-preview-last-line nil
  "Last source line synced to the preview, to avoid redundant scrolls.")

(defun my/markdown-preview-sync-scroll ()
  "Scroll the xwidget preview to match point in the markdown source.
Uses proportional scroll (point's line / total lines), which is
approximate but tracks well in practice.  Does nothing when
`my/markdown-preview-sync-scroll-enabled' is nil, or when the
preview target is not the xwidget."
  (when (and my/markdown-preview-sync-scroll-enabled
             (eq my/markdown-preview-target 'xwidget)
             (bound-and-true-p markdown-live-preview-mode)
             my/markdown-preview-xwidget
             (xwidget-live-p my/markdown-preview-xwidget))
    (let ((line (line-number-at-pos)))
      (unless (eq line my/markdown-preview-last-line)
        (setq my/markdown-preview-last-line line)
        (let ((ratio (/ (float line)
                        (max 1 (line-number-at-pos (point-max))))))
          (xwidget-webkit-execute-script
           my/markdown-preview-xwidget
           (format "window.scrollTo(0, (document.body.scrollHeight - window.innerHeight) * %f);"
                   ratio)))))))

(add-hook 'markdown-live-preview-mode-hook
          (lambda ()
            (if markdown-live-preview-mode
                (progn
                  (add-hook 'post-command-hook
                            #'my/markdown-preview-sync-scroll nil t)
                  (add-hook 'kill-buffer-hook
                            #'my/markdown-preview--on-source-kill nil t))
              (remove-hook 'post-command-hook
                           #'my/markdown-preview-sync-scroll t)
              (remove-hook 'kill-buffer-hook
                           #'my/markdown-preview--on-source-kill t))))


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
