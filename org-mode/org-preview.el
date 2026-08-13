;;;; Live HTML preview for Org mode -*- lexical-binding: t -*-
;;;
;;; Renders the current Org buffer to HTML in an xwidget webkit window,
;;; mirroring the Markdown tap's live preview.  Built on Org's own `ox-html'
;;; exporter -- no extra package needed.  It reuses the SAME Catppuccin CSS
;;; files as the Markdown preview (pandoc-mocha.html / pandoc-light.html) so
;;; both renderings look identical.
;;;
;;;   C-c o v : toggle the live preview (re-renders on save).
;;;             With a prefix (C-u C-c o v), include a table of contents.
;;;   C-c o V : toggle preview theme (Mocha <-> Latte) and re-render.

(require 'org)
(require 'ox-html)

(defvar my/org-preview-theme 'dark
  "Theme for the Org HTML preview: `dark' (Mocha) or `light' (Latte).")

(defvar my/org-preview-with-toc nil
  "When non-nil, include a table of contents in the Org preview.
Set for the session by calling the preview command with a prefix arg.")

(defvar my/org-preview-css-dir "~/.emacs.d/taps/markdown-mode/"
  "Directory holding the shared `pandoc-light.html'/`pandoc-mocha.html' styles.")

(defvar my/org-preview-directory "/tmp/"
  "Directory where preview HTML files are written.
`/tmp' is conventional and transient (the OS prunes it); files are also
deleted when the preview is turned off or the buffer is killed.")

(defun my/org-preview--pandoc-css ()
  "Return the shared pandoc `<style>' block for the current theme.
This is the SAME file used by the Markdown preview, so colors match."
  (let ((file (expand-file-name
               (if (eq my/org-preview-theme 'light)
                   "pandoc-light.html"
                 "pandoc-mocha.html")
               my/org-preview-css-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun my/org-preview--structure-css ()
  "Return an Org-specific `<style>' block that restores the outline tree.
The shared pandoc CSS only sets colors, so headings fall back to browser
default sizes (which are flat past `h4').  This adds stepped heading sizes,
indents nested `.outline-N' sections to show depth, and styles TODO/tag
badges with theme-appropriate accents."
  (let* ((light (eq my/org-preview-theme 'light))
         (base    (if light "#eff1f5" "#1e1e2e"))
         (surface (if light "#ccd0da" "#313244"))
         (red     (if light "#d20f39" "#f38ba8"))
         (green   (if light "#40a02b" "#a6e3a1"))
         (yellow  (if light "#df8e1d" "#f9e2af"))
         (blue    (if light "#1e66f5" "#89b4fa"))
         (maroon  (if light "#e64553" "#eba0ac"))
         (gray    (if light "#9ca0b0" "#6c7086")) ; overlay0
         (text    (if light "#4c4f69" "#cdd6f4"))
         (muted   (if light "#6c6f85" "#9399b2"))
         (sky     (if light "#04a5e5" "#89dceb"))
         (bullet  (if light "#8839ef" "#cba6f7"))
         ;; Keyword -> accent, resolved from the faces used in `svg-tag-tags'.
         ;; Filled/inverse pills mimic svg-tag-mode's :inverse t in the buffer.
         (keyword-colors
          `(("TODO"    . ,red)     ; font-lock-warning-face
            ("WORK"    . ,yellow)  ; font-lock-type-face
            ("WAIT"    . ,blue)    ; font-lock-function-name-face
            ("STOP"    . ,gray)    ; font-lock-comment-face (shadow/overlay0)
            ("BLOCKED" . ,maroon)  ; dired-flagged
            ("READY"   . ,blue)    ; font-lock-function-name-face
            ("REVIEW"  . ,text)    ; font-lock-variable-name-face
            ("DONE"    . ,green))) ; font-lock-string-face
         (keyword-css
          (mapconcat
           (lambda (kc)
             (format "  .%s { background: %s; color: %s; }\n"
                     (car kc) (cdr kc) base))
           keyword-colors "")))
    (concat
     (format "<style>
  /* Org maps *->h2, **->h3, ***->h4, ...  Give each level a distinct size. */
  h1.title { font-size: 1.9rem; text-align: left; margin-bottom: 1.2rem; }
  h2 { font-size: 1.55rem; margin-top: 1.8rem; }
  h3 { font-size: 1.3rem;  margin-top: 1.4rem; }
  h4 { font-size: 1.12rem; margin-top: 1.15rem; }
  h5 { font-size: 1.0rem;  margin-top: 1.0rem; font-weight: 700; }
  h6 { font-size: 0.9rem;  margin-top: 1.0rem; font-weight: 700;
       text-transform: uppercase; letter-spacing: 0.03em; }
  /* Indent nested sections; margins compound with depth => tree structure. */
  .outline-3, .outline-4, .outline-5, .outline-6 { margin-left: 1.1rem; }
  /* Heading bullets, mimicking org-modern/org-superstar in the buffer. */
  h2::before, h3::before, h4::before, h5::before, h6::before {
    color: %s; font-weight: 700; margin-right: 0.5rem; }
  h2::before { content: \"\\25C9\"; }  /* filled circle */
  h3::before { content: \"\\25CB\"; }  /* open circle */
  h4::before { content: \"\\2726\"; }  /* filled star */
  h5::before { content: \"\\2727\"; }  /* open star */
  h6::before { content: \"\\25B8\"; }  /* triangle */
  /* TODO-keyword pills.  Shared shape here; per-keyword colors below match
     the faces in `svg-tag-tags'.  Unlisted keywords fall back to this. */
  .todo, .done {
    font-family: \"SF Mono\", Menlo, monospace; font-size: 0.72em;
    font-weight: 700; padding: 0.08rem 0.4rem; border-radius: 4px;
    vertical-align: 0.12em; letter-spacing: 0.02em;
    background: %s; color: %s; }
"
             bullet surface text)
     keyword-css
     (format "  .tag { color: %s; background: %s; font-family: \"SF Mono\", Menlo, monospace;
         font-size: 0.7em; padding: 0.05rem 0.35rem; border-radius: 4px; }
  .timestamp { color: %s; }
  .timestamp-kwd { color: %s; }
</style>"
             yellow surface muted sky))))

(defun my/org-preview-css ()
  "Return the combined CSS injected into the preview `<head>'.
Shared pandoc theme (colors) plus Org structure styling (hierarchy)."
  (concat (my/org-preview--pandoc-css) "\n" (my/org-preview--structure-css)))

(defvar-local my/org-preview-export-file nil
  "Temp HTML file backing this buffer's Org preview.")

(defvar my/org-preview-xwidget nil
  "Xwidget session used by the Org live preview.")

(defun my/org-preview-export ()
  "Export the current Org buffer to its preview HTML file and return it.
Injects the shared Catppuccin CSS, drops Org's default stylesheet, omits
section numbers, and exports code blocks verbatim (styled by the CSS, not
by htmlize) so the result matches the Markdown/pandoc preview in both
themes.  A table of contents is included only when
`my/org-preview-with-toc' is non-nil."
  (let* ((file (or my/org-preview-export-file
                   (let ((temporary-file-directory
                          (expand-file-name my/org-preview-directory)))
                     (make-temp-file "org-preview-" nil ".html"))))
         (org-html-head-include-default-style nil)
         (org-html-head-extra (my/org-preview-css))
         (org-html-htmlize-output-type nil)
         (org-export-with-toc my/org-preview-with-toc)
         (org-export-with-section-numbers nil)
         (org-export-show-temporary-export-buffer nil))
    (setq my/org-preview-export-file file)
    (org-export-to-file 'html file)
    file))

(defvar-local my/org-preview-buffer nil
  "Xwidget preview buffer backing this Org source buffer.")

(defvar-local my/org-preview-source-buffer nil
  "Org source buffer that this xwidget preview was generated from.")

(defvar my/org-preview--in-teardown nil
  "Non-nil while tearing down a preview, to prevent re-entrant kills.")

(defun my/org-preview-teardown (source xwidget file)
  "Tear down an Org live preview.
Delete FILE, kill the XWIDGET preview buffer, and turn off live
preview in the SOURCE buffer (SOURCE itself is never killed).
Safe to call from the `kill-buffer-hook' of either buffer: the
buffer currently being killed is left alone and re-entrant calls
are ignored, so killing the source and killing the xwidget behave
the same."
  (unless my/org-preview--in-teardown
    (let ((my/org-preview--in-teardown t)
          (kill-buffer-query-functions nil))
      (when (and file (file-exists-p file))
        (delete-file file))
      (when (buffer-live-p source)
        (with-current-buffer source
          (setq my/org-preview-buffer nil     ; avoid re-killing xwidget
                my/org-preview-export-file nil)
          (when (and (bound-and-true-p my/org-live-preview-mode)
                     (not (eq source (current-buffer))))
            (my/org-live-preview-mode -1))))
      (when (and (buffer-live-p xwidget)
                 (not (eq xwidget (current-buffer))))
        (kill-buffer xwidget)))))

(defun my/org-preview--on-xwidget-kill ()
  "Tear down the preview when the xwidget buffer is killed."
  (let ((source my/org-preview-source-buffer))
    (my/org-preview-teardown
     source
     (current-buffer)
     (and (buffer-live-p source)
          (buffer-local-value 'my/org-preview-export-file source)))))

(defun my/org-preview--on-source-kill ()
  "Tear down the preview when the Org source buffer is killed."
  (my/org-preview-teardown
   (current-buffer)
   my/org-preview-buffer
   my/org-preview-export-file))

(defun my/org-preview-window-xwidget (file)
  "Show FILE in the xwidget preview window, reusing the session."
  (let ((source (current-buffer)))
    (xwidget-webkit-browse-url (concat "file://" file))
    (setq my/org-preview-xwidget (xwidget-webkit-current-session))
    (let ((buf (xwidget-buffer my/org-preview-xwidget)))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (when (buffer-live-p source)
          (with-current-buffer source
            (setq my/org-preview-buffer buf)))
        (with-current-buffer buf
          (setq my/org-preview-source-buffer source)
          (add-hook 'kill-buffer-hook #'my/org-preview--on-xwidget-kill nil t))
        (pop-to-buffer buf)))))

(defun my/org-preview-refresh ()
  "Re-export the current Org buffer and update the xwidget preview."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (my/org-preview-window-xwidget (my/org-preview-export))))

;;; Scroll sync: keep the preview centered near point in the source buffer.
(defvar my/org-preview-sync-scroll-enabled t
  "When non-nil, scroll the xwidget preview to track point in the Org source.")

(defvar-local my/org-preview-last-line nil
  "Last source line synced to the preview, to avoid redundant scrolls.")

(defun my/org-preview-sync-scroll ()
  "Proportionally scroll the xwidget preview to match point in the source."
  (when (and my/org-preview-sync-scroll-enabled
             (bound-and-true-p my/org-live-preview-mode)
             my/org-preview-xwidget
             (xwidget-live-p my/org-preview-xwidget))
    (let ((line (line-number-at-pos)))
      (unless (eq line my/org-preview-last-line)
        (setq my/org-preview-last-line line)
        (let ((ratio (/ (float line)
                        (max 1 (line-number-at-pos (point-max))))))
          (xwidget-webkit-execute-script
           my/org-preview-xwidget
           (format "window.scrollTo(0, (document.body.scrollHeight - window.innerHeight) * %f);"
                   ratio)))))))

(define-minor-mode my/org-live-preview-mode
  "Live HTML preview of the current Org buffer in an xwidget.
Re-renders on every save; tracks point via proportional scroll."
  :lighter " OrgPrev"
  (if my/org-live-preview-mode
      (progn
        (unless (featurep 'xwidget-internal)
          (setq my/org-live-preview-mode nil)
          (user-error "This Emacs was built without xwidget support"))
        (add-hook 'after-save-hook #'my/org-preview-refresh nil t)
        (add-hook 'post-command-hook #'my/org-preview-sync-scroll nil t)
        (add-hook 'kill-buffer-hook #'my/org-preview--on-source-kill nil t)
        (my/org-preview-refresh))
    (remove-hook 'after-save-hook #'my/org-preview-refresh t)
    (remove-hook 'post-command-hook #'my/org-preview-sync-scroll t)
    (remove-hook 'kill-buffer-hook #'my/org-preview--on-source-kill t)
    (my/org-preview-teardown (current-buffer)
                             my/org-preview-buffer
                             my/org-preview-export-file)))

(defun my/org-live-preview (&optional arg)
  "Toggle the Org live HTML preview.
With a prefix ARG, include a table of contents in the render (and, if the
preview is already on, re-render to add it)."
  (interactive "P")
  (setq my/org-preview-with-toc (and arg t))
  (if my/org-live-preview-mode
      (if arg
          (my/org-preview-refresh)
        (my/org-live-preview-mode -1))
    (my/org-live-preview-mode 1)))

(define-key org-mode-map (kbd "C-c o v") #'my/org-live-preview)

(defun my/org-toggle-preview-theme ()
  "Toggle the Org preview between dark (Mocha) and light (Latte), then re-render."
  (interactive)
  (setq my/org-preview-theme
        (if (eq my/org-preview-theme 'light) 'dark 'light))
  (when (bound-and-true-p my/org-live-preview-mode)
    (my/org-preview-refresh))
  (message "Org preview theme: %s" my/org-preview-theme))

(define-key org-mode-map (kbd "C-c o V") #'my/org-toggle-preview-theme)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
