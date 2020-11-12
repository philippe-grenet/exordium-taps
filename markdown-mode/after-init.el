;;;; Local extensions to Exordium: Markdown mode

(require 'markdown-mode)

;; Renderer:
;; markdown rendering or impatient-markdown-mode:
(when exordium-osx
  ;; (setq markdown-command "/Users/pgrenet/Tools/markup/bin/github-markup"
  ;;       markdown-command-needs-filename t)
  (setq markdown-command "/usr/local/bin/multimarkdown"))

;; flymd
;; (require 'flymd)

;; (defun my-flymd-browser-function (url)
;;   (let ((process-environment (browse-url-process-environment)))
;;     (apply 'start-process
;;            (concat "google-chrome " url) nil
;;            "/usr/bin/open"
;;            (list "google-chrome" "--new-window" "--allow-file-access-from-files" url))))

;; (setq flymd-browser-open-function 'my-flymd-browser-function)

(setq markdown-max-image-size '(800 . 800))

;; Snippets
(add-hook 'markdown-mode-hook
          '(lambda ()
             (yas-minor-mode)))
(define-key markdown-mode-map (kbd "C-c y") 'yas-expand)
(define-key markdown-mode-map (kbd "<f2>") 'yas-expand)

;; Ease of use
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Causes a crash in Emacs, in function x_produce_glyph, after MacOS update!
;; Work around: toggle interactively with C-c C-x C-f
(setq markdown-fontify-code-blocks-natively t)

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

;; Use the Mac's built in dictionary
(when exordium-osx
  (load-file "~/.emacs.d/taps/markdown-mode/osx-dictionary.el")
  (define-key markdown-mode-map (kbd "s-$") 'osx-dictionary))
