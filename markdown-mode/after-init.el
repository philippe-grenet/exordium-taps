;;;; Local extensions to Exordium: Markdown mode

(require 'markdown-mode)

;; Renderer
(when exordium-osx
  (setq markdown-command "/Users/pgrenet/Tools/markup/bin/github-markup"
        markdown-command-needs-filename t))

;; Snippets
(add-hook 'markdown-mode-hook
          '(lambda ()
             (yas-minor-mode)))
(define-key markdown-mode-map (kbd "C-c y") 'yas-expand)
(define-key markdown-mode-map (kbd "<f2>") 'yas-expand)

;; Ease of use
(add-hook 'markdown-mode-hook 'flyspell-mode)
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

;; Use the Mac's built in dictionary
(when exordium-osx
  (load-file "~/.emacs.d/taps/markdown-mode/osx-dictionary.el")
  (define-key markdown-mode-map (kbd "s-$") 'osx-dictionary))
