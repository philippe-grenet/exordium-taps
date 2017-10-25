;;;; Local extensions to Exordium: Markdown mode

(require 'markdown-mode)
(setq markdown-command "/opt/local/bin/pandoc")
(add-hook 'markdown-mode-hook 'flyspell-mode)

(setq markdown-fontify-code-blocks-natively t)

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
(load-file "~/.emacs.d/taps/markdown-mode/osx-dictionary.el")
(define-key markdown-mode-map (kbd "s-$") 'osx-dictionary)
