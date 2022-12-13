;; See https://github.com/emacs-lsp/lsp-java

(require 'helm)
(setq helm-split-window-default-side 'below)
(setq helm-split-window-in-side-p t)

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(use-package lsp-ui)
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package helm-lsp)
(use-package lsp-treemacs)

;; Keys: see https://emacs-lsp.github.io/lsp-mode/page/keybindings/
