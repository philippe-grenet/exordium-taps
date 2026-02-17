;;;; package -- Summary
;;;; Commentary:
;;; after-init.el --- Initialize LSP mode
;;;; Code:


;; JAVA
;; See https://github.com/emacs-lsp/lsp-java

(require 'helm)
(setq helm-split-window-default-side 'below
      helm-split-window-inside-p t)

;;(require 'lsp-java)
;;(add-hook 'java-mode-hook #'lsp)

(use-package lsp-ui)
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package helm-lsp)

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))


(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)


;; Keys: see https://emacs-lsp.github.io/lsp-mode/page/keybindings/

(define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
(define-key lsp-mode-map (kbd "M-,") #'lsp-find-references)

;;; after-init.el ends here
