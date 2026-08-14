;;;; Package --- summary : Local extensions to Exordium: Org Mode
;;; Commentary:
;;; Code:

;; Claude Code: https://github.com/stevemolitor/claude-code.el
;; Prefix C-c c for menu of commands. C-c c c to start Claude.

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for eat terminal backend:
(use-package eat :ensure t)

;; for vterm terminal backend:
(use-package vterm :ensure t)

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  ;;(add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;;(monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(setq claude-code-terminal-backend 'vterm)

;; Expose the org repo's Claude skills as Emacs commands: M-x skill-reindex,
;; M-x skill-drqs, etc. See org-skills.el.
(load-file "~/.emacs.d/taps/ai/org-skills.el")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
