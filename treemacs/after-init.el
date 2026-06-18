;;;; Package --- summary : Local extensions to Exordium: Treemacs
;;; Commentary:
;;; Code:

(require 'treemacs)
(use-package treemacs-all-the-icons
  :ensure t)

(setq treemacs-wrap-around nil
      treemacs-width 65
      ;;treemacs-text-scale -0.3
      treemacs-is-never-other-window nil)
(treemacs-filewatch-mode 0)  ; to many files descriptors otherwise
(treemacs-git-mode 'extended)
(treemacs-load-theme 'all-the-icons)

(global-set-key (kbd "C-c t t") #'treemacs-select-window)

(defun toggle-treemacs-visibility ()
  "Show or hide treemacs and resize the frame as needed."
  (interactive)
  (treemacs)
  (if (eq (treemacs-current-visibility) 'visible)
      (set-frame-width (selected-frame) 185)
    (set-frame-width (selected-frame) 120)))

(global-set-key (kbd "<f5>") #'toggle-treemacs-visibility)

(defun treemacs-current ()
  "Open treemacs for the current buffer."
  (interactive)
  (when (eq (treemacs-current-visibility) 'none)
    (treemacs)
    (treemacs-add-and-display-current-project-exclusively)))

(global-set-key (kbd "C-c t s") #'treemacs-switch-workspace)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
