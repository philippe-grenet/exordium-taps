;;;; SVG pills for TODO keywords, dates and progress -*- lexical-binding: t -*-
;;;
;;; Renders TODO keywords, priorities, dates, {:tags:}, ((pills)) and progress
;;; cookies as SVG tags: https://github.com/rougier/svg-tag-mode/
;;;
;;; The rules below are global.  A file that declares its own keywords with a
;;; "#+todo:" line gets pills for them on the fly, in that buffer only; see
;;; `my/svg-todo-auto-tags' at the bottom.  Nothing here knows about the notes
;;; repo, so after-init.el loads this file on every machine.

(require 'org)
(require 'cl-lib)

(use-package svg-tag-mode
  :ensure t
  :config
  (progn
    (defun svg-progress-percent (value)
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                        nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag (concat value "%")
                               nil :stroke 0 :margin 0)) :ascent 'center))

    (defun svg-progress-count (value)
      (let* ((seq (mapcar #'string-to-number (split-string value "/")))
             (count (float (car seq)))
             (total (float (cadr seq))))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag value nil
                                 :stroke 0 :margin 0)) :ascent 'center)))

    (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
    (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
    (defconst day-re "[A-Za-z]\\{3\\}")
    (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

    (setq svg-tag-tags
          `(
            ;; Plain TODO statuses
            ("\\(TODO\\)" . ((lambda (tag)
                               (svg-tag-make tag :face 'font-lock-warning-face :inverse t))))
            ("\\(DONE\\)" . ((lambda (tag)
                               (svg-tag-make tag :face 'font-lock-string-face :inverse t))))
            ("\\(WORK\\)" . ((lambda (tag)
                               (svg-tag-make tag :face 'font-lock-type-face :inverse t))))
            ("\\(STOP\\)" . ((lambda (tag)
                               (svg-tag-make tag :face 'font-lock-comment-face :inverse t))))
            ("\\(WAIT\\)" . ((lambda (tag)
                               (svg-tag-make tag :face 'font-lock-function-name-face :inverse t))))
            ("\\(BLOCKED\\)" . ((lambda (tag)
                                  (svg-tag-make tag :face 'dired-flagged :inverse t))))
            ("\\(READY\\)" . ((lambda (tag)
                                (svg-tag-make tag :face 'font-lock-function-name-face :inverse t))))
            ("\\(REVIEW\\)" . ((lambda (tag)
                                 (svg-tag-make tag :face 'font-lock-variable-name-face :inverse t))))
            ;; Priorities
            ("\\(\\[#A\\]\\)" . ((lambda (tag)
                                   (svg-tag-make tag
                                                 :face 'font-lock-warning-face :inverse t
                                                 :beg 1 :end -1))))
            ("\\(\\[#B\\]\\)" . ((lambda (tag)
                                   (svg-tag-make tag
                                                 :face 'font-lock-type-face :inverse t
                                                 :beg 1 :end -1))))
            ("\\(\\[#C\\]\\)" . ((lambda (tag)
                                   (svg-tag-make tag
                                                 :face 'font-function-name-face :inverse t
                                                 :beg 1 :end -1))))
            ;; Rectangles with plain words: {:Something:}
            ;; Consider expending to "\\({:[A-Za-z0-9]+\\(?:[ ][A-Za-z0-9]+\\)*:}\\)"
            ("\\({:[A-Za-z]+:}\\)" . ((lambda (tag)
                                        (svg-tag-make tag
                                                      :face 'font-lock-type-face
                                                      :beg 2 :end -2 :inverse nil))))
            ;; Rectangles with plain words: {{Something}}
            ;; ("\\({{[A-Za-z]+}}\\)" . ((lambda (tag)
            ;;                             (svg-tag-make tag
            ;;                                           :face 'font-lock-comment-face
            ;;                                           :beg 1 :end -1 :inverse nil))))
            ;; Pills with 1 or 2 letters or numbers: ((A)) ((AA)) ((1)) ((10))
            ("\\((([0-9a-zA-Z]))\\)" . ((lambda (tag)
                                          (svg-tag-make tag :beg 2 :end -2 :radius 12))))
            ("\\((([0-9a-zA-Z][0-9a-zA-Z]))\\)" . ((lambda (tag)
                                                     (svg-tag-make tag :beg 2 :end -2 :radius 8))))
            ;;
            ;; Active date (with or without day name, with or without time)
            (,(format "\\(<%s>\\)" date-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :end -1 :margin 0))))
            (,(format "\\(<%s \\)%s>" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
            (,(format "<%s \\(%s>\\)" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
            ;; Inactive date  (with or without day name, with or without time)
            (,(format "\\(\\[%s\\]\\)" date-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
            (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
            (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
            ;;
            ;; Progress: [1/3] or [42%]
            ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                                (svg-progress-percent (substring tag 1 -2)))))
            ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                              (svg-progress-count (substring tag 1 -1)))))))
    (add-hook 'org-mode-hook 'svg-tag-mode)
    ))


;;; Auto svg-tags for per-file TODO keywords
;;
;; Files may declare extra states via a "#+todo:" line (e.g. DROPPED, GOAL)
;; that have no rule in the global `svg-tag-tags'.  For any such keyword we
;; generate a buffer-local pill on the fly.  Colours are auto-cycled, but a
;; file can pin them explicitly with one or more directives:
;;
;;   #+svg_todo: DROPPED font-lock-comment-face
;;   #+svg_todo: GOAL    font-lock-warning-face

(defvar my/svg-todo-auto-palette
  '(font-lock-keyword-face font-lock-constant-face
    font-lock-preprocessor-face font-lock-doc-face)
  "Faces cycled through for auto-generated TODO svg tags.")

(defun my/svg-todo-covered-p (kw)
  "Non-nil if KW already matches an existing `svg-tag-tags' rule."
  (cl-some (lambda (entry) (string-match-p (car entry) kw)) svg-tag-tags))

(defun my/svg-todo-overrides ()
  "Return an alist (KEYWORD . FACE) parsed from #+svg_todo: directives."
  (let (alist)
    (dolist (val (cdr (assoc "SVG_TODO" (org-collect-keywords '("SVG_TODO")))))
      (pcase-let ((`(,kw ,face) (split-string (string-trim val) nil t)))
        (when (and kw face (facep (intern face)))
          (push (cons kw (intern face)) alist))))
    alist))

(defun my/svg-todo-auto-tags ()
  "Give any file-local TODO keyword without an svg rule a default pill.
Explicit colours from #+svg_todo: directives take precedence over the
auto-cycled palette; done-type keywords otherwise fall back to a dimmed face."
  (when (bound-and-true-p svg-tag-mode)
    (let ((overrides (my/svg-todo-overrides))
          (extra '())
          (i 0))
      (dolist (kw (and (boundp 'org-todo-keywords-1) org-todo-keywords-1))
        (unless (my/svg-todo-covered-p kw)
          (let ((face (or (cdr (assoc kw overrides))
                          (if (member kw org-done-keywords)
                              'font-lock-comment-face
                            (prog1 (nth (mod i (length my/svg-todo-auto-palette))
                                        my/svg-todo-auto-palette)
                              (setq i (1+ i)))))))
            (push (cons (format "\\(%s\\)" (regexp-quote kw))
                        (list (lambda (tag)
                                (svg-tag-make tag :face face :inverse t))))
                  extra))))
      (when extra
        ;; Buffer-local copy = new rules + global rules; global list untouched.
        (setq-local svg-tag-tags (append extra svg-tag-tags))
        (svg-tag-mode -1)
        (svg-tag-mode 1)))))

;; Append so it runs AFTER svg-tag-mode has been enabled by its own hook.
(add-hook 'org-mode-hook #'my/svg-todo-auto-tags t)


;;; org-svg-tags.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
