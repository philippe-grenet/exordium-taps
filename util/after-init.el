;;; Utility functions

(defalias 'repunctuate 'bde-repunctuate)
(defun depunctuate ()
  "Put ONE space at the end of sentences in the selected region
or comment block. See also `repunctuate-sentences'."
  (interactive)
  (let (beginning end)
    (cond ((region-active-p)
           (setq beginning (region-beginning)
                 end (region-end)))
          ((bde-in-comment-p)
           (setq beginning (bde-comment-beginning)
                 end (bde-comment-end))))
    (if (and beginning end)
        (save-excursion
          (goto-char beginning)
          (while (re-search-forward "\\([]\"')]?\\)\\([.?!]\\)\\([]\"')]?\\) +" end t)
            (replace-match "\\1\\2\\3 " nil nil))
          (fill-paragraph))
      (message "No region or comment"))))

(defun scratch-msg ()
  "Create a new scratch bufer to edit markdown, that does not
  need to be safed."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-"))
  (markdown-mode))


;; Meta and Super key for the BB keyboard

(defun pc-mode ()
  "Binds Alt to Meta and the Windows key to Super."
  (interactive)
  (setq mac-option-modifier  'super
        mac-command-modifier 'meta)
  (message "PC keyboard mode (ALT is meta)"))

(defun mac-mode ()
  "Binds Option to Meta and Command to Super."
  (interactive)
  (setq mac-option-modifier  'meta
        mac-command-modifier 'alt)
  (message "Mac keyboard mode (OPTION is meta)"))

(pc-mode)


;; Jean-Louis's diff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
