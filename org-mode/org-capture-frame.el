;;;; Desktop-wide Org capture frame -*- lexical-binding: t -*-
;;;
;;; Pops a small, dedicated Emacs frame in the middle of the current screen to
;;; run `org-capture', so that capturing works from anywhere in the macOS
;;; desktop and not only from within Emacs.  The frame closes itself on
;;; C-c C-c (finalize), C-c C-k (kill), C-c C-w (refile) or C-g (abort).
;;;
;;; It is driven from Hammerspoon (see ~/dotfiles/init.lua), which binds:
;;;
;;;   C-M-s-<f12>   : capture straight to the Inbox
;;;   S-C-M-s-<f12> : capture with the full template menu
;;;
;;; and calls, via emacsclient:
;;;
;;;   emacsclient -e '(my/org-capture-frame "i" SX SY SW SH)'
;;;
;;; where SX SY SW SH is the rectangle of the screen holding the focused
;;; window, in points with a top-left origin.  Hammerspoon supplies it because
;;; Emacs has no idea which display the user is looking at.
;;;
;;; Two macOS-specific details shape the code below.  First, a frame created by
;;; `emacsclient -c' belongs to the client and is torn down when the client
;;; exits, so the frame is built by this server-side `make-frame' instead.
;;; Second, the NS port only raises a frame, it cannot activate the
;;; application, so Hammerspoon is the one that gives Emacs keyboard focus.

(require 'org)
(require 'org-capture)
(require 'seq)

(defvar my/org-capture-frame-lines 10
  "Height, in lines, of the capture frame once a template is chosen.")

(defvar my/org-capture-frame-columns 90
  "Width, in columns, of the capture frame.")

(defvar my/org-capture-menu-lines 24
  "Height, in lines, of the capture frame while the template menu is shown.
`org-capture-templates' holds enough entries that the menu does not fit in
`my/org-capture-frame-lines'.")

(defun my/org-capture--frame ()
  "Return the live dedicated capture frame, or nil."
  (seq-find (lambda (frame) (frame-parameter frame 'my/org-capture-frame))
            (frame-list)))

(defun my/org-capture--center (frame)
  "Center FRAME in the screen rectangle Hammerspoon stored on it.
Positions are wrapped in a `(+ N)' list so that a negative coordinate means
\"absolute\" rather than \"offset from the right/bottom edge\", which matters
as soon as a display sits to the left of the main one."
  (pcase (frame-parameter frame 'my/org-capture-screen)
    (`(,sx ,sy ,sw ,sh)
     (let ((x (truncate (+ sx (/ (- sw (frame-pixel-width frame)) 2))))
           (y (truncate (+ sy (/ (- sh (frame-pixel-height frame)) 3)))))
       (modify-frame-parameters frame `((left . (+ ,x)) (top . (+ ,y))))))))

(defun my/org-capture--close (frame)
  "Delete FRAME, unless it is dead already or is the last remaining frame."
  (when (and (frame-live-p frame) (cdr (frame-list)))
    (delete-frame frame)))

(defun my/org-capture--setup ()
  "Give the capture buffer the whole capture frame, then shrink and recenter.
`org-capture' splits the window, which would leave half a dozen usable lines,
and the frame starts tall enough to show the template menu.  Also force the
pending fontification, so that the entry is styled as soon as it appears."
  (when (frame-parameter nil 'my/org-capture-frame)
    (delete-other-windows)
    (set-frame-height nil my/org-capture-frame-lines)
    (my/org-capture--center (selected-frame))
    ;; `jit-lock-defer-time' is non-nil, so the freshly inserted template is
    ;; left marked `fontified' = `defer': the TODO pill and the bullet would
    ;; only show up once an edit dirties the line.  The capture buffer is
    ;; narrowed to the entry, so forcing the pass here is a handful of
    ;; characters, never the whole of the target file.
    (font-lock-ensure)))

(add-hook 'org-capture-mode-hook #'my/org-capture--setup)

(defvar my/org-capture--refiling nil
  "Non-nil while `org-capture-refile' is running.")

(defun my/org-capture--finalize ()
  "Close the dedicated capture frame on C-c C-c or C-c C-k.
A no-op for captures started from an ordinary frame, since
`org-capture-after-finalize-hook' is global."
  (when (and (frame-parameter nil 'my/org-capture-frame)
             (not my/org-capture--refiling))
    (my/org-capture--close (selected-frame))))

(add-hook 'org-capture-after-finalize-hook #'my/org-capture--finalize)

(defun my/org-capture--refile-advice (fn &rest args)
  "Apply FN to ARGS, keeping the capture frame alive until refiling is done.
`org-capture-refile' finalizes the capture -- and so would delete the frame --
before prompting for a refile target."
  (let ((my/org-capture--refiling t)
        (frame (and (frame-parameter nil 'my/org-capture-frame)
                    (selected-frame))))
    (unwind-protect (apply fn args)
      (my/org-capture--close frame))))

(advice-add 'org-capture-refile :around #'my/org-capture--refile-advice)

(defun my/org-capture-frame (&optional key sx sy sw sh)
  "Pop a dedicated frame and run `org-capture' in it.
KEY, when non-nil, is an `org-capture-templates' key to use directly, skipping
the selection menu.  SX, SY, SW and SH describe the screen rectangle to center
the frame in, in points with a top-left origin; when omitted, the window
manager places the frame."
  (interactive)
  (let ((existing (my/org-capture--frame)))
    (if existing
        ;; Second press of the hotkey: reuse the frame rather than stack frames.
        (select-frame-set-input-focus existing)
      (let ((frame (make-frame
                    `((name . "org-capture")
                      (my/org-capture-frame . t)
                      (my/org-capture-screen . ,(and sx (list sx sy sw sh)))
                      (width . ,my/org-capture-frame-columns)
                      (height . ,(if key
                                     my/org-capture-frame-lines
                                   my/org-capture-menu-lines))
                      (minibuffer . t)
                      (tool-bar-lines . 0)
                      (menu-bar-lines . 0)
                      (vertical-scroll-bars . nil)))))
        (my/org-capture--center frame)
        (select-frame-set-input-focus frame)
        ;; Dispatch the capture itself out-of-band: the template menu reads a
        ;; key, which would otherwise block the calling emacsclient until the
        ;; user answers.  Frame creation stays synchronous so that genuine
        ;; failures still reach Hammerspoon as a non-zero exit code.
        (run-at-time
         0 nil
         (lambda ()
           (with-selected-frame frame
             (condition-case nil
                 (org-capture nil key)
               ;; C-g at the template menu never reaches the finalize hook.
               ((quit error) (my/org-capture--close frame))))))
        t))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
