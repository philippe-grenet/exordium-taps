;;;; Local extensions to Exordium: Window manager

;;; Frame geometry
(require 'treemacs)

(defconst pg/window-width 120)
(defconst pg/frame-height 104)

(defun frame-show-one-or-two-windows (x)
  "Show a single window if no argument. With C-u 1 prefix, show 2 windows"
  (interactive "P")
  (message (if x "Showing two windows" "Showing one window"))
  (delete-other-windows)
  ;; doesn't work with multiple monitors, need to find a solution
  ;;(modify-frame-parameters (selected-frame) '((top . 0) (left . 0)))
  (set-frame-width (selected-frame) (+ (if x (* 2 pg/window-width) pg/window-width)
                                       (if (eq (treemacs-current-visibility) 'visible) 20 0)))
  (set-frame-height (selected-frame) pg/frame-height)
  (when x
    (let ((win (split-window-right)))
      (switch-to-other-buffer))
    (switch-to-other-buffer)))

(defun frame-show-component ()
  "Show all 3 files for a C++ component"
  (interactive)
  (delete-other-windows)
  (toggle-frame-fullscreen)
  (cpp-switch-h-cpp nil)
  (let ((win (split-window-right)))
    (switch-to-other-buffer))
  (cpp-switch-h-cpp t)
  (let ((win (split-window1-right)))
    (switch-to-other-buffer))
  (balance-windows))

(global-set-key [(f10)] #'frame-show-one-or-two-windows)
(global-set-key [(shift f10)] #'frame-show-component)



;; Distraction-free mode
(require 'darkroom)
(defun distraction-free-mode ()
  (interactive)
  (darkroom-mode 'toggle)
  (toggle-frame-fullscreen))

(global-set-key[(shift f11)] #'distraction-free-mode)



;;; Split windows

(defun num-windows ()
  "Return the number of windows (unique buffers) in the current frame"
  (length (cl-delete-duplicates (mapcar #'window-buffer (window-list)))))

(defun toggle-window-split ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;; (defun resplit-vertically ()
;;   "Switch window split from horizontal to vertical, resizing the frame as well"
;;   (interactive)
;;   (if (not (= 2 (num-windows)))
;;       (error "I don't see 2 windows")
;;     (frame-show-two-windows)
;;     (toggle-window-split)))

(defun split-window-func-with-other-buffer (split-function)
  (let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-c3" 'split-window-horizontally-instead)
(global-set-key "\C-c2" 'split-window-vertically-instead)



;;; Buffers

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
