;;;; Local extensions to Exordium: Window manager

;;; Frame geometry

(defun frame-show-one-window ()
  (interactive)
  (delete-other-windows)
  ;;(modify-frame-parameters (selected-frame) '((top . 0) (left . 50)))
  (set-frame-width (selected-frame) 110))

(defun frame-show-two-windows ()
  (interactive)
  (delete-other-windows)
  ;;(modify-frame-parameters (selected-frame) '((top . 0) (left . 50)))
  (set-frame-width (selected-frame) (* 2 110))
  (let ((win (split-window-right)))
    (switch-to-other-buffer)))

(global-set-key [(f9)] #'frame-show-one-window)
(global-set-key [(f10)] #'frame-show-two-windows)

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

(defun resplit-vertically ()
  "Switch window split from horizontal to vertical, resizing the frame as well"
  (interactive)
  (if (not (= 2 (num-windows)))
      (error "I don't see 2 windows")
    (frame-show-two-windows)
    (toggle-window-split)))

;;; Buffers

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
