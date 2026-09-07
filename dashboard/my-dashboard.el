;;;; The second-brain dashboard -*- lexical-binding: t -*-
;;;
;;; `M-x dashboard' opens a read-only overview of the org notes repo:
;;;
;;;   - a three-month calendar centred on the current month, followed by the
;;;     org agenda for the next three weeks;
;;;   - the tasks that matter this week, read from todo.org;
;;;   - the projects touched most recently, according to git.
;;;
;;; Each section lives in its own file -- my-dashboard-calendar.el,
;;; my-dashboard-tasks.el and my-dashboard-projects.el -- and exports a single
;;; `my-dashboard-insert-...' function, called from `my-dashboard--render'.
;;; This file owns everything the three have in common: the faces, the layout
;;; helpers, the major mode and its keys.
;;;
;;; Everything addresses `my/org-repo', so after-init.el only loads this file
;;; on a machine that actually has the repo.

(require 'button)
(require 'cl-lib)
(require 'time-date)

(defvar my/org-repo)
(declare-function my/org-file "before-init")
(declare-function open-todo-file "org-notes")
(declare-function open-catchup-file "org-notes")
(declare-function org-fold-show-context "org-fold")

(defconst my-dashboard--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the dashboard tap, used to locate the logo.")


;;; Options

(defgroup my-dashboard nil
  "A start-up overview of the second-brain org repo."
  :group 'applications
  :prefix "my-dashboard-")

(defcustom my-dashboard-buffer-name "*dashboard*"
  "Name of the dashboard buffer."
  :type 'string)

(defcustom my-dashboard-logo (expand-file-name "Exordium-mocha.png"
                                               my-dashboard--directory)
  "Image displayed at the top of the dashboard, or nil for none."
  :type '(choice file (const nil)))

(defcustom my-dashboard-logo-width 520
  "Width in pixels of the logo, before it is capped to the window width."
  :type 'natnum)

(defcustom my-dashboard-agenda-days 21
  "Number of days of org agenda to show, starting today."
  :type 'natnum)

(defcustom my-dashboard-projects 7
  "Number of recently updated projects to list."
  :type 'natnum)

(defcustom my-dashboard-keys
  '(("RET" . "open") ("TAB" . "next")    ("g" . "refresh") ("t" . "todo")
    ("c"   . "catchup") ("r" . "roadmap") ("a" . "agenda") ("q" . "quit"))
  "Keys listed in the reminder at the bottom of the dashboard.
An alist of (KEY . WHAT).  Keeping this separate from `dashboard-mode-map'
means the reminder can stay short; edit both when you add a binding worth
advertising."
  :type '(alist :key-type string :value-type string))


;;; Faces
;;
;; Each face is defined against a stock face, so the dashboard is readable
;; under any theme, and then repainted below with the Catppuccin palette --
;; which is the theme this configuration actually runs.

;; The scaled faces carry their `:height' in a constant as well, because
;; `my-dashboard-insert-centered' has to know it: a string at 1.4 is wider
;; than `string-width' says, and would otherwise be centred too far right.

(defconst my-dashboard-date-height 1.4
  "The `:height' of `my-dashboard-date' and `my-dashboard-week'.")

(defface my-dashboard-date
  `((t :inherit font-lock-function-name-face :weight bold
       :height ,my-dashboard-date-height))
  "Face for today's date, at the top of the dashboard.")

(defface my-dashboard-week
  `((t :inherit shadow :height ,my-dashboard-date-height))
  "Face for the week number next to today's date.")

(defconst my-dashboard-heading-height 1.15
  "The `:height' of `my-dashboard-heading'.")

(defface my-dashboard-heading
  `((t :inherit font-lock-keyword-face :weight bold
       :height ,my-dashboard-heading-height))
  "Face for the dashboard section headings.")

(defface my-dashboard-separator
  '((t :inherit shadow :underline t))
  "Face for the horizontal rules between sections.
The rules are stretch spaces reaching the right edge of the window, so the
line itself is this face's underline.")

(defconst my-dashboard-footer-height 0.85
  "The `:height' of `my-dashboard-footer'.")

(defface my-dashboard-footer
  `((t :inherit shadow :height ,my-dashboard-footer-height))
  "Face for the key reminder at the bottom of the dashboard.")

(defface my-dashboard-today
  '((t :inherit calendar-today :weight bold))
  "Face for today's date inside the three-month calendar.")

(defface my-dashboard-week-number
  '((t :inherit shadow))
  "Face for the ISO week numbers down the left of the calendar.")

(defface my-dashboard-important
  '((t :inherit warning :weight bold))
  "Face for tasks flagged important or urgent.")

(defface my-dashboard-detail
  '((t :inherit shadow))
  "Face for secondary text, such as how long ago a project was touched.")

(defvar exordium-theme)
(defvar exordium-catppuccin-flavor)
(eval-when-compile
  (require 'color-theme-catppuccin nil t))

(defun my-dashboard--apply-catppuccin ()
  "Repaint the dashboard faces with the Catppuccin palette."
  (require 'color-theme-catppuccin)
  (with-catppuccin-colors
   exordium-catppuccin-flavor
   (set-face-attribute 'my-dashboard-date nil :foreground blue)
   (set-face-attribute 'my-dashboard-week nil :foreground overlay1)
   (set-face-attribute 'my-dashboard-heading nil :foreground mauve)
   (set-face-attribute 'my-dashboard-separator nil
                       :foreground surface1 :underline surface1)
   (set-face-attribute 'my-dashboard-today nil
                       :foreground base :background green)
   (set-face-attribute 'my-dashboard-week-number nil :foreground overlay0)
   (set-face-attribute 'my-dashboard-important nil :foreground peach)
   (set-face-attribute 'my-dashboard-detail nil :foreground overlay1)
   (set-face-attribute 'my-dashboard-footer nil :foreground overlay0)))

(when (memq (bound-and-true-p exordium-theme)
            '(catppuccin-mocha catppuccin-frappe
              catppuccin-macchiato catppuccin-latte))
  (my-dashboard--apply-catppuccin))


;;; Layout helpers
;;
;; The body width is computed once per redraw and cached, so that a section
;; rendered into a temporary buffer still lines up with the rest.

(defvar my-dashboard--width nil
  "Body width in columns for the redraw in progress, or nil outside one.")

(defun my-dashboard-width ()
  "Return the width, in columns, of the dashboard body."
  (or my-dashboard--width (max 40 (window-body-width))))

(defun my-dashboard-insert-centered (string &optional height)
  "Insert STRING horizontally centred, followed by a newline.
HEIGHT is the `:height' of the face STRING carries; the padding is
corrected for it, since a scaled string is not `string-width' columns wide."
  (let* ((width (* (or height 1.0) (string-width string)))
         (pad (max 0 (floor (/ (- (my-dashboard-width) width) 2)))))
    (insert (make-string pad ?\s) string "\n")))

(defun my-dashboard-insert-separator ()
  "Insert a horizontal rule reaching the right edge of the window.
The rule is a stretch space carrying `my-dashboard-separator', whose
underline draws the line -- so it stays exact when the window is resized,
which a run of box-drawing characters would not.  A blank line is left on
either side, so sections do not have to end with one of their own."
  (insert "\n"
          (propertize " "
                      'display '(space :align-to right)
                      'font-lock-face 'my-dashboard-separator)
          "\n\n"))

(defun my-dashboard-insert-heading (text)
  "Insert TEXT as a section heading."
  (insert (propertize text 'font-lock-face 'my-dashboard-heading) "\n\n"))

(defun my-dashboard-insert-empty (text)
  "Insert TEXT as the placeholder for a section that has nothing to show."
  (insert "  " (propertize text 'font-lock-face 'my-dashboard-detail) "\n"))

(defvar svg-tag-tags)
(declare-function svg-tag-make "svg-tag-mode")

(defun my-dashboard-svg-tag (text face)
  "Return TEXT as an SVG pill, falling back to TEXT propertized with FACE.
The pill is built from the rules in `svg-tag-tags', so TODO keywords and
priority cookies look here exactly as they do in an Org buffer -- see
taps/org-mode/org-svg-tags.el, which is where those rules come from."
  (or (and (display-graphic-p)
           (fboundp 'svg-tag-make)
           (boundp 'svg-tag-tags)
           (when-let*
               ((image
                 (cl-loop for (regexp . spec) in svg-tag-tags
                          for maker = (car-safe spec)
                          when (and (functionp maker)
                                    (string-match-p
                                     (concat "\\`\\(?:" regexp "\\)\\'") text))
                          return (ignore-errors (funcall maker text)))))
             (propertize text 'display image)))
      (propertize text 'font-lock-face face)))

(defun my-dashboard-relative-age (time)
  "Return TIME as an approximate age relative to now, such as \"3 days ago\"."
  (let ((days (- (time-to-days (current-time)) (time-to-days time))))
    (cond ((<= days 0) "today")
          ((= days 1) "yesterday")
          ((< days 7) (format "%d days ago" days))
          ((< days 14) "last week")
          ((< days 60) (format "%d weeks ago" (/ days 7)))
          (t (format "%d months ago" (max 2 (/ days 30)))))))


;;; Following a link
;;
;; The dashboard is a launcher: following one of its lines closes it and
;; leaves the target file in the window it occupied.
;;
;; Two mechanisms, because only some of the lines should look like links.
;; The projects are buttons, underlined by the `button' face.  The tasks and
;; the agenda lines carry a `my-dashboard-target' property instead: RET still
;; opens them, but they are drawn as ordinary text.

(defun my-dashboard-visit (file &optional location)
  "Close the dashboard and open FILE.
LOCATION says where to land: a buffer position, or a string to search for
from the top of FILE.  The spot is revealed when FILE is in Org mode."
  (my-dashboard-quit)
  (find-file file)
  (when location
    (widen)
    (goto-char (point-min))
    (when (cond ((integerp location)
                 (goto-char (min location (point-max))))
                ((search-forward location nil t)
                 (goto-char (match-beginning 0))))
      (when (and (derived-mode-p 'org-mode)
                 (fboundp 'org-fold-show-context))
        (org-fold-show-context 'org-goto))
      (recenter 3))))

(defun my-dashboard-target-at-point ()
  "Return the (FILE . LOCATION) of the line at point, or nil."
  (get-text-property (line-beginning-position) 'my-dashboard-target))

(defun my-dashboard-set-target (beg end file location)
  "Make the text between BEG and END open FILE at LOCATION on RET."
  (put-text-property beg end 'my-dashboard-target (cons file location)))

(defun dashboard-follow ()
  "Follow the thing at point: a button, or a line with a target."
  (interactive)
  (let ((target (my-dashboard-target-at-point)))
    (cond
     ((button-at (point)) (push-button (point)))
     ((and target (car target)) (my-dashboard-visit (car target) (cdr target)))
     (t (user-error "Nothing to follow here")))))

(define-button-type 'my-dashboard-file
  'follow-link t
  'help-echo "mouse-1, RET: open this file"
  'action (lambda (button)
            (my-dashboard-visit (button-get button 'my-file)
                                (button-get button 'my-search))))

(defun my-dashboard-insert-file-button (label file &optional search)
  "Insert a button labelled LABEL that opens FILE, optionally at SEARCH."
  (insert-text-button label
                      :type 'my-dashboard-file
                      'my-file file
                      'my-search search))


;;; Rendering

(declare-function my-dashboard-insert-calendar "my-dashboard-calendar")
(declare-function my-dashboard-insert-agenda "my-dashboard-calendar")
(declare-function my-dashboard-insert-tasks "my-dashboard-tasks")
(declare-function my-dashboard-insert-projects "my-dashboard-projects")

(defun my-dashboard--insert-logo ()
  "Insert the logo, centred, when there is one and Emacs can display it."
  (when (and my-dashboard-logo
             (display-graphic-p)
             (file-readable-p my-dashboard-logo)
             (image-type-available-p 'png))
    (let* ((width (min my-dashboard-logo-width
                       (max 200 (- (window-pixel-width) 80))))
           (image (create-image my-dashboard-logo nil nil
                                :width width :ascent 'center))
           (pad (max 0 (/ (- (window-pixel-width)
                             (car (image-size image t)))
                          2))))
      (insert (propertize " " 'display `(space :width (,pad))))
      (insert-image image)
      (insert "\n\n"))))

(defun my-dashboard--insert-date ()
  "Insert today's date, spelled out, with its ISO week number."
  (my-dashboard-insert-centered
   (concat (propertize (format-time-string "%A, %B %-d, %Y")
                       'font-lock-face 'my-dashboard-date)
           (propertize (format-time-string "   ·   Week %V")
                       'font-lock-face 'my-dashboard-week))
   my-dashboard-date-height))

(defun my-dashboard--insert-keys ()
  "Insert the reminder of this mode's keys, small and dimmed."
  (my-dashboard-insert-centered
   (propertize (mapconcat (lambda (entry)
                            (format "%s %s" (car entry) (cdr entry)))
                          my-dashboard-keys "  ·  ")
               'font-lock-face 'my-dashboard-footer)
   my-dashboard-footer-height))

(defun my-dashboard--render ()
  "Draw the whole dashboard into the current buffer."
  (let ((inhibit-read-only t)
        (my-dashboard--width (max 40 (window-body-width))))
    (erase-buffer)
    (my-dashboard--insert-logo)
    (my-dashboard--insert-date)
    (my-dashboard-insert-separator)
    (my-dashboard-insert-calendar)
    (insert "\n")
    (my-dashboard-insert-agenda)
    (my-dashboard-insert-separator)
    (my-dashboard-insert-tasks)
    (my-dashboard-insert-separator)
    (my-dashboard-insert-projects)
    (my-dashboard-insert-separator)
    (my-dashboard--insert-keys)
    (goto-char (point-max))
    ;; Drop the final newline, so the last line is the key reminder rather
    ;; than an empty one.
    (when (bolp) (delete-char -1))
    ;; Leave point there: on the logo, `hl-line-mode' would otherwise draw a
    ;; band across the image.
    (set-buffer-modified-p nil)))


;;; Commands

(defun my-dashboard--org-file (name)
  "Return the path of NAME in the org repo, or nil when there is no repo."
  (and my/org-repo (expand-file-name name my/org-repo)))

(defun dashboard-refresh ()
  "Redraw the dashboard."
  (interactive)
  (my-dashboard--render)
  (message "Dashboard refreshed"))

(defun dashboard-quit ()
  "Close the dashboard."
  (interactive)
  (my-dashboard-quit))

(defun my-dashboard-quit ()
  "Kill the dashboard buffer if it exists, and stay in its window."
  (when-let* ((buffer (get-buffer my-dashboard-buffer-name)))
    (when (eq buffer (current-buffer))
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun dashboard-open-todo ()
  "Close the dashboard and open todo.org."
  (interactive)
  (my-dashboard-quit)
  (if (fboundp 'open-todo-file)
      (open-todo-file)
    (find-file (my-dashboard--org-file "todo.org"))))

(defun dashboard-open-catchup ()
  "Close the dashboard and open catchup.org."
  (interactive)
  (my-dashboard-quit)
  (if (fboundp 'open-catchup-file)
      (open-catchup-file)
    (find-file (my-dashboard--org-file "catchup.org"))))

(defun dashboard-open-roadmap ()
  "Close the dashboard and open roadmap.org."
  (interactive)
  (my-dashboard-visit (my-dashboard--org-file "roadmap.org")))

(defun dashboard-open-agenda ()
  "Close the dashboard and open the org agenda."
  (interactive)
  (my-dashboard-quit)
  (call-interactively #'org-agenda))

;;;###autoload
(defun dashboard ()
  "Open the second-brain dashboard."
  (interactive)
  (unless my/org-repo
    (user-error "No org repo on this machine (set ORG_REPO_DIR)"))
  (let ((buffer (get-buffer-create my-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'dashboard-mode)
        (dashboard-mode)))
    (switch-to-buffer buffer)
    ;; Render once the buffer is on screen: the layout depends on the width
    ;; of the window it ends up in.
    (with-current-buffer buffer
      (my-dashboard--render))))


;;; Opening at startup
;;
;; `emacs-startup-hook' rather than `initial-buffer-choice', because the hook
;; can look at what is already on screen and stand down.  Setting
;; `initial-buffer-choice' cannot: whatever its function returns is displayed,
;; so a file named on the command line would end up in a split next to the
;; dashboard, with the dashboard focused.
;;
;; That costs one global setting.  `command-line-1' runs `emacs-startup-hook'
;; itself, *before* putting up the startup screen, and the screen would then
;; replace whatever the hook had displayed -- so the screen has to go.  With
;; `inhibit-startup-screen' set, `command-line-1' leaves the hook to
;; `normal-top-level', which runs it once everything else is displayed.  The
;; visible effect in a terminal is that Emacs starts on *scratch* rather than
;; on the GNU Emacs splash.

(defcustom my-dashboard-at-startup t
  "Whether Emacs should start on the dashboard rather than *scratch*.
Only ever on a graphical frame: a terminal Emacs keeps its usual *scratch*.
Setting this in `custom-file' comes too late to take effect for the session
being started; set it from ~/.emacs.d/before-init.el instead."
  :type 'boolean)

(defun my-dashboard--startup ()
  "Show the dashboard, unless this session asked to start on something else."
  (when (and my-dashboard-at-startup
             my/org-repo
             (display-graphic-p)
             ;; A file named on the command line, or a restored desktop, has
             ;; already claimed the window -- and that is what was asked for.
             (not (buffer-file-name (window-buffer))))
    (dashboard)))

(when my-dashboard-at-startup
  (setq inhibit-startup-screen t)
  (add-hook 'emacs-startup-hook #'my-dashboard--startup t))


;;; Major mode

(defvar-keymap dashboard-mode-map
  :doc "Keymap for `dashboard-mode'."
  "q"         #'dashboard-quit
  "g"         #'dashboard-refresh
  "t"         #'dashboard-open-todo
  "c"         #'dashboard-open-catchup
  "r"         #'dashboard-open-roadmap
  "a"         #'dashboard-open-agenda
  "n"         #'next-line
  "p"         #'previous-line
  "RET"       #'dashboard-follow
  "TAB"       #'forward-button
  "<tab>"     #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Major mode for the second-brain dashboard.

\\{dashboard-mode-map}"
  (buffer-disable-undo)
  (setq-local truncate-lines nil
              show-trailing-whitespace nil
              indicate-empty-lines nil)
  (display-line-numbers-mode -1)
  (when (fboundp 'hl-line-mode)
    (hl-line-mode -1)))


;;; Sections

(load (expand-file-name "my-dashboard-calendar.el" my-dashboard--directory))
(load (expand-file-name "my-dashboard-tasks.el" my-dashboard--directory))
(load (expand-file-name "my-dashboard-projects.el" my-dashboard--directory))

;;; my-dashboard.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
