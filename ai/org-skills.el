;;; org-skills.el --- Run org repo Claude skills as Emacs commands -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Exposes the Claude Code skills defined in the org notes repo
;; (`org-skills-repo'/.claude/skills/) as ordinary Emacs commands, so that a
;; skill normally invoked as "/reindex" inside the Claude TUI can be invoked
;; with `M-x skill-reindex'.
;;
;; The skills are discovered when this file is loaded: each subdirectory of
;; .claude/skills containing a SKILL.md becomes a command named
;; skill-<name>, where <name> comes from the SKILL.md front matter (falling
;; back to the directory name).  Skills marked `user-invocable: false' are
;; skipped.  Use `M-x skill-refresh' after adding or renaming a skill.
;;
;; Running a command finds - or starts - a Claude session rooted at the org
;; repo, types the slash command into it, and pops to that buffer, so the
;; whole interaction happens in Emacs and stays available for follow-up
;; questions.
;;
;; Entry points:
;;   M-x skill-<name>   run that skill (prompts for arguments when the skill
;;                      declares an `argument-hint'; C-u forces a prompt)
;;   M-x skill-run      pick a skill with completion
;;   M-x skill-refresh  re-scan the skills directory
;;   M-x skill-list     show the discovered skills in a help buffer
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'claude-code)

;;; Customization

(defgroup org-skills nil
  "Run Claude Code skills from the org notes repo as Emacs commands."
  :group 'tools
  :prefix "org-skills-")

(defcustom org-skills-repo (expand-file-name "~/Documents/org/")
  "Root of the repository whose Claude skills are exposed as commands."
  :type 'directory
  :group 'org-skills)

(defcustom org-skills-command-prefix "skill-"
  "Prefix for the generated command names.
With the default value the /reindex skill is invoked as `skill-reindex'."
  :type 'string
  :group 'org-skills)

(defcustom org-skills-startup-timeout 30
  "Seconds to wait for a freshly started Claude session to show its prompt."
  :type 'number
  :group 'org-skills)

(defcustom org-skills-startup-settle 1.0
  "Extra seconds to wait after Claude's prompt appears, before typing.
Increase this if the first slash command of a session is garbled."
  :type 'number
  :group 'org-skills)

(defvar org-skills-alist nil
  "Skills discovered in `org-skills-repo'.
Each entry is a plist with keys :name, :description, :argument-hint
and :directory.")

;;; SKILL.md front matter parsing

(defun org-skills--strip-quotes (string)
  "Remove surrounding single or double quotes from STRING."
  (let ((s (string-trim string)))
    (if (and (> (length s) 1)
             (memq (aref s 0) '(?\" ?'))
             (eq (aref s 0) (aref s (1- (length s)))))
        (substring s 1 -1)
      s)))

(defun org-skills--parse-front-matter (file)
  "Parse the YAML front matter of FILE into an alist of (KEY . VALUE) strings.
Only the flat scalar keys are understood, including block scalars
introduced by `>' or `|', whose continuation lines are joined with
spaces.  Return nil when FILE has no front matter."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (looking-at-p "^---[ \t]*$")
      (forward-line 1)
      (let ((alist nil)
            (key nil)
            (folded nil)
            (done nil))
        (while (and (not done) (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ;; End of the front matter block.
             ((string-match-p "^---[ \t]*$" line)
              (setq done t))
             ;; A new "key: value" pair at column 0.
             ((string-match "^\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)$" line)
              (let ((k (match-string 1 line))
                    (v (string-trim (match-string 2 line))))
                (setq key k
                      folded (member v '(">" "|" ">-" "|-" ">+" "|+")))
                (push (cons k (if folded "" (org-skills--strip-quotes v))) alist)))
             ;; A continuation line of the current key.
             ((and key (string-match-p "^[ \t]+[^ \t]" line))
              (let ((cell (assoc key alist)))
                (setcdr cell (string-trim (concat (cdr cell) " "
                                                  (string-trim line))))))
             ;; Blank line inside a block scalar: keep the key active.
             ((string-blank-p line) nil)
             (t (setq key nil folded nil))))
          (forward-line 1))
        (nreverse alist)))))

;;; Discovery

(defun org-skills--skills-directory ()
  "Return the .claude/skills directory of `org-skills-repo', or nil."
  (let ((dir (expand-file-name ".claude/skills" org-skills-repo)))
    (and (file-directory-p dir) dir)))

(defun org-skills--read-skill (directory)
  "Return a skill plist for DIRECTORY, or nil if it holds no usable skill.
A skill is usable when it contains a SKILL.md that is not explicitly
marked `user-invocable: false'."
  (let ((skill-md (expand-file-name "SKILL.md" directory)))
    (when (file-readable-p skill-md)
      (let* ((front (org-skills--parse-front-matter skill-md))
             (invocable (cdr (assoc "user-invocable" front))))
        ;; `user-invocable' defaults to true; only an explicit false opts out.
        (unless (and invocable (member (downcase invocable) '("false" "no")))
          (list :name (or (cdr (assoc "name" front))
                          (file-name-nondirectory (directory-file-name directory)))
                :description (or (cdr (assoc "description" front)) "")
                :argument-hint (cdr (assoc "argument-hint" front))
                :directory directory))))))

(defun org-skills--discover ()
  "Scan `org-skills-repo' and return the list of skill plists, sorted by name."
  (if-let ((skills-dir (org-skills--skills-directory)))
      (sort (delq nil
                  (mapcar (lambda (dir)
                            (and (file-directory-p dir)
                                 (org-skills--read-skill dir)))
                          (directory-files skills-dir t "\\`[^.]")))
            (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))
    (message "org-skills: no .claude/skills directory under %s" org-skills-repo)
    nil))

;;; Talking to the Claude session

(defun org-skills--claude-buffer ()
  "Return a live Claude buffer rooted at `org-skills-repo', starting one if needed."
  (let ((dir (file-name-as-directory (expand-file-name org-skills-repo))))
    (or (car (claude-code--find-claude-buffers-for-directory dir))
        (progn
          (message "org-skills: starting Claude in %s..." dir)
          ;; `claude-code--start' derives its working directory from
          ;; `claude-code--directory', which prefers the current buffer's
          ;; project.  Override it so the session is always rooted at the org
          ;; repo, whatever buffer the command was invoked from.
          (cl-letf (((symbol-function 'claude-code--directory) (lambda () dir)))
            (let ((default-directory dir))
              (claude-code--start nil nil)))
          (let ((buffer (car (claude-code--find-claude-buffers-for-directory dir))))
            (when buffer (org-skills--wait-for-prompt buffer))
            buffer)))))

(defun org-skills--wait-for-prompt (buffer)
  "Wait until the Claude session in BUFFER looks ready for input.
Poll for at most `org-skills-startup-timeout' seconds, then pause for
`org-skills-startup-settle' seconds.  Return non-nil if the prompt was
actually seen."
  (let ((deadline (+ (float-time) org-skills-startup-timeout))
        (ready nil))
    (while (and (not ready)
                (buffer-live-p buffer)
                (< (float-time) deadline))
      (setq ready (with-current-buffer buffer
                    (save-excursion
                      (goto-char (point-min))
                      ;; The TUI draws its input box as "│ > " once it is ready.
                      (re-search-forward "^[│|][ \t]*>" nil t))))
      (unless ready (sit-for 0.2)))
    (if ready
        (sit-for org-skills-startup-settle)
      (message "org-skills: Claude prompt not seen after %ss, sending anyway"
               org-skills-startup-timeout))
    ready))

(defun org-skills--send (command)
  "Type COMMAND into the org repo's Claude session and submit it.
COMMAND is a full slash command such as \"/reindex\"."
  (let ((buffer (org-skills--claude-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "Could not start Claude in %s" org-skills-repo))
    (with-current-buffer buffer
      ;; The trailing space closes the slash-command completion popup, so that
      ;; the following RET submits the command instead of merely accepting the
      ;; highlighted completion.
      (claude-code--term-send-string claude-code-terminal-backend
                                     (concat command " "))
      (sit-for 0.3)
      (claude-code--term-send-string claude-code-terminal-backend (kbd "RET")))
    (pop-to-buffer buffer)
    buffer))

;;; Commands

(defun org-skills--command-symbol (skill)
  "Return the command symbol for SKILL."
  (intern (concat org-skills-command-prefix (plist-get skill :name))))

(defun org-skills--docstring (skill)
  "Return a docstring for SKILL's command."
  (let ((description (plist-get skill :description))
        (hint (plist-get skill :argument-hint)))
    (concat (format "Run the /%s Claude skill in %s.\n"
                    (plist-get skill :name) org-skills-repo)
            (unless (string-empty-p description)
              (concat "\n" (string-trim description) "\n"))
            (when hint (format "\nArguments: %s\n" hint))
            "\nWith prefix ARG, prompt for arguments even when the skill\ndeclares none.")))

(defun org-skills--read-arguments (skill)
  "Read arguments for SKILL from the minibuffer.  Return a string, possibly empty."
  (string-trim
   (read-string (format "/%s arguments%s: "
                        (plist-get skill :name)
                        (if-let ((hint (plist-get skill :argument-hint)))
                            (format " (%s)" hint)
                          ""))
                nil 'org-skills--argument-history)))

(defun org-skills-invoke (skill &optional arg)
  "Send SKILL to the org repo's Claude session.
SKILL is a plist from `org-skills-alist'.  Arguments are read from the
minibuffer when the skill declares an `argument-hint' or when ARG is
non-nil."
  (let* ((prompt-for-args (or arg (plist-get skill :argument-hint)))
         (arguments (if prompt-for-args (org-skills--read-arguments skill) ""))
         (command (concat "/" (plist-get skill :name)
                          (unless (string-empty-p arguments)
                            (concat " " arguments)))))
    (org-skills--send command)))

(defun org-skills--define-command (skill)
  "Define the interactive command for SKILL and return its symbol."
  (let ((symbol (org-skills--command-symbol skill)))
    (defalias symbol
      (lambda (&optional arg)
        (interactive "P")
        (org-skills-invoke skill arg))
      (org-skills--docstring skill))
    symbol))

;;;###autoload
(defun org-skills-refresh ()
  "Re-scan `org-skills-repo' and (re)define one command per skill."
  (interactive)
  (setq org-skills-alist (org-skills--discover))
  (mapc #'org-skills--define-command org-skills-alist)
  (when (called-interactively-p 'interactive)
    (message "org-skills: %d skill%s available (%s)"
             (length org-skills-alist)
             (if (= 1 (length org-skills-alist)) "" "s")
             (mapconcat (lambda (skill)
                          (symbol-name (org-skills--command-symbol skill)))
                        org-skills-alist ", ")))
  org-skills-alist)

;;;###autoload
(defun org-skills-run (name &optional arg)
  "Pick a skill by NAME with completion and run it.
With prefix ARG, prompt for arguments even when the skill declares none."
  (interactive
   (list (completing-read "Skill: "
                          (mapcar (lambda (skill) (plist-get skill :name))
                                  (or org-skills-alist (org-skills-refresh)))
                          nil t)
         current-prefix-arg))
  (if-let ((skill (cl-find name org-skills-alist
                           :key (lambda (s) (plist-get s :name))
                           :test #'string=)))
      (org-skills-invoke skill arg)
    (user-error "Unknown skill: %s" name)))

;;;###autoload
(defun org-skills-list ()
  "Display the discovered skills and their commands in a help buffer."
  (interactive)
  (unless org-skills-alist (org-skills-refresh))
  (with-help-window "*Org Skills*"
    (princ (format "Claude skills in %s\n\n" org-skills-repo))
    (dolist (skill org-skills-alist)
      (princ (format "M-x %s\n" (org-skills--command-symbol skill)))
      (when-let ((hint (plist-get skill :argument-hint)))
        (princ (format "    arguments: %s\n" hint)))
      (let ((description (string-trim (plist-get skill :description))))
        (unless (string-empty-p description)
          (princ (format "    %s\n"
                         (truncate-string-to-width description 100 nil nil t)))))
      (princ "\n"))))

;; Shorter aliases, matching the skill-* naming of the generated commands.
(defalias 'skill-refresh #'org-skills-refresh)
(defalias 'skill-run #'org-skills-run)
(defalias 'skill-list #'org-skills-list)

;; Discover the skills at load time.
(org-skills-refresh)

(provide 'org-skills)

;;; org-skills.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
