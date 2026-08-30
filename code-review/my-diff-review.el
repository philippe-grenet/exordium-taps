;;;; my-diff-review.el --- Review local changes, hand the comments to an agent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Review uncommitted changes -- typically written by a coding agent -- line by
;; line in Emacs, then export all the comments as markdown to hand back to the
;; agent.  Several terminal tools do the same thing (tuicr, for one); this one
;; keeps the review in Emacs, with Emacs keys.
;;
;; Entry point: `my/review-local-changes' (bound to C-c M-l).
;;
;;   Key        Action
;;   ---------- --------------------------------------------------------------
;;   C-c C-c    Comment, or re-edit the comment already under point.  What you
;;              get depends on where point is:
;;                - on an existing comment      -> re-open it for editing,
;;                                                 pre-filled with its text
;;                - on a diff line (or region)  -> comment on that line/range
;;                - on a file heading or hunk   -> file-level comment
;;                - in the buffer header        -> review-level comment
;;              The input buffer's header states the exact target, e.g.
;;              "Comment on foo.el:509-512."
;;   C-c C-k    Delete the comment at point
;;   C-c C-w    Copy every comment as markdown to the kill ring
;;   C-c C-s    Save every comment as markdown to a file, defaulting to
;;              `my-diff-review-output-file-name' (".diff-review.md") in the
;;              repository root
;;   C-c C-r    Re-run the diff, keeping the comments already written
;;   C-c C-v    Visit the working-tree file at the line under point
;;   C-c C-f    Jump to a file in the diff
;;   q          Quit
;;
;; In the comment input buffer: C-c C-c sends, C-c C-k aborts, and C-c C-i
;; inserts the lines being commented on as a ```suggestion block.
;;
;; This reuses three pieces of pr-review that turn out to have no dependency on
;; the GitHub API:
;;
;;   `pr-review--insert-diff'   -- unified diff string -> rendered buffer, with
;;                                 every line tagged with (path . line)
;;   `pr-review-add-pending-review-thread'
;;                              -- reads that tag, opens an input buffer, and
;;                                 renders the comment inline under the line
;;   `pr-review--pending-review-threads'
;;                              -- buffer-local list of those comments
;;
;; Everything GitHub-specific lives in the submit path, which we simply never
;; call.  Instead the comment list is formatted as markdown.
;;
;; WARNING: all three are private (`--') functions.  pr-review owes us no
;; stability there, so one of its updates could break this module -- most
;; likely with a void-function error the first time C-c M-l is pressed after an
;; upgrade, or a diff that renders without the (path . line) tags so comments
;; cannot be anchored.
;;
;; To cut the dependency, copy the bodies of those three into this file under
;; `my-diff-review--' names and drop the `require' of pr-review.  Only
;; `pr-review--insert-diff' is much work, at roughly 50 lines, and most of that
;; is a loop over magit's washed diff attaching the line tags.  That also means
;; giving up pr-review's comment rendering and input buffer, which are the
;; other things reused here -- so it is a last resort, not a planned migration.
;; Pinning the installed pr-review version is the cheaper first response.
;;
;; Untracked files are included: an agent that creates new files should have
;; those reviewable too.  They are diffed against /dev/null and the synthetic
;; header is rewritten so magit's diff washer treats them as normal additions.

;;; Code:

(require 'pr-review)
(require 'magit-git)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup my-diff-review nil
  "Review local changes and export the comments as markdown."
  :group 'tools)

(defcustom my-diff-review-output-file-name ".diff-review.md"
  "Default file name, relative to the repository root, for saved comments."
  :type 'string
  :group 'my-diff-review)

(defcustom my-diff-review-include-untracked t
  "When non-nil, include untracked files in the reviewed diff."
  :type 'boolean
  :group 'my-diff-review)

(defconst my-diff-review--ranges
  '(("uncommitted (staged + unstaged)" . ("HEAD"))
    ("unstaged only"                   . ())
    ("staged only"                     . ("--cached"))
    ("against a ref..."                . ref))
  "Alist of human label -> extra arguments for `git diff'.
The value `ref' means prompt for a revision.")

(defvar-local my-diff-review--repo nil
  "Absolute path of the repository root being reviewed.")

(defvar-local my-diff-review--args nil
  "Extra `git diff' arguments used to build this buffer.")

(defvar-local my-diff-review--label nil
  "Human-readable description of what is being reviewed.")

(defvar-local my-diff-review--header-end nil
  "Position just after the buffer header; where review-level comments go.")

(defvar-local my-diff-review--suggestion-source nil
  "In an input buffer, the source lines the comment is attached to.
Used by `my-diff-review-insert-suggestion'.")


;;; Building the diff

(defun my-diff-review--git-output (repo &rest args)
  "Run git with ARGS in REPO and return stdout as a string."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory repo)))
      (apply #'call-process "git" nil t nil args))
    (buffer-string)))

(defun my-diff-review--untracked-files (repo)
  "Return the untracked, not-ignored files in REPO, relative to its root.
Excludes `my-diff-review-output-file-name', so saving the review into the
repository does not make the next review include it."
  (seq-remove
   (lambda (file) (equal file my-diff-review-output-file-name))
   (split-string
    (my-diff-review--git-output repo "ls-files" "--others" "--exclude-standard")
    "\n" t)))

(defun my-diff-review--untracked-diff (repo file)
  "Return a unified diff adding FILE (relative to REPO) in full.
With --no-prefix, `git diff --no-index' against /dev/null already emits a
header naming FILE on both sides, so the result needs no fixing up."
  (my-diff-review--git-output
   repo "diff" "--no-color" "--no-prefix" "--no-index" "--" "/dev/null" file))

(defun my-diff-review--build-diff (repo args)
  "Return the unified diff for REPO with extra `git diff' ARGS.
Appends synthetic diffs for untracked files when
`my-diff-review-include-untracked' is non-nil and ARGS does not restrict
the diff to the index.

--no-prefix matters: magit always passes it to git, so its diff washer
expects file names without the a/ and b/ prefixes.  Leaving them in makes
every file heading render as \"a/foo -> b/foo\"."
  (concat
   (apply #'my-diff-review--git-output
          repo "diff" "--no-color" "--no-prefix" args)
   (when (and my-diff-review-include-untracked
              (not (member "--cached" args)))
     (mapconcat (lambda (file) (my-diff-review--untracked-diff repo file))
                (my-diff-review--untracked-files repo)
                ""))))


;;; Rendering

(defun my-diff-review--normalize-path (path)
  "Strip a leading a/ or b/ prefix from PATH."
  (if (and path (string-match "\\`[ab]/" path))
      (substring path 2)
    path))

(defun my-diff-review--insert-header ()
  "Insert the buffer header.
Also guarantees the diff does not start at `point-min': the annotation loop
in `pr-review--insert-diff' steps back one line before it starts, so a diff
flush against the top of the buffer loses its first file name."
  (insert (propertize (format "Review: %s\n" my-diff-review--label)
                      'face 'bold)
          (format "Repo:   %s\n" my-diff-review--repo)
          (substitute-command-keys
           (concat "\\<my-diff-review-mode-map>"
                   "\\[my-diff-review-comment] comment  "
                   "\\[my-diff-review-delete-comment] delete  "
                   "\\[my-diff-review-copy-comments] copy markdown  "
                   "\\[my-diff-review-save-comments] save  "
                   "\\[my-diff-review-refresh] refresh  "
                   "\\[quit-window] quit\n"))
          "\n"))

(defun my-diff-review--insert-comment-at-point (thread)
  "Insert THREAD's body at point and propertize it as a pending comment.
Mirrors what `pr-review--insert-in-diff-pending-review-thread' does for
line comments, so `my-diff-review-delete-comment' and editing work the
same way on all three kinds of comment."
  (let ((beg (point)))
    (insert (propertize (format "> PENDING %s comment\n"
                                (if (alist-get 'path thread) "file" "review"))
                        'face 'pr-review-in-diff-pending-begin-face))
    (pr-review--insert-fontified (alist-get 'body thread) 'gfm-mode nil
                                 'pr-review-in-diff-pending-body-face)
    (insert (propertize " \n" 'face 'pr-review-in-diff-pending-end-face))
    (add-text-properties beg (point)
                         (list 'pr-review-pending-review-thread thread))))

(defun my-diff-review--insert-thread (thread)
  "Render THREAD in the buffer, at a place that matches its scope."
  (let-alist thread
    (cond
     ;; Review-level: no file, no line.  Goes just under the header.
     ((null .path)
      (save-excursion
        (goto-char my-diff-review--header-end)
        (my-diff-review--insert-comment-at-point thread)))
     ;; File-level: a file but no line.  Goes right under the file heading.
     ((null .line)
      (save-excursion
        (if-let* ((section (pr-review--find-section-with-value .path)))
            (progn
              (goto-char (oref section start))
              (forward-line)
              (my-diff-review--insert-comment-at-point thread))
          ;; File is gone after a refresh; keep the comment rather than lose it.
          (goto-char my-diff-review--header-end)
          (my-diff-review--insert-comment-at-point thread))))
     (t
      (pr-review--insert-in-diff-pending-review-thread thread t)))))

(defun my-diff-review--disable-painting (section)
  "Stop magit repainting SECTION and its children when point enters them.

Magit repaints the section under point, rewriting the face of every line
between the section's start and end with its own magit-diff-* faces.  Two
things go wrong here: the diff stops using the diff-mode faces the theme
styles, and -- worse -- comments rendered inside a hunk fall within that
range and get painted as if they were diff lines.

`magit-section-update-highlight' only repaints sections whose `painted'
slot is bound, so unbinding it opts out."
  (when (slot-boundp section 'painted)
    (slot-makeunbound section 'painted))
  (mapc #'my-diff-review--disable-painting (oref section children)))

(defun my-diff-review--render ()
  "(Re)render the current review buffer, preserving existing comments."
  (let ((inhibit-read-only t)
        (threads (reverse pr-review--pending-review-threads))
        (diff (my-diff-review--build-diff my-diff-review--repo
                                          my-diff-review--args)))
    (erase-buffer)
    (setq-local pr-review--pending-review-threads nil)
    (my-diff-review--insert-header)
    (setq-local my-diff-review--header-end (point))
    (if (string-empty-p (string-trim diff))
        (insert (propertize "No changes.\n" 'face 'italic))
      (magit-insert-section (my-diff-review-root)
        (pr-review--insert-diff diff))
      (my-diff-review--disable-painting magit-root-section))
    ;; Re-anchor comments carried over from the previous render.  Threads whose
    ;; anchor no longer exists fall back rather than being dropped silently.
    (dolist (thread threads)
      (my-diff-review--insert-thread thread)
      (push thread pr-review--pending-review-threads))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))


;;; Commands

(defun my-diff-review--anchor ()
  "Return the position that a new comment should be resolved against.
With an active region this is the last selected character, not point:
selecting whole lines leaves point at the start of the *following* line,
which may belong to another file's section entirely.  This matches how
`pr-review--get-review-thread-input-at-current-point' reads the region."
  (if (use-region-p) (1- (region-end)) (point)))

(defun my-diff-review--file-at-point ()
  "Return the file name of the diff section at the comment anchor, or nil."
  (save-excursion
    (goto-char (my-diff-review--anchor))
    (when-let* ((section (magit-current-section)))
      (cl-loop for s = section then (oref s parent)
               while s
               when (magit-file-section-p s) return (oref s value)))))

(defun my-diff-review--add-scoped-comment (thread prompt &optional source initial)
  "Open an input buffer for THREAD, described by PROMPT, and render it.
SOURCE, when given, is the text THREAD refers to; it is made available to
`my-diff-review-insert-suggestion'.  INITIAL, when given, is inserted into
the input buffer."
  (pr-review--open-input-buffer
   prompt
   (lambda ()
     (setq-local my-diff-review--suggestion-source source)
     (local-set-key (kbd "C-c C-i") #'my-diff-review-insert-suggestion)
     (when initial
       (insert initial)
       (goto-char (point-min))))
   (let ((buffer (current-buffer)))
     (lambda (body)
       (setf (alist-get 'body thread) body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (my-diff-review--insert-thread thread))
           (push thread pr-review--pending-review-threads)))))))

(defun my-diff-review-insert-suggestion ()
  "Insert the lines being commented on as a ```suggestion block.
Only available in the input buffer of a comment on one or more diff lines."
  (interactive)
  (unless my-diff-review--suggestion-source
    (user-error "Nothing to suggest: this comment is not on diff lines"))
  (let* ((source my-diff-review--suggestion-source)
         (fence (my-diff-review--fence source)))
    (insert fence "suggestion\n"
            source (unless (string-suffix-p "\n" source) "\n")
            fence "\n")))

(defun my-diff-review--edit-comment (thread)
  "Re-open THREAD for editing, then re-render it in place."
  (if-let* ((end (next-single-property-change
                  (point) 'pr-review-pending-review-thread))
            (beg (previous-single-property-change
                  end 'pr-review-pending-review-thread)))
      (progn
        (let ((inhibit-read-only t))
          (delete-region beg end))
        (setq-local pr-review--pending-review-threads
                    (delq thread pr-review--pending-review-threads))
        (my-diff-review--add-scoped-comment
         thread
         (format "Edit comment on %s." (my-diff-review--thread-location thread))
         (and (alist-get 'line thread) (my-diff-review--thread-source thread))
         (alist-get 'body thread)))
    (user-error "Cannot locate the comment to edit")))

(defun my-diff-review-comment ()
  "Add or edit a comment.  What you get depends on where point is.

On a diff line, comment on that line.  With an active region, comment on
every line the region touches -- note that selecting whole lines leaves
point on the line *after* the last one you want, as everywhere in Emacs.
The input buffer's header states the exact range either way, and
\\<my-diff-review-mode-map>\\[my-diff-review-insert-suggestion] there
inserts those lines as a ```suggestion block.

On an existing comment, edit it.  On a file heading or hunk header,
comment on the file as a whole.  In the buffer header, comment on the
whole review."
  (interactive)
  (let ((existing (and (not (use-region-p))
                       (get-text-property
                        (point) 'pr-review-pending-review-thread))))
    (cond
     (existing
      (my-diff-review--edit-comment existing))
     ;; One or more diff lines.  Build the thread ourselves rather than going
     ;; through `pr-review-add-pending-review-thread', which pre-fills a
     ;; suggestion block only when a region happens to be active -- so marking
     ;; a line without moving produced an empty buffer and no indication of
     ;; what was being commented on.
     ((pr-review--get-diff-line-info (my-diff-review--anchor))
      (let* ((thread (pr-review--get-review-thread-input-at-current-point))
             (source (and thread (my-diff-review--thread-source thread))))
        (unless thread
          (user-error "Select lines within a single file"))
        (deactivate-mark)
        (my-diff-review--add-scoped-comment
         thread
         (format "Comment on %s." (my-diff-review--thread-location thread))
         source)))
     ;; Inside a file's diff but not on a numbered line: file heading or hunk.
     ((my-diff-review--file-at-point)
      (let ((path (my-diff-review--file-at-point)))
        (my-diff-review--add-scoped-comment
         (list (cons 'path path) (cons 'line nil))
         (format "Comment on %s as a whole." path))))
     ;; Header area: a comment about the change set as a whole.
     (t
      (my-diff-review--add-scoped-comment
       (list (cons 'path nil) (cons 'line nil))
       "Comment on the whole review.")))))

(defun my-diff-review-delete-comment ()
  "Delete the comment at point."
  (interactive)
  (if-let* ((thread (get-text-property (point) 'pr-review-pending-review-thread))
            (end (next-single-property-change (point) 'pr-review-pending-review-thread))
            (beg (previous-single-property-change end 'pr-review-pending-review-thread)))
      (progn
        (let ((inhibit-read-only t))
          (delete-region beg end))
        (setq-local pr-review--pending-review-threads
                    (delq thread pr-review--pending-review-threads))
        (message "Comment deleted (%d left)"
                 (length pr-review--pending-review-threads)))
    (user-error "No comment at point")))

(defun my-diff-review-refresh ()
  "Re-run the diff, keeping the comments already written."
  (interactive)
  (my-diff-review--render)
  (message "Refreshed (%d comment%s kept)"
           (length pr-review--pending-review-threads)
           (if (= 1 (length pr-review--pending-review-threads)) "" "s")))

(defun my-diff-review-visit-file ()
  "Visit the working-tree file for the diff line at point."
  (interactive)
  (if-let* ((info (pr-review--get-diff-line-info (point)))
            (path (my-diff-review--normalize-path (cadr info)))
            (file (expand-file-name path my-diff-review--repo)))
      (progn
        (find-file-other-window file)
        (when (equal (car info) "RIGHT")
          (goto-char (point-min))
          (forward-line (1- (cddr info)))))
    (user-error "Point is not on a diff line")))


;;; Markdown export

(defconst my-diff-review--fence-languages
  '(("el"       . "elisp")
    ("py"       . "python")
    ("h"        . "cpp")
    ("hpp"      . "cpp")
    ("cc"       . "cpp")
    ("cpp"      . "cpp")
    ("js"       . "javascript")
    ("ts"       . "typescript")
    ("yml"      . "yaml")
    ("yaml"     . "yaml")
    ("sh"       . "bash")
    ("bash"     . "bash")
    ("zsh"      . "zsh")
    ("html"     . "html")
    ("md"       . "markdown")
    ("markdown" . "markdown")
    ("org"      . "org"))
  "Extension -> markdown fence language, where the two differ.
Extensions not listed are used verbatim, which is already right for html,
xml, yaml, json, css and most others, so only add an entry when the tag has
to differ from the extension.")

(defun my-diff-review--fence-language (path)
  "Return a markdown fence language tag for PATH, or the empty string.
Files with no extension -- shell functions in a dotfiles repo, say -- get an
empty tag, which is a valid unhighlighted fence."
  (let ((ext (downcase (or (file-name-extension (or path "")) ""))))
    (or (cdr (assoc ext my-diff-review--fence-languages))
        ext)))

(defun my-diff-review--thread-source (thread)
  "Return the source text THREAD refers to, or nil if it cannot be located."
  (save-excursion
    (let-alist thread
      (let ((start-line (or .startLine .line))
            (side (or .startSide .side)))
        (when (pr-review--goto-diff-line .path side start-line)
          (let* ((beg (line-beginning-position))
                 (end (progn
                        (pr-review--goto-diff-line .path .side .line)
                        (line-end-position))))
            (when (<= beg end)
              (replace-regexp-in-string
               (rx line-start (any ?+ ?- ?\s)) ""
               (buffer-substring-no-properties beg end)))))))))

(defun my-diff-review--fence (content)
  "Return a backtick fence long enough to wrap CONTENT.
CommonMark ends a fenced block at the first line whose fence is at least
as long as the opening one, so quoting a file that itself contains ```
needs a longer fence."
  (let ((longest 0))
    (with-temp-buffer
      (insert (or content ""))
      (goto-char (point-min))
      (while (re-search-forward "^[[:blank:]]*\\(`+\\)" nil t)
        (setq longest (max longest (length (match-string 1))))))
    (make-string (max 3 (1+ longest)) ?`)))

(defun my-diff-review--thread-location (thread)
  "Return the markdown heading text for THREAD.
\"General\" for a review-level comment, \"path\" for a file-level one, and
\"path:line\" or \"path:start-end\" for a line comment."
  (let-alist thread
    (let ((path (my-diff-review--normalize-path .path)))
      (cond
       ((null path) "General")
       ((null .line) path)
       ((and .startLine (not (equal .startLine .line)))
        (format "%s:%s-%s" path .startLine .line))
       (t (format "%s:%s" path .line))))))

(defun my-diff-review--sorted-threads ()
  "Return the pending comments sorted for export.
Review-level comments first, then by file; within a file the file-level
comment comes before the line comments."
  (sort (copy-sequence pr-review--pending-review-threads)
        (lambda (a b)
          (let ((pa (alist-get 'path a))
                (pb (alist-get 'path b)))
            (cond
             ((and (null pa) (null pb)) nil)
             ((null pa) t)
             ((null pb) nil)
             ((equal pa pb)
              (< (or (alist-get 'line a) 0) (or (alist-get 'line b) 0)))
             (t (string< pa pb)))))))

(defun my-diff-review--markdown ()
  "Return all comments in the current buffer as a markdown string."
  (let ((threads (my-diff-review--sorted-threads)))
    (concat
     "# Code review comments\n\n"
     (format "Reviewed: %s in `%s`\n\n" my-diff-review--label my-diff-review--repo)
     (if (null threads)
         "No comments.\n"
       (concat
        (format (concat "%d comment%s. Headings are `file:line` in the working "
                        "tree, or `file` for a comment on a whole file, or "
                        "`General` for one on the whole change set.\n\n")
                (length threads) (if (= 1 (length threads)) "" "s"))
        (mapconcat
         (lambda (thread)
           (let* ((path (my-diff-review--normalize-path (alist-get 'path thread)))
                  ;; Only line comments quote source; file- and review-level
                  ;; comments have nothing meaningful to anchor to.
                  (source (and path (alist-get 'line thread)
                               (my-diff-review--thread-source thread)))
                  (fence (and source (my-diff-review--fence source))))
             (concat
              (format "## %s\n\n" (my-diff-review--thread-location thread))
              (when source
                (format "%s%s\n%s\n%s\n\n"
                        fence
                        (my-diff-review--fence-language path)
                        source
                        fence))
              (string-trim (or (alist-get 'body thread) ""))
              "\n")))
         threads
         "\n"))))))

(defun my-diff-review-copy-comments ()
  "Copy all comments as markdown to the kill ring."
  (interactive)
  (let ((markdown (my-diff-review--markdown))
        (count (length pr-review--pending-review-threads)))
    (kill-new markdown)
    (message "Copied %d comment%s as markdown"
             count (if (= 1 count) "" "s"))))

(defun my-diff-review-save-comments (file)
  "Write all comments as markdown to FILE."
  (interactive
   (list (read-file-name
          "Write review comments to: "
          (file-name-as-directory my-diff-review--repo)
          nil nil
          my-diff-review-output-file-name)))
  (let ((markdown (my-diff-review--markdown))
        (count (length pr-review--pending-review-threads)))
    (with-temp-file file
      (insert markdown))
    (message "Wrote %d comment%s to %s"
             count (if (= 1 count) "" "s") (abbreviate-file-name file))))


;;; Mode

(defvar my-diff-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pr-review-mode-map)
    ;; Everything inherited from pr-review that talks to the GitHub API is
    ;; either replaced or removed, so nothing in this buffer can try to reach a
    ;; forge.
    (define-key map (kbd "C-c C-c") #'my-diff-review-comment)
    (define-key map (kbd "C-c C-k") #'my-diff-review-delete-comment)
    (define-key map (kbd "C-c C-w") #'my-diff-review-copy-comments)
    (define-key map (kbd "C-c C-s") #'my-diff-review-save-comments)
    (define-key map (kbd "C-c C-r") #'my-diff-review-refresh)
    (define-key map (kbd "C-c C-v") #'my-diff-review-visit-file)
    (define-key map (kbd "q")       #'quit-window)
    ;; pr-review-goto-file is buffer-local, keep it on C-c C-f.
    (dolist (key '("C-c C-e" "C-c C-d" "C-c C-o" "C-c C-q" "C-c C-l" "C-c C-t"
                   "C-c C-j"))
      (define-key map (kbd key) #'undefined))
    map)
  "Keymap for `my-diff-review-mode'.")

(define-derived-mode my-diff-review-mode pr-review-mode "DiffReview"
  "Major mode for reviewing local changes and exporting the comments."
  :interactive nil
  :group 'my-diff-review
  (use-local-map my-diff-review-mode-map)
  ;; `magit-section-mode' sets `truncate-lines' to t, which hides all but the
  ;; first line of a comment.  Comments matter more here than keeping diff
  ;; lines on one row.
  (setq-local truncate-lines nil
              word-wrap t)
  ;; Magit repaints the section under point with its *-highlight faces, so
  ;; simply moving into a hunk recoloured the whole thing and made comments
  ;; look like part of the diff.  That highlight exists to show what a stage
  ;; or discard would act on; nothing here acts on a section, so it is only
  ;; noise.  Selection highlighting stays on -- it shows what a region
  ;; comment would cover.
  (setq-local magit-section-highlight-current nil))


;;; Entry point

(defun my-diff-review--read-args (prompt-p)
  "Return (LABEL . ARGS) for `git diff'.
When PROMPT-P is non-nil, ask which range to review."
  (if (not prompt-p)
      (cons "uncommitted (staged + unstaged)" '("HEAD"))
    (let* ((label (completing-read "Review: " (mapcar #'car my-diff-review--ranges)
                                   nil t nil nil
                                   "uncommitted (staged + unstaged)"))
           (args (cdr (assoc label my-diff-review--ranges))))
      (if (eq args 'ref)
          (let ((ref (magit-read-branch-or-commit "Diff against")))
            (cons (format "changes since %s" ref) (list ref)))
        (cons label args)))))

;;;###autoload
(defun my/review-local-changes (&optional prompt-p)
  "Review the local changes in the current repository.

Renders the diff in a dedicated buffer where you can comment on individual
lines, then export every comment as markdown for a coding agent to act on.

By default reviews all uncommitted changes (staged and unstaged) plus
untracked files.  With a prefix argument PROMPT-P, choose what to review."
  (interactive "P")
  (let* ((repo (or (magit-toplevel)
                   (user-error "Not inside a Git repository")))
         (spec (my-diff-review--read-args prompt-p))
         (buffer (get-buffer-create
                  (format "*diff review: %s*"
                          (file-name-nondirectory
                           (directory-file-name repo))))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'my-diff-review-mode)
                 pr-review--pending-review-threads
                 (not (yes-or-no-p
                       (format "Discard %d existing comment%s? "
                               (length pr-review--pending-review-threads)
                               (if (= 1 (length pr-review--pending-review-threads))
                                   "" "s")))))
        (user-error "Aborted"))
      (setq default-directory repo)
      (my-diff-review-mode)
      (setq-local my-diff-review--repo repo
                  my-diff-review--args (cdr spec)
                  my-diff-review--label (car spec)
                  pr-review--pending-review-threads nil)
      (my-diff-review--render))
    (pop-to-buffer buffer)))

(provide 'my-diff-review)

;;; my-diff-review.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
