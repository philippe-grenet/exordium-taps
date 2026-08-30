;;;; my-review-server.el --- Hand a local review off to a waiting agent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Lets a coding agent ask for a review and block until it is done, with no
;; copy-paste in between.  The agent runs
;;
;;   emacsclient /tmp/emacs-review-XXXX.md
;;
;; which blocks.  Emacs recognises the file, opens `my/review-local-changes'
;; for the repository named in it, and when you finish with C-c C-d the
;; comments are written into that file and the client is released.  The agent
;; reads the file and gets to work.
;;
;; How the blocking works: `emacsclient FILE' waits until every buffer it
;; opened is marked done.  C-x # (`server-edit') is the interactive way to do
;; that; `server-done' is the same thing from Lisp, so the review buffer can
;; release the agent itself.
;;
;; Note that `server-visit-hook' runs before `server-buffer-clients' is set
;; (server.el), so the request file has to be recognised by name, not by asking
;; whether the buffer belongs to a client.
;;
;; Safety valve: killing the handoff buffer also releases the client, via
;; `server-kill-buffer'.  An abandoned review cannot leave an agent hanging;
;; the file still carries its AWAITING-REVIEW marker, which is how the agent
;; tells "no comments" from "never reviewed".

;;; Code:

(require 'server)
(require 'cl-lib)

(defconst my/emacs-review-file-regexp "/emacs-review-[^/]*\\.md\\'"
  "Files matching this are review requests from an agent.")

(defconst my/emacs-review-awaiting-marker "<!-- AWAITING-REVIEW -->"
  "Marker an agent writes into a request, and that finishing removes.
If it is still there when the client is released, the review was abandoned
rather than submitted with no comments.")

(defvar my/emacs-review--handoff nil
  "Buffer an agent is blocked on, or nil.")

(defvar my/emacs-review--review nil
  "Review buffer opened for the waiting agent, or nil.")

(declare-function my/review-local-changes "my-diff-review" (&optional prompt-p))
(declare-function my-diff-review--markdown "my-diff-review" ())
(defvar my-diff-review-mode-map)
(defvar pr-review--pending-review-threads)

(defun my/emacs-review--field (name)
  "Return the value of `NAME:' in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^%s:[ \t]*\\(.+\\)$" (regexp-quote name))
                             nil t)
      (string-trim (match-string 1)))))

(defun my/emacs-review--visit ()
  "Turn a review request file into a review buffer.
Runs from `server-visit-hook'."
  (when (and buffer-file-name
             (string-match-p my/emacs-review-file-regexp buffer-file-name))
    (let ((repo (my/emacs-review--field "repo"))
          (handoff (current-buffer)))
      (cond
       ((not (and repo (file-directory-p repo)))
        (message "emacs-review: request names no usable repo, leaving it alone"))
       ((and my/emacs-review--handoff (buffer-live-p my/emacs-review--handoff))
        (message "emacs-review: a review is already pending; finish it first"))
       (t
        (setq my/emacs-review--handoff handoff)
        ;; `save-current-buffer' is load-bearing.  `server-visit-files' sets
        ;; `server-buffer-clients' *after* this hook returns, in whatever
        ;; buffer is current by then, and displaying the review makes the
        ;; review buffer current.  Without this the client attaches to the
        ;; review buffer instead of the handoff file, and `server-done' on the
        ;; handoff never releases the agent.  The review still ends up
        ;; displayed; only the current buffer is restored.
        (save-current-buffer
          (my/emacs-review--open repo)))))))

(defun my/emacs-review--open (repo)
  "Open a review of REPO for a waiting agent."
  ;; An agent-initiated review starts clean, so answer the "discard existing
  ;; comments?" prompt for it.  Nothing must block here: a `yes-or-no-p' nobody
  ;; is looking at would wedge Emacs while the agent waits on it.
  (let ((default-directory (file-name-as-directory repo)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (my/review-local-changes)))
  (setq my/emacs-review--review (current-buffer)))

(defun my/emacs-review--switch ()
  "Show the review rather than the request file.
`server-switch-buffer' displays the client's buffer once visiting is done,
which happens after `server-visit-hook' and would otherwise leave the raw
request file on screen -- in `markdown-mode', where none of the review keys
mean anything."
  (when (and my/emacs-review--handoff
             (eq (current-buffer) my/emacs-review--handoff)
             (buffer-live-p my/emacs-review--review))
    (switch-to-buffer my/emacs-review--review)
    (message
     (substitute-command-keys
      (concat "Agent is waiting. Comment, then "
              "\\<my-diff-review-mode-map>\\[my/emacs-review-finish] to send, "
              "\\[my/emacs-review-abort] to abandon.")))))

(add-hook 'server-switch-hook #'my/emacs-review--switch)

(add-hook 'server-visit-hook #'my/emacs-review--visit)

(defun my/emacs-review-finish ()
  "Write this review's comments to the waiting agent and release it."
  (interactive)
  (unless (and my/emacs-review--handoff
               (buffer-live-p my/emacs-review--handoff))
    (user-error "No agent is waiting on this review"))
  (let ((markdown (my-diff-review--markdown))
        (count (length pr-review--pending-review-threads))
        (review (current-buffer)))
    (with-current-buffer my/emacs-review--handoff
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert markdown))
      (save-buffer)
      ;; Releases the emacsclient process.  The buffer is not modified after
      ;; the save, so this does not prompt.
      (server-done))
    (setq my/emacs-review--handoff nil
          my/emacs-review--review nil)
    (when (buffer-live-p review)
      (with-current-buffer review
        ;; The comments have been delivered, so drop them.  Leaving them behind
        ;; means the next review of this repository asks whether to discard
        ;; them -- and a `yes-or-no-p' nobody is watching would wedge the whole
        ;; Emacs instance while an agent waits on it.
        (setq-local pr-review--pending-review-threads nil)
        (quit-window)))
    (message "Sent %d comment%s to the agent" count (if (= 1 count) "" "s"))))

(defun my/emacs-review-abort ()
  "Release the waiting agent without sending anything.
The request keeps its AWAITING-REVIEW marker, so the agent can tell an
abandoned review from one submitted with no comments."
  (interactive)
  (unless (and my/emacs-review--handoff
               (buffer-live-p my/emacs-review--handoff))
    (user-error "No agent is waiting on this review"))
  (when (yes-or-no-p "Abandon this review and send the agent nothing? ")
    (let ((handoff my/emacs-review--handoff))
      (setq my/emacs-review--handoff nil
            my/emacs-review--review nil)
      ;; Killing the buffer releases the client, via `server-kill-buffer'.
      (kill-buffer handoff))
    (message "Review abandoned; agent released")))

;; C-c C-d and C-c C-q are both keys `my-diff-review-mode' explicitly unbinds
;; (ediff and request-reviewers in pr-review, neither of which works without a
;; forge), so they are free for "done" and "quit".
(with-eval-after-load 'my-diff-review
  (define-key my-diff-review-mode-map (kbd "C-c C-d") #'my/emacs-review-finish)
  (define-key my-diff-review-mode-map (kbd "C-c C-q") #'my/emacs-review-abort))

(provide 'my-review-server)

;;; my-review-server.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
