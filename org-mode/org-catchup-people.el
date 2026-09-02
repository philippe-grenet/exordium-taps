;;;; Jump to a person's section in catchup.org -*- lexical-binding: t -*-
;;;
;;; Every person section in the org repo's catchup.org is keyed by an emoji
;;; encoding the relationship: `✨' a senior leader, `🌟' a direct report, `👤'
;;; an individual.  People are either top level, or nested one level below a
;;; `👥' group heading (e.g. `** 👤 Dylan Sams' under `* 👥 Team Data Layer').
;;; Dated meeting entries carry no emoji, so the emoji alone separates people
;;; from their notes.
;;;
;;; Group headings (`👥') and recurring meeting series (`📅') are not offered as
;;; candidates; a group only contributes its name as context for the people
;;; nested under it.  A heading carrying the `CATCHUP_IGNORE' property is left
;;; out too, along with everything below it, since the property is looked up
;;; with inheritance: put it on a group to hide all of its people at once.
;;;
;;;   * 👥 VIP
;;;   :PROPERTIES:
;;;   :CATCHUP_IGNORE: t
;;;   :END:
;;;
;;;   C-c o p : pick a person and jump to their heading.
;;;             With a prefix (C-u C-c o p), open their subtree in an indirect
;;;             buffer instead.
;;;   C-c o c : pick a person and pop up their open discussion items
;;;             (TODO/WORK/WAIT) to raise at the next meeting.
;;;   C-c o n : region over a run of those open items -- move them into a new
;;;             meeting entry dated today, as a plain list.
;;;
;;; The same scan builds the per-person `org-capture' templates; see
;;; `my/org-catchup-capture-templates', called from after-init.el.

(require 'org)

(declare-function my/org-file "before-init")

(defconst my/org-catchup-file (my/org-file "catchup.org")
  "File holding catch-up notes, one section per person.")

(defconst my/org-catchup-todo-file (my/org-file "todo.org")
  "File holding the personal TODO list, for the non-person picker targets.")

(defconst my/org-catchup-person-emojis '("✨" "🌟" "👤")
  "Emojis marking a person heading in `my/org-catchup-file'.")

(defconst my/org-catchup-group-emoji "👥"
  "Emoji marking a group heading, whose people are nested one level below.")

(defconst my/org-catchup-ignore-property "CATCHUP_IGNORE"
  "Property hiding a heading, and its subtree, from the catch-up pickers.
Looked up with inheritance, so setting it on a `👥' group heading excludes
every person nested under it.")

(defconst my/org-catchup--heading-re
  (concat "^\\(\\*\\{1,2\\}\\) \\("
          (regexp-opt (cons my/org-catchup-group-emoji
                            my/org-catchup-person-emojis))
          "\\) *\\(.+?\\) *$")
  "Match a person or group heading: level, emoji, name.
Deliberately does not match `📅' meeting series, nor dated entries, which
carry no emoji.")

(defun my/org-catchup-people ()
  "Return an alist of (DISPLAY . HEADING) for every person in catchup.org.
HEADING is the exact heading text, suitable for
`org-find-exact-headline-in-buffer'.  DISPLAY appends the group name for
people nested under a `👥' heading.  People under a heading carrying
`my/org-catchup-ignore-property' are skipped.  Sorted by name,
case-insensitively."
  (with-current-buffer (find-file-noselect my/org-catchup-file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search nil)
              group candidates)
          (while (re-search-forward my/org-catchup--heading-re nil t)
            (let* ((level   (length (match-string 1)))
                   (emoji   (match-string-no-properties 2))
                   (name    (match-string-no-properties 3))
                   (group-p (string= emoji my/org-catchup-group-emoji)))
              ;; A top-level heading either opens a group or ends the previous one.
              (when (= level 1)
                (setq group (and group-p name)))
              (unless (or group-p
                          ;; Inherited, so one property on the group hides
                          ;; everybody nested under it.
                          (org-entry-get (point) my/org-catchup-ignore-property t))
                (push (cons (concat emoji " " name
                                    (when (and group (= level 2))
                                      (propertize (concat "  " group)
                                                  'face 'helm-ff-directory)))
                            (concat emoji " " name))
                      candidates))))
          (sort (nreverse candidates)
                (lambda (a b) (string< (downcase (car a)) (downcase (car b))))))))))

(defun my/org-catchup--read-person (prompt)
  "Prompt with PROMPT for a person and return their exact heading text."
  (let ((candidates (my/org-catchup-people)))
    (cdr (assoc (completing-read prompt candidates nil t) candidates))))

(defun my/org-catchup-goto-person (&optional indirect)
  "Pick a person and jump to their heading in `my/org-catchup-file'.
With prefix argument INDIRECT, open their subtree in an indirect buffer."
  (interactive "P")
  (let ((heading (my/org-catchup--read-person "Catch-up with: ")))
    (find-file my/org-catchup-file)
    (widen)
    (let ((pos (org-find-exact-headline-in-buffer heading)))
      (unless pos (user-error "Heading not found: %s" heading))
      (push-mark)
      (goto-char pos)
      (org-fold-show-context 'org-goto)   ; unfold the enclosing group, if any
      (org-fold-show-children)            ; reveal the dated entries below
      (recenter 0)                        ; heading at the top of the window
      (when indirect
        (org-tree-to-indirect-buffer)))))

(define-key global-map (kbd "C-c o p") #'my/org-catchup-goto-person)


;;; Catch-up TODOs: the open items to raise at the next meeting.

(defvar my/org-catchup-todo-keywords '("TODO" "WORK" "WAIT")
  "TODO keywords considered open discussion items for catch-up popups.")

(defun my/org-catchup--todo-targets ()
  "Return an alist of (DISPLAY . (HEADING . FILE)) for the catch-up picker.
Every person in `my/org-catchup-file', plus the two catch-all headings of
`my/org-catchup-todo-file'."
  (append
   (mapcar (lambda (person)
             (cons (car person) (cons (cdr person) my/org-catchup-file)))
           (my/org-catchup-people))
   (list (cons "📥 Inbox" (cons "📥 Inbox" my/org-catchup-todo-file))
         (cons "☕️ Today" (cons "☕️ Today" my/org-catchup-todo-file)))))

(defun my/org-catchup--collect (headline file)
  "Return list of subtree strings for open items directly under HEADLINE in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (let ((pos (org-find-exact-headline-in-buffer headline)))
          (unless pos (user-error "Headline not found: %s" headline))
          (goto-char pos)
          (let ((child-level (1+ (org-current-level))))
            (org-narrow-to-subtree)
            (delq nil
                  (org-map-entries
                   (lambda ()
                     (when (and (= (org-current-level) child-level)
                                (member (org-get-todo-state)
                                        my/org-catchup-todo-keywords))
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (save-excursion (org-end-of-subtree t t)))))
                   t 'tree))))))))

(defun my/org-catchup--show (name headline file)
  "Display the open items under HEADLINE in FILE in a small, dismissable buffer.
NAME is used for the buffer name and header line."
  (let ((items (my/org-catchup--collect headline file)))
    (if (null items)
        (message "No open (TODO/WORK/WAIT) items for %s" name)
      (let ((buf (get-buffer-create (format "*Catch-up: %s*" name))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (mapconcat (lambda (s) (string-trim-right s)) items "\n")))
          (goto-char (point-min))
          (org-mode)                         ; fontification + svg-tag TODO pills
          (setq-local header-line-format
                      (format " Catch-up — %s   (q to dismiss)" name))
          (view-mode 1))                     ; read-only; q = quit-window
        (let ((win (display-buffer
                    buf '((display-buffer-below-selected display-buffer-at-bottom)))))
          (when win
            (fit-window-to-buffer win (floor (* 0.5 (frame-height))) 5)
            (ignore-errors (window-resize win 1))  ; one blank line of breathing room
            (select-window win)))))))        ; focus so q dismisses immediately

(defun my/org-catchup-todos ()
  "Pick a person and show their open items to raise at the next meeting."
  (interactive)
  (let* ((targets (my/org-catchup--todo-targets))
         (choice (completing-read "Open items for: " targets nil t))
         (target (cdr (assoc choice targets)))
         (heading (car target)))
    (my/org-catchup--show heading heading (cdr target))))

(define-key global-map (kbd "C-c o c") #'my/org-catchup-todos)


;;; New meeting note: the items that were discussed become the notes.
;;;
;;; A person's section holds the open items to raise at the next 1:1, then the
;;; dated meeting entries, newest first.  Once the meeting has happened the
;;; items that came up belong in that meeting's notes, as prose rather than as
;;; tasks.  `C-c o n' does the move: put the region over a run of open items and
;;; they leave the to-raise list for a new entry dated today, converted to a
;;; plain list, inserted above the most recent dated entry.

(defconst my/org-catchup--date-format "[%Y-%m-%d]"
  "Date prefix of a meeting entry, as accepted by `format-time-string'.")

(defun my/org-catchup--dated-child-re (level)
  "Return a regexp matching a dated meeting entry at LEVEL.
Only a real `[YYYY-MM-DD]' prefix counts: a `??' heading is a meeting whose
date was lost, not a date, so it never anchors the insertion."
  (format "^\\*\\{%d\\} +\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]" level))

(defun my/org-catchup--undecorate (heading)
  "Strip the TODO keyword, priority cookie and tags from HEADING text.
Used for sub-headings, which `org-heading-components' is not applied to
because they are read back out of the body as plain text."
  (let ((text heading))
    (when (and org-todo-keywords-1
               (string-match (concat "\\`" (regexp-opt org-todo-keywords-1) " +")
                             text))
      (setq text (substring text (match-end 0))))
    (when (string-match "\\`\\[#[A-Z]\\] +" text)
      (setq text (substring text (match-end 0))))
    (when (string-match " +:[[:alnum:]_@#%:]+:\\'" text)
      (setq text (substring text 0 (match-beginning 0))))
    (string-trim text)))

(defun my/org-catchup--as-list-item (title body level)
  "Return TITLE as a plain list item, with BODY indented underneath.
LEVEL is the outline level TITLE came from, which sets how deep a sub-heading
in BODY nests.  Body lines are shifted right by two columns: prose becomes the
continuation of the item, and a list keeps its relative depth."
  (let ((body (string-trim-right body)))
    (concat
     "- " title "\n"
     (unless (string= "" body)
       (concat
        (mapconcat
         (lambda (line)
           (cond
            ((string= "" line) "")
            ;; A sub-heading has no place in a list: make it a nested bullet,
            ;; one level of indentation per level below the item itself.
            ((string-match "\\`\\(\\*+\\) +\\(.*\\)\\'" line)
             (concat "  "
                     (make-string (* 2 (max 0 (- (length (match-string 1 line))
                                                 level 1)))
                                  ?\s)
                     "- " (my/org-catchup--undecorate (match-string 2 line))))
            (t (concat "  " line))))
         (split-string body "\n")
         "\n")
        "\n")))))

(defun my/org-catchup--item-at-point ()
  "Return (START END TITLE BODY) for the entry at point.
START and END delimit the whole subtree, so deleting that region moves the
item out.  TITLE has the keyword, priority cookie and tags stripped; BODY
starts after the planning line and any drawer."
  (let* ((start (point))
         (title (or (nth 4 (org-heading-components)) ""))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (body-start (min end (save-excursion (org-end-of-meta-data t) (point)))))
    (list start end title (buffer-substring-no-properties body-start end))))

(defun my/org-catchup--open-headings (beg end)
  "Return an alist of (POSITION . LEVEL) for open headings between BEG and END.
Open means the heading carries one of `my/org-catchup-todo-keywords'.  A
region starting inside an entry counts that entry, so it is enough to sweep
roughly over the items rather than land on the first heading exactly."
  (save-excursion
    (goto-char beg)
    (unless (org-at-heading-p)
      (ignore-errors (org-back-to-heading t)))
    (let (found)
      (when (and (org-at-heading-p)
                 (member (org-get-todo-state) my/org-catchup-todo-keywords))
        (push (cons (point) (org-current-level)) found))
      (while (and (outline-next-heading) (< (point) end))
        (when (member (org-get-todo-state) my/org-catchup-todo-keywords)
          (push (cons (point) (org-current-level)) found)))
      (nreverse found))))

(defun my/org-catchup--selected-items ()
  "Return (PARENT LEVEL ITEMS) for the open items the user selected.
The selection is the region, or the entry at point when there is none.  Only
the shallowest open headings are taken, so an open sub-item stays part of its
parent's body.  Signals if nothing is open, or if the selection reaches into
more than one person's section."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (org-back-to-heading t)
                     (cons (point) (line-end-position)))))
         (headings (my/org-catchup--open-headings (car bounds) (cdr bounds))))
    (unless headings
      (user-error "No %s item in the selection"
                  (mapconcat #'identity my/org-catchup-todo-keywords "/")))
    (let* ((level (apply #'min (mapcar #'cdr headings)))
           (tops (seq-filter (lambda (heading) (= (cdr heading) level)) headings))
           (parents (mapcar (lambda (heading)
                              (save-excursion
                                (goto-char (car heading))
                                (and (org-up-heading-safe) (point))))
                            tops)))
      (unless (car parents)
        (user-error "Open item is not under a person heading"))
      (unless (apply #'= (mapcar (lambda (parent) (or parent -1)) parents))
        (user-error "The selection spans more than one section"))
      (list (car parents) level
            (mapcar (lambda (heading)
                      (save-excursion
                        (goto-char (car heading))
                        (my/org-catchup--item-at-point)))
                    tops)))))

(defun my/org-catchup--meeting-note-point (parent level)
  "Return where a new meeting entry goes in the subtree of PARENT.
The start of the first dated child at LEVEL -- the entries run newest first,
so that is above the most recent meeting and below whatever open or undated
headings come before it.  The end of the subtree if there is no dated entry."
  (save-excursion
    (goto-char parent)
    (let ((limit (save-excursion (org-end-of-subtree t t) (point))))
      (if (re-search-forward (my/org-catchup--dated-child-re level) limit t)
          (line-beginning-position)
        limit))))

(defun my/org-catchup-new-meeting-note ()
  "Move the selected open items into a new meeting entry dated today.
The items are the ones the region touches -- or the one at point, with no
region -- among those carrying a `my/org-catchup-todo-keywords' keyword.  They
are converted to a plain list under a new heading dated today, inserted above
the most recent dated entry of the same section, and removed from where they
were.  Point is left after the date, for the title of the meeting."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (pcase-let* ((`(,parent ,level ,items) (my/org-catchup--selected-items))
               (date (format-time-string my/org-catchup--date-format))
               (note (mapconcat (pcase-lambda (`(,_ ,_ ,title ,body))
                                  (my/org-catchup--as-list-item title body level))
                                items "")))
    (atomic-change-group
      ;; Last first: deleting from the end keeps the earlier positions valid.
      (dolist (item (reverse items))
        (delete-region (nth 0 item) (nth 1 item)))
      (goto-char (my/org-catchup--meeting-note-point parent level))
      (unless (bolp) (insert "\n"))
      (let ((start (point)))
        (insert (make-string level ?*) " " date " \n" note)
        (goto-char start)
        (end-of-line)))
    (deactivate-mark)
    (message "Moved %d item%s into %s"
             (length items) (if (= 1 (length items)) "" "s") date)))

(define-key org-mode-map (kbd "C-c o n") #'my/org-catchup-new-meeting-note)


;;; Capture templates
;;;
;;; `org-capture' picks its target by mnemonic key, which does not scale to
;;; every person in the file, so the frequently-used ones are listed here by
;;; hand.  Only the KEY and the LABEL are hand-written: the heading itself is
;;; resolved from the file, so renaming a section in catchup.org cannot leave a
;;; template pointing at a heading that no longer exists (org-capture would
;;; silently create a duplicate section at the end of the file).  Everyone else
;;; is reachable through the `Anyone...' entry, which prompts with the same
;;; completion as C-c o p.

(defconst my/org-catchup-capture-keys
  '(("S" "Shabbir"  "Shabbir")
    ("A" "Anthony"  "AC ("           "AC")
    ("M" "Manish"   "Manish")
    ("a" "Abhishek" "Abhishek Gupta" "AG")
    ("s" "Sathya"   "Sathya (")
    ("t" "Tom"      "Tom Walsh")
    ("g" "Gino"     "Gino (")
    ("y" "Yogesh"   "Yogesh")
    ("m" "Amey"     "Amey")
    ("j" "Jas"      "Jas ("))
  "People to offer as `org-capture' targets, in menu order.
Each entry is (KEY LABEL MATCH [PLACEHOLDER]).  KEY is the `org-mks'
mnemonic; a two-character KEY puts the entry in a submenu under its first
character.  MATCH is a substring that must resolve to exactly one person in
`my/org-catchup-file' -- it is matched against the full heading, so use
enough of it to be unambiguous (\"Wiesemann\", not \"Mike\").  PLACEHOLDER is
the word pre-inserted in the captured item and selected for overtyping; it
defaults to LABEL.")

(defun my/org-catchup--resolve (match)
  "Return the one heading in `my/org-catchup-file' containing MATCH.
Return nil, after warning, if MATCH resolves to none or to several."
  (let ((hits (seq-filter (lambda (person) (string-search match (cdr person)))
                          (my/org-catchup-people))))
    (pcase (length hits)
      (1 (cdr (car hits)))
      (0 (display-warning 'org-catchup
                          (format "Capture target %S matches nobody" match))
         nil)
      (n (display-warning 'org-catchup
                          (format "Capture target %S is ambiguous (%d matches: %s)"
                                  match n (mapconcat #'cdr hits ", ")))
         nil))))

(defun my/org-catchup--capture-entry (key label heading placeholder)
  "Return an `org-capture' template for HEADING, keyed KEY and shown as LABEL.
PLACEHOLDER is pre-inserted in the item and selected for overtyping."
  (list key
        ;; Same emoji-then-tab layout as the Inbox and Today entries.
        (concat (car (split-string heading)) "\t" label)
        'entry
        (list 'file+headline my/org-catchup-file heading)
        (format "** TODO %%?%s\n  %%i\n" placeholder)
        :prepend t
        :empty-lines-after 0))

(defun my/org-catchup--goto-prompted-person ()
  "Prompt for a person and move point to their heading.
Used as the target function of the `Anyone...' capture template."
  (let* ((heading (my/org-catchup--read-person "Capture for: "))
         (pos (org-find-exact-headline-in-buffer heading)))
    (unless pos (user-error "Heading not found: %s" heading))
    (goto-char pos)))

(defun my/org-catchup-capture-templates ()
  "Return `org-capture' templates for the people in `my/org-catchup-file'.
The entries of `my/org-catchup-capture-keys' that resolve, each two-character
key preceded by its submenu heading, then a catch-all reaching anyone."
  (let (entries seen)
    (pcase-dolist (`(,key ,label ,match . ,rest) my/org-catchup-capture-keys)
      (when-let* ((heading (my/org-catchup--resolve match)))
        ;; A two-character key needs its submenu declared once, described by
        ;; the labels of every entry sharing that prefix.
        (when (= (length key) 2)
          (let ((prefix (substring key 0 1)))
            (unless (member prefix seen)
              (push prefix seen)
              (push (list prefix
                          (concat "\t"
                                  (mapconcat #'cadr
                                             (seq-filter
                                              (lambda (spec)
                                                (string-prefix-p prefix (car spec)))
                                              my/org-catchup-capture-keys)
                                             ", ")))
                    entries))))
        (push (my/org-catchup--capture-entry key label heading (or (car rest) label))
              entries)))
    (nreverse
     (cons (list "P" "🔎\tAnyone..." 'entry
                 (list 'file+function my/org-catchup-file
                       #'my/org-catchup--goto-prompted-person)
                 "** TODO %?\n  %i\n"
                 :prepend t
                 :empty-lines-after 0)
           entries))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
