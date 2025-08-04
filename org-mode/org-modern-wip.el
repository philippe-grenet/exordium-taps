(defcustom org-modern-horizontal-rule t
  "Prettify horizontal rulers.
The value can either be a boolean to enable/disable style or display
replacement expression, e.g., a string."
  :type '(choice boolean sexp))

(defcustom org-modern-block-name t
  "Prettify blocks names, i.e. #+begin_NAME and #+end_NAME lines.
If set to a pair of two strings, e.g. (\"‣\" . \"‣\"), the strings are
used as replacements for the #+begin_ and #+end_ prefixes, respectively.
If set to an alist of block names and cons cells of strings, the associated
strings will be used as a replacements for the whole of #+begin_NAME and
#+end_NAME, respectively, and the association with t treated as the value for
all other blocks."
  :type '(choice
          (const :tag "Hide #+begin_ and #+end_ prefixes" t)
          (cons (string :tag "#+begin_ replacement")
                (string :tag "#+end_ replacement"))
          (const :tag "Triangle bullets" ("‣" . "‣"))
          (alist :key-type
                 (choice
                  (string :tag "Block name")
                  (const :tag "Default" t))
                 :value-type
                 (choice
                  (list (string :tag "#+begin_NAME replacement")
                        (string :tag "#+end_NAME replacement"))
                  (const :tag "Hide #+begin_ and #+end_ prefixes" t)))))

(defcustom org-modern-block-fringe 2
  "Add a border to the blocks in the fringe.
This variable can also be set to an integer between 0 and 16,
which specifies the offset of the block border from the edge of
the window."
  :type '(choice boolean natnum))

(defface org-modern-horizontal-rule
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face used for horizontal ruler.")

(defun org-modern--block-name ()
  "Prettify block according to `org-modern-block-name'."
  (let* ((beg-ind (match-beginning 1))
         (beg-rep (match-beginning 2))
         (end-rep (match-end 3))
         (beg-name (match-beginning 3))
         (end-name (match-end 3))
         (names (and (listp org-modern-block-name) org-modern-block-name))
         (rep (cdr (assoc (downcase (match-string-no-properties 3)) names)))
         (fringe (and org-modern-block-fringe (not (bound-and-true-p org-indent-mode)))))
    (unless rep
      (setq rep (cdr (assq t names)) end-rep beg-name))
    (when (consp rep)
      (setq rep (if (= 8 (- beg-name beg-rep)) (car rep) (cadr rep))))
    (pcase rep
      ('t
       (add-face-text-property beg-name end-name 'org-modern-block-name)
       (put-text-property (if fringe beg-ind beg-rep) beg-name 'invisible 'org-modern))
      ((pred stringp)
       (add-face-text-property beg-name end-name 'org-modern-block-name)
       (put-text-property beg-rep end-rep 'display rep)
       (when fringe
         (put-text-property beg-ind beg-rep 'invisible 'org-modern))))))

(defun org-modern--block-fringe ()
  "Prettify blocks with fringe bitmaps."
  (save-excursion
    (goto-char (match-beginning 0))
    (add-text-properties
     (point) (min (line-end-position) (point-max))
     '(wrap-prefix
       #(" " 0 1 (display (left-fringe org-modern--block-begin org-block-begin-line)))
       line-prefix
       #(" " 0 1 (display (left-fringe org-modern--block-begin org-block-begin-line)))))
    (forward-line)
    (while
        (cond
         ((eobp) nil)
         ((save-excursion
            (let ((case-fold-search t))
              (re-search-forward
               "^[ \t]*#\\+end_" (line-end-position) 'noerror)))
          (add-text-properties
           (point) (min (line-end-position) (point-max))
           '(wrap-prefix
             #(" " 0 1 (display (left-fringe org-modern--block-end org-block-begin-line)))
             line-prefix
             #(" " 0 1 (display (left-fringe org-modern--block-end org-block-begin-line)))))
          nil)
         (t
          (add-text-properties
           (point) (min (1+ (line-end-position)) (point-max))
           '(wrap-prefix
             #(" " 0 1 (display (left-fringe org-modern--block-inner org-block-begin-line)))
             line-prefix
             #(" " 0 1 (display (left-fringe org-modern--block-inner org-block-begin-line)))))
          (forward-line)
          t)))))

(defun org-modern--pre-redisplay (_)
  "Compute font parameters before redisplay."
  (when-let ((box (and org-modern-label-border
                       (face-attribute 'org-modern-label :box nil t))))
    (unless (equal (and (listp box) (plist-get box :color))
                   (face-attribute 'default :background nil t))
      (org-modern--update-label-face)))
  (let ((face-remapping-alist
         `((default org-table
            ,@(or (ensure-list (cdr (assq 'default face-remapping-alist)))
                  '(default)))
           ,@face-remapping-alist)))
    (setq org-modern--table-sp-width (default-font-width)))
  (setf (cadr org-modern--table-overline) (face-attribute 'org-table :foreground nil t)))

(defun org-modern--update-label-face ()
  "Update border of the `org-modern-label' face."
  (set-face-attribute
   'org-modern-label nil
   :box
   (when org-modern-label-border
     (let ((border (if (eq org-modern-label-border 'auto)
                       (max 2 (cond
                               ((integerp line-spacing)
                                line-spacing)
                               ((floatp line-spacing)
                                (ceiling (* line-spacing (frame-char-height))))
                               (t (/ (frame-char-height) 10))))
                     org-modern-label-border)))
       (list :color (face-attribute 'default :background nil t)
             :line-width (cons -1 (- border)))))))

(defun org-modern--update-fringe-bitmaps ()
  "Update fringe bitmaps."
  (when (and org-modern-block-fringe
             (fboundp 'fringe-bitmap-p)
             (not (fringe-bitmap-p 'org-modern--block-inner)))
    (let* ((g (ceiling (frame-char-height) 1.8))
           (h (- (default-line-height) g))
           (v (expt 2 (- 15 (if (booleanp org-modern-block-fringe) 0
                              org-modern-block-fringe))))
           (w (+ v v -1)))
      (define-fringe-bitmap 'org-modern--block-inner
        (vector v) nil 16 '(top t))
      (define-fringe-bitmap 'org-modern--block-begin
        (vconcat (make-vector g 0) (vector w) (make-vector (- 127 g) v)) nil 16 'top)
      (define-fringe-bitmap 'org-modern--block-end
        (vconcat (make-vector (- 127 h) v) (vector w) (make-vector h 0)) nil 16 'bottom))))

(defun org-modern--symbol (str)
  "Add `org-modern-symbol' face to STR."
  (setq str (if (stringp str) (copy-sequence str) (char-to-string str)))
  (add-face-text-property 0 (length str) 'org-modern-symbol 'append str)
  str)

(defun org-modern--make-font-lock-keywords ()
  "Compute font-lock keywords."
  (append
   (when org-modern-horizontal-rule
     `(("^[ \t]*-\\{5,\\}$" 0
        '(face org-modern-horizontal-rule display
               ,(if (eq org-modern-horizontal-rule t)
                    '(space :width text)
                  org-modern-horizontal-rule)))))
   ;; Do not add source block fringe markers if org-indent-mode is
   ;; enabled. org-indent-mode uses line prefixes for indentation.
   ;; Therefore we cannot have both.
   (when (and org-modern-block-fringe (not (bound-and-true-p org-indent-mode)))
     '(("^[ \t]*#\\+\\(?:begin\\|BEGIN\\)_\\S-"
        (0 (org-modern--block-fringe)))))
   (when org-modern-block-name
     (let* ((indent (and org-modern-block-fringe
                         (not (bound-and-true-p org-indent-mode))
                         '((1 '(face nil invisible org-modern)))))
            (name '(3 'org-modern-block-name append))
            (hide `(,@indent (2 '(face nil invisible org-modern)) ,name))
            (specs
             (pcase org-modern-block-name
               ('t ;; Hide
                (cons hide hide))
               (`((,_k . ,_v) . ,_rest) ;; Dynamic replacement
                '(((0 (org-modern--block-name))) . ((0 (org-modern--block-name)))))
               (`(,beg . ,end) ;; Static replacement
                `((,@indent (2 '(face nil display ,beg)) ,name) .
                  (,@indent (2 '(face nil display ,end)) ,name))))))
       `(("^\\([ \t]*\\)\\(#\\+\\(?:begin\\|BEGIN\\)_\\)\\(\\S-+\\).*"
          ,@(car specs))
         ("^\\([ \t]*\\)\\(#\\+\\(?:end\\|END\\)_\\)\\(\\S-+\\).*"
          ,@(cdr specs)))))))

;;;###autoload
(define-minor-mode org-modern-mode
  "Modern looks for Org."
  :global nil
  :group 'org-modern
  (unless (derived-mode-p 'org-mode)
    (error "`org-modern-mode' should be enabled only in `org-mode'"))
  (cond
   (org-modern-mode
    (add-to-invisibility-spec 'org-modern)
    (setq
     org-modern--font-lock-keywords
     (append (remove '(org-fontify-meta-lines-and-blocks) org-font-lock-keywords)
             (org-modern--make-font-lock-keywords)))
    (font-lock-remove-keywords nil org-font-lock-keywords)
    (font-lock-add-keywords nil org-modern--font-lock-keywords)
    (setq-local font-lock-unfontify-region-function #'org-modern--unfontify)
    (add-hook 'pre-redisplay-functions #'org-modern--pre-redisplay nil 'local)
    (add-hook 'org-after-promote-entry-hook #'org-modern--unfontify-line nil 'local)
    (add-hook 'org-after-demote-entry-hook #'org-modern--unfontify-line nil 'local)
    ;; (when (eq org-modern-star 'fold)
    ;;   (add-hook 'org-cycle-hook #'org-modern--cycle nil 'local))
    (org-modern--update-label-face)
    (org-modern--update-fringe-bitmaps))
   (t
    (remove-from-invisibility-spec 'org-modern)
    (font-lock-remove-keywords nil org-modern--font-lock-keywords)
    (font-lock-add-keywords nil org-font-lock-keywords)
    (setq-local font-lock-unfontify-region-function #'org-unfontify-region)
    (remove-hook 'pre-redisplay-functions #'org-modern--pre-redisplay 'local)
    (remove-hook 'org-after-promote-entry-hook #'org-modern--unfontify-line 'local)
    (remove-hook 'org-after-demote-entry-hook #'org-modern--unfontify-line 'local)
    (when (eq org-modern-star 'fold)
      (remove-hook 'org-cycle-hook #'org-modern--cycle 'local))))
  (without-restriction
    (with-silent-modifications
      (org-modern--unfontify (point-min) (point-max)))
    (font-lock-flush)))

(defun org-modern--unfontify-line ()
  "Unfontify prettified elements on current line."
  (org-modern--unfontify (pos-bol) (pos-eol)))

(defun org-modern--unfontify (beg end &optional _loud)
  "Unfontify prettified elements between BEG and END."
  (let ((font-lock-extra-managed-props
         (append
          ;; Only remove line/wrap-prefix if block fringes are used
          (if (and org-modern-block-fringe (not (bound-and-true-p org-indent-mode)))
              '(wrap-prefix line-prefix display invisible)
            '(display invisible))
          font-lock-extra-managed-props)))
    (org-unfontify-region beg end)))

;;;###autoload
(define-globalized-minor-mode global-org-modern-mode
  org-modern-mode org-modern--on
  :group 'org-modern
  (if global-org-modern-mode
      (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
    (remove-hook 'org-agenda-finalize-hook #'org-modern-agenda)))

(defun org-modern--on ()
  "Enable `org-modern' in every Org buffer."
  (when (derived-mode-p #'org-mode)
    (org-modern-mode)))

(provide 'org-modern)
;;; org-modern.el ends here

(with-eval-after-load 'org (global-org-modern-mode))
