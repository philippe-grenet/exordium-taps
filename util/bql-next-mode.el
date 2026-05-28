;;; bql-next-mode.el --- Major mode for BQL.Next queries -*- lexical-binding: t; -*-

;;; Commentary:
;; A major mode for BQL.Next (v2.0) with syntax highlighting and simple indentation.
;; BQL.Next uses pipeline operators, graph traversal, and action-oriented syntax.
;; Save as bql-next-mode.el and (require 'bql-next-mode).

;;; Code:

(require 'rx)

(defgroup bql-next nil
  "Major mode for BQL.Next."
  :group 'languages)

;; ---- Syntax table -----------------------------------------------------------

(defvar bql-next-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    ;; Strings: "..." and '...'
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    ;; Comments: // to end of line
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Punctuation
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?@ "." st)
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?| "." st)
    ;; Parentheses/brackets
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    st)
  "Syntax table for `bql-next-mode'.")

;; ---- Font-lock (syntax highlighting) ---------------------------------------

(defconst bql-next--keywords
  '("load" "retrieve" "get" "show" "fetch"
    "filter" "top" "sort" "skip" "take" "order" "by"
    "let" "set" "as"
    "group" "aggregate" "per"
    "union" "intersect" "except"
    "has" "having" "is"
    "for" "with" "on"))

(defconst bql-next--builtin-funcs
  '("avg" "sum" "count" "count_all" "wavg" "medianif"
    "sumif" "countif" "groupavg" "groupsort" "groupcount"
    "correlation" "zscore" "pct_diff"
    "concat" "textjoin" "left"
    "round" "floor" "mod" "abs" "sqrt" "format"
    "if" "avail" "any" "all" "in" "dropna" "matches" "replaceNA"
    "year" "month" "range" "today"
    "value" "to_ids"
    "pct_chg" "std" "members" "equity"))

(defconst bql-next--constants
  '("true" "false" "null" "na" "NA" "ALL"))

(defconst bql-next--logical-operators
  '("and" "or" "not" "AND" "OR" "NOT"))

(defconst bql-next-font-lock-keywords
  (let* ((kw (rx-to-string `(seq symbol-start
                                 (or ,@bql-next--keywords)
                                 symbol-end)
                           'no-group))
         (fn (rx-to-string `(seq symbol-start
                                 (or ,@bql-next--builtin-funcs)
                                 symbol-end)
                           'no-group))
         (cn (rx-to-string `(seq symbol-start
                                 (or ,@bql-next--constants)
                                 symbol-end)
                           'no-group))
         (lo (rx-to-string `(seq symbol-start
                                 (or ,@bql-next--logical-operators)
                                 symbol-end)
                           'no-group)))
    `(
      ;; ## single-line comments (syntax table handles //)
      ("^\\s-*##.*$" . font-lock-comment-face)
      ;; Pipeline operator |>
      ("|>" . font-lock-keyword-face)
      ;; Graph traversal operators => -> <- <=
      ("\\(=>\\|->\\|<-\\|<=\\)" . font-lock-keyword-face)
      ;; Keywords
      (,kw . font-lock-keyword-face)
      ;; Logical operators as keywords
      (,lo . font-lock-keyword-face)
      ;; Builtins (followed by opening paren)
      (,(concat "\\(\\<" fn "\\>\\)\\s-*(") 1 font-lock-builtin-face)
      ;; Constants
      (,cn . font-lock-constant-face)
      ;; Entity references: @'...'
      ("@'[^']*'" . font-lock-string-face)
      ;; Variable prefix: #identifier
      ("\\B#\\([A-Za-z_][A-Za-z0-9_]*\\)\\b" . font-lock-preprocessor-face)
      ;; Entity alias: identifier:Type (e.g. e:Equity, p:parent)
      ("\\b\\([A-Za-z_][A-Za-z0-9_]*\\):" 1 font-lock-variable-name-face)
      ;; Dot-access: identifier.field
      ("\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\." 1 font-lock-type-face)
      ;; Numbers with optional suffix (e.g. 10b, 1y, 3d)
      ("\\b-?[0-9]+\\(?:\\.[0-9]+\\)?[bBmMdDqQyY]?\\b" . font-lock-constant-face)
      ))
  "Font-lock keywords for `bql-next-mode'.")

;; ---- Indentation ------------------------------------------------------------

(defcustom bql-next-indent-offset 2
  "Indentation width for `bql-next-mode'."
  :type 'integer :group 'bql-next)

(defun bql-next--line-starts-with-closing-p ()
  "Return non-nil if current line begins with a closing delimiter."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "[])}]")))

(defun bql-next--continuation-line-p ()
  "Return non-nil if current line is a continuation (starts with |>)."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "|>")))

(defun bql-next--compute-indentation ()
  "Compute indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (let ((ppss (syntax-ppss)))
      (cond
       ((or (nth 3 ppss) (nth 4 ppss))
        (current-indentation))
       ((bql-next--line-starts-with-closing-p)
        (max 0 (- (bql-next--indentation-from-parens) bql-next-indent-offset)))
       ((bql-next--continuation-line-p)
        bql-next-indent-offset)
       (t
        (bql-next--indentation-from-parens))))))

(defun bql-next--indentation-from-parens ()
  "Base indentation derived from unmatched opening delimiters."
  (save-excursion
    (let ((depth 0))
      (save-restriction
        (widen)
        (let ((pos (point)))
          (goto-char (point-min))
          (while (< (point) pos)
            (let ((ppss (syntax-ppss)))
              (cond
               ((nth 3 ppss) (goto-char (or (nth 8 ppss) (point))) (forward-sexp 1))
               ((nth 4 ppss) (forward-comment (point-max)))
               (t
                (cond
                 ((looking-at-p "[([{]")
                  (setq depth (1+ depth)))
                 ((looking-at-p "[])}]")
                  (setq depth (max 0 (1- depth))))))))
            (forward-char 1))))
      (* depth bql-next-indent-offset))))

(defun bql-next-indent-line ()
  "Indent current line as BQL.Next."
  (interactive)
  (let* ((col (current-column))
         (indent (bql-next--compute-indentation)))
    (indent-line-to indent)
    (when (> (current-column) (current-indentation))
      (move-to-column (max indent col)))))

;; ---- Imenu ------------------------------------------------------------------

(defvar bql-next-imenu-generic-expression
  '(("Let bindings" "^\\s-*\\(?:let\\|set\\)\\s-+\\([A-Za-z_#][A-Za-z0-9_]*\\)" 1)
    ("Load/Retrieve" "^\\s-*\\(load\\|retrieve\\|get\\|fetch\\)\\b" 1))
  "Imenu patterns for `bql-next-mode'.")

;; ---- Electric pairs ---------------------------------------------------------

(defvar bql-next-electric-pairs
  '((?\" . ?\") (?\' . ?\') (?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}))
  "Electric pairs for `bql-next-mode'.")

;; ---- Mode definition --------------------------------------------------------

;;;###autoload
(define-derived-mode bql-next-mode prog-mode "BQL.Next"
  "Major mode for editing BQL.Next queries."
  :syntax-table bql-next-mode-syntax-table
  (setq-local case-fold-search t)
  (setq-local font-lock-defaults
              '(bql-next-font-lock-keywords nil nil ((?_ . "w"))))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|##+\\)\\s-*")
  (setq-local indent-line-function #'bql-next-indent-line)
  (setq-local electric-pair-pairs bql-next-electric-pairs)
  (setq-local electric-pair-text-pairs bql-next-electric-pairs)
  (setq-local imenu-generic-expression bql-next-imenu-generic-expression)
  (electric-pair-local-mode 1))

;; ---- Easy menu --------------------------------------------------------------

(easy-menu-define bql-next-mode-menu bql-next-mode-map
  "Menu for BQL.Next mode."
  '("BQL.Next"
    ["Indent line" bql-next-indent-line t]
    ["Comment region" comment-region t]
    ["Uncomment region" uncomment-region t]
    "---"
    ["Imenu" imenu t]))

;; ---- File association -------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bqln\\'" . bql-next-mode))

(provide 'bql-next-mode)

;;; bql-next-mode.el ends here
