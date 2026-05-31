;;; bql-mode.el --- Major mode for Bloomberg Query Language (BQL) -*- lexical-binding: t; -*-

;;; Commentary:
;; A small major mode for BQL with SQL-like highlighting and simple indentation.
;; Save as bql-mode.el and (require 'bql-mode).

;;; Code:

(require 'rx)

(defgroup bql nil
  "Major mode for Bloomberg Query Language."
  :group 'languages)

;; ---- Syntax table -----------------------------------------------------------

(defvar bql-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat underscore as word constituent
    (modify-syntax-entry ?_ "w" st)
    ;; Strings: "..." and '...'
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    ;; Comments: -- to end of line, and /* ... */
    (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; Comparison operators: ensure < and > are plain punctuation
    ;; (prevents org-mode's paired-delimiter treatment from leaking in)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    ;; Parentheses/brackets
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    st)
  "Syntax table for `bql-mode'.")

;; ---- Font-lock (syntax highlighting) ---------------------------------------

(defconst bql--keywords
  '("get" "for" "with" "on" "preferences" "let"
    "and" "xor" "or" "in"))

(defconst bql--builtin-funcs
  ;; A small starter set—extend with common BQL functions you use.
  '(
    ;; Arithmetic
    "abs" "ceil" "exp" "floor" "ln" "log" "round" "sign" "sqrt" "square" "mod" "negation"
    "pow" "normal_dist" "normal_inv"
    ;; Statistical
    "sum" "count" "avg" "wavg" "min" "max" "median" "product" "corr" "rsq" "std" "var"
    "skew" "kurt" "zscore" "winsorize" "compoundGrowthRate" "cut" "rank" "quantile"
    ;; Grouping data
    "group" "ungroup" "groupAvg" "groupCount" "groupMax" "groupMedian" "groupMin"
    "groupRank" "groupStd" "groupSum" "groupWAvg" "groupZscore" "groupcut"
    "groupwinsorize"
    ;; Time series manipulation
    "cumAvg" "cumMax" "cumMin" "cumProd" "cumSum" "diff" "net_chg" "pct_chg" "pct_diff"
    "rolling"
    ;; Date manipulation
    "today" "year" "month" "dayofweek" "dayofmonth" "minute" "hour" "range"
    ;; String manipulation
    "concat" "left" "len" "replace" "right" "startsWith" "toLower" "toUpper"
    ;; Filtering and conditionals
    "filter" "if" "and" "equals" "greaterThan" "greaterThanOrEquals" "in" "lessThan"
    "lessThanOrEquals" "not" "notEquals" "or" "xor" "all" "any" "between" "matches"
    ;; Data handling
    "avail" "dropNA" "znav" "replaceNonNumeric" "first" "last" "sort" "groupSort" "value"
    ;; Universe functions
    ;; -- ID list operations
    "filter" "union" "intersect" "setDiff" "list"
    ;; -- Security universes
    "equityUniv" "fundsUniv" "bondsUniv" "debtUniv" "municipalsUniv" "loansUniv"
    "mortgagesUniv" "preferredUniv" "preferreds"
    ;; -- Security chains and membership
    "members" "peers" "segments" "bonds" "loans" "municipals" "mortgages" "preferred"
    "debt" "options" "futures" "curveMembers"
    ;; -- Related entities
    "fundamentalTicker" "relativeIndex" "translateSymbols" "issuerOf" "parent"
    "equityPricingTicker" "ESGTicker" "cds"
    ;; -- User data and applications
    "screenresults" "axeduniv" "notesUniv" "notes"))

(defconst bql--constants
  '("true" "false" "null" "na"))

(defconst bql-font-lock-keywords
  (let* ((case-fold-search t)
         (kw (rx-to-string `(seq symbol-start
                                 (or ,@bql--keywords)
                                 symbol-end)
                           'no-group))
         (fn (rx-to-string `(seq symbol-start
                                 (or ,@bql--builtin-funcs)
                                 symbol-end)
                           'no-group))
         (cn (concat "\\<"
                     (rx-to-string `(or ,@bql--constants) 'no-group)
                     "\\>")))
    `(
      ;; Keywords
      (,kw . font-lock-keyword-face)
      ;; Builtins (before opening parenthesis)
      (,(concat "\\(\\<" fn "\\>\\)\\s-*(") 1 font-lock-builtin-face)
      ;; Constants
      (,cn . font-lock-constant-face)
      ;; Table/column identifiers
      ("\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\.[A-Za-z_][A-Za-z0-9_]*\\b"
       1 font-lock-type-face)
      ;; Numbers (use font-lock-constant-face to avoid issues with
      ;; font-lock-number-face evaluation in org source block fontification)
      ("\\b-?[0-9]+\\(?:\\.[0-9]+\\)?[dDmMqQyY]?\\b" . font-lock-constant-face)
      ;; Strings in single or double quotes
      ("'[^']*'" . font-lock-string-face)
      ("\"[^\"]*\"" . font-lock-string-face)
      ;; Macros beginning with #, followed by a symbol
      ("\\B#\\([A-Za-z_][A-Za-z0-9_]*\\)\\b" . font-lock-preprocessor-face)
      )))



;; ---- Indentation ------------------------------------------------------------

(defcustom bql-indent-offset 2
  "Indentation width for `bql-mode'."
  :type 'integer :group 'bql)

(defun bql--line-starts-with-closing-p ()
  "Return non-nil if current line begins with a closing delimiter."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "[])}]")))

(defun bql--compute-indentation ()
  "Compute indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (let ((ppss (syntax-ppss)))
      (cond
       ;; Inside a string or comment: keep column 0 unless after opening
       ((or (nth 3 ppss) (nth 4 ppss))
        (current-indentation))
       ;; Line starts with a closing paren/bracket/brace: dedent
       ((bql--line-starts-with-closing-p)
        (max 0 (- (bql--indentation-from-parens) bql-indent-offset)))
       (t
        (bql--indentation-from-parens))))))

(defun bql--indentation-from-parens ()
  "Base indentation derived from unmatched opening delimiters."
  (save-excursion
    (let ((depth 0) (col 0))
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
                  (setq depth (max 0 (1- depth))))))
               ))
            (forward-char 1))))
      (setq col (* depth bql-indent-offset))
      col)))

(defun bql-indent-line ()
  "Indent current line as BQL."
  (interactive)
  (let* ((col (current-column))
         (indent (bql--compute-indentation)))
    (indent-line-to indent)
    (when (> (current-column) (current-indentation))
      (move-to-column (max indent col)))))

;; ---- Imenu ------------------------------------------------------------------

(defvar bql-imenu-generic-expression
  '(("Let bindings" "^\\s-*\\(let\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 2)
    ("Get blocks"   "^\\s-*\\(get\\)\\b" 1))
  "Imenu patterns for `bql-mode'.")

;; ---- Electric pairs ---------------------------------------------------------

(defvar bql-electric-pairs
  '((?\" . ?\") (?\'. ?\') (?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}))
  "Electric pairs for `bql-mode'.")

;; ---- Mode definition --------------------------------------------------------

;;;###autoload
(define-derived-mode bql-mode prog-mode "BQL"
  "Major mode for editing Bloomberg Query Language (BQL)."
  :syntax-table bql-mode-syntax-table
  (setq-local case-fold-search t)
  (setq-local font-lock-defaults
              '(bql-font-lock-keywords t t ((?< . ".") (?> . "."))))
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?:--+\\|/\\*+\\)\\s-*")
  (setq-local indent-line-function #'bql-indent-line)
  (setq-local electric-pair-pairs bql-electric-pairs)
  (setq-local electric-pair-text-pairs bql-electric-pairs)
  (setq-local imenu-generic-expression bql-imenu-generic-expression)
  (electric-pair-local-mode 1))

;; ---- Easy menu --------------------------------------------------------------

(easy-menu-define bql-mode-menu bql-mode-map
  "Menu for BQL mode."
  '("BQL"
    ["Indent line" bql-indent-line t]
    ["Comment region" comment-region t]
    ["Uncomment region" uncomment-region t]
    "---"
    ["Imenu" imenu t]))

;; ---- File association -------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bql\\'" . bql-mode))

(provide 'bql-mode)

;;; bql-mode.el ends here
