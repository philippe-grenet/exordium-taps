;;; markdown-to-org--string.t.el --- Unit tests for markdown-to-org--string -*- lexical-binding: t -*-

;;; Commentary:
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert

(require 'cl-lib)

;; Load only the function under test from after-init.el, avoiding the
;; top-level side effects (require 'markdown-mode, hooks, keybindings).
(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (src (with-temp-buffer
              (insert-file-contents (expand-file-name "after-init.el" dir))
              (buffer-string))))
  ;; Extract and evaluate the markdown-to-org--string defun
  (with-temp-buffer
    (insert src)
    (goto-char (point-min))
    (when (re-search-forward "^(defun markdown-to-org--string " nil t)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer))))))

;;; Code:

;; --- Heading conversion ---

(ert-deftest test-md-to-org-heading-1 ()
  "Level 1 heading becomes #+title:."
  (should (string= (markdown-to-org--string "# Document title")
                   "#+title: Document title")))

(ert-deftest test-md-to-org-heading-2 ()
  "Level 2 heading becomes single star."
  (should (string= (markdown-to-org--string "## Section")
                   "* Section")))

(ert-deftest test-md-to-org-heading-3 ()
  "Level 3 heading becomes two stars."
  (should (string= (markdown-to-org--string "### Subsection")
                   "** Subsection")))

(ert-deftest test-md-to-org-heading-4 ()
  "Level 4 heading becomes three stars."
  (should (string= (markdown-to-org--string "#### Deep")
                   "*** Deep")))

(ert-deftest test-md-to-org-heading-5 ()
  "Level 5 heading."
  (should (string= (markdown-to-org--string "##### Deeper")
                   "**** Deeper")))

(ert-deftest test-md-to-org-heading-6 ()
  "Level 6 heading."
  (should (string= (markdown-to-org--string "###### Deepest")
                   "***** Deepest")))

(ert-deftest test-md-to-org-heading-with-leading-space ()
  "Heading with leading whitespace."
  (should (string= (markdown-to-org--string "  ## Indented heading")
                   "* Indented heading")))

(ert-deftest test-md-to-org-heading-multiline ()
  "Multiple headings in a document."
  (should (string= (markdown-to-org--string
                    "# Title\n\n## Header\n\n### Sub-header")
                   "#+title: Title\n\n* Header\n\n** Sub-header")))

;; --- Plain text passthrough ---

(ert-deftest test-md-to-org-plain-text ()
  "Plain text passes through unchanged."
  (should (string= (markdown-to-org--string "Hello world")
                   "Hello world")))

(ert-deftest test-md-to-org-not-a-heading ()
  "Text starting with # but not a heading pattern should not be treated as heading."
  (should (string= (markdown-to-org--string "#no-space-after-hash")
                   "#no-space-after-hash")))

;; --- Bold/emphasis conversion ---

(ert-deftest test-md-to-org-bold-stars ()
  "**bold** becomes *bold*."
  (should (string= (markdown-to-org--string "This is **Bold**.")
                   "This is *Bold*.")))

(ert-deftest test-md-to-org-bold-underscores ()
  "__bold__ becomes *bold*."
  (should (string= (markdown-to-org--string "This is __Bold__.")
                   "This is *Bold*.")))

(ert-deftest test-md-to-org-italic-stars ()
  "*italic* becomes /italic/."
  (should (string= (markdown-to-org--string "This is *italic*.")
                   "This is /italic/.")))

(ert-deftest test-md-to-org-italic-underscores ()
  "_italic_ becomes /italic/."
  (should (string= (markdown-to-org--string "This is _italic_.")
                   "This is /italic/.")))

;; --- Inline code ---

(ert-deftest test-md-to-org-inline-code ()
  "`code` becomes ~code~."
  (should (string= (markdown-to-org--string "Use `foo` here.")
                   "Use `foo` here.")))

;; --- Strikethrough ---

(ert-deftest test-md-to-org-strikethrough ()
  "~~del~~ becomes +del+."
  (should (string= (markdown-to-org--string "This is ~~deleted~~.")
                   "This is +deleted+.")))

;; --- Links ---

(ert-deftest test-md-to-org-link ()
  "[text](url) becomes [[url][text]]."
  (should (string= (markdown-to-org--string "[Click here](http://example.com)")
                   "[[http://example.com][Click here]]")))

(ert-deftest test-md-to-org-image-link ()
  "![alt](url) becomes [[url]]."
  (should (string= (markdown-to-org--string "![alt text](http://img.png)")
                   "[[http://img.png]]")))

;; --- Fenced code blocks ---

(ert-deftest test-md-to-org-fenced-code-block ()
  "Fenced code block with language."
  (should (string= (markdown-to-org--string
                    "```python\nprint('hello')\n```")
                   "#+begin_src python\nprint('hello')\n#+end_src")))

(ert-deftest test-md-to-org-fenced-code-block-no-lang ()
  "Fenced code block without language."
  (should (string= (markdown-to-org--string
                    "```\nsome code\n```")
                   "#+begin_src \nsome code\n#+end_src")))

(ert-deftest test-md-to-org-fenced-code-preserves-content ()
  "Content inside fenced blocks is not converted."
  (should (string= (markdown-to-org--string
                    "```\n**not bold**\n```")
                   "#+begin_src \n**not bold**\n#+end_src")))

;; --- Blockquotes ---

(ert-deftest test-md-to-org-blockquote ()
  "> text becomes : text."
  (should (string= (markdown-to-org--string "> This is quoted")
                   ": This is quoted")))

;; --- Lists ---

(ert-deftest test-md-to-org-unordered-list ()
  "Unordered list items."
  (should (string= (markdown-to-org--string "- Item one\n- Item two")
                   "- Item one\n- Item two")))

(ert-deftest test-md-to-org-ordered-list ()
  "Ordered list items."
  (should (string= (markdown-to-org--string "1. First\n2. Second")
                   "1. First\n1. Second")))

;; --- Horizontal rules ---

(ert-deftest test-md-to-org-horizontal-rule-dashes ()
  "--- becomes -----."
  (should (string= (markdown-to-org--string "---")
                   "-----")))

(ert-deftest test-md-to-org-horizontal-rule-stars ()
  "*** becomes -----."
  (should (string= (markdown-to-org--string "***")
                   "-----")))

;; --- Full document from the plan ---

(ert-deftest test-md-to-org-full-document ()
  "Full document conversion from the plan example."
  (let ((input "# Document title\n\n## This is a header\n\n### This is a sub-header\n\nThis is a text with **Bold**.")
        (expected "#+title: Document title\n\n* This is a header\n\n** This is a sub-header\n\nThis is a text with *Bold*."))
    (should (string= (markdown-to-org--string input) expected))))

;; --- Empty / edge cases ---

(ert-deftest test-md-to-org-empty-string ()
  "Empty string."
  (should (string= (markdown-to-org--string "") "")))

(ert-deftest test-md-to-org-blank-lines ()
  "Blank lines are preserved."
  (should (string= (markdown-to-org--string "a\n\nb") "a\n\nb")))

(ert-deftest test-md-to-org-unclosed-fence ()
  "Unclosed fence block gets auto-closed."
  (should (string= (markdown-to-org--string "```\ncode")
                   "#+begin_src \ncode\n#+end_src")))

(provide 'markdown-to-org--string.t)

;;; markdown-to-org--string.t.el ends here
