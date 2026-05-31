;;; org-to-markdown.t.el --- Unit tests for org-to-markdown--string -*- lexical-binding: t -*-

;;; Commentary:
;;
;; To run all tests:
;;     M-x eval-buffer
;;     M-x ert

(require 'cl-lib)

;; Load only the function under test from org-to-markdown.el.
(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (src (with-temp-buffer
              (insert-file-contents (expand-file-name "org-to-markdown.el" dir))
              (buffer-string))))
  ;; Extract and evaluate the org-to-markdown--string defun
  (with-temp-buffer
    (insert src)
    (goto-char (point-min))
    (when (re-search-forward "^(defun org-to-markdown--string " nil t)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer))))))

;;; Code:

;; --- Title conversion ---

(ert-deftest test-org-to-md-title ()
  "#+title: becomes # heading."
  (should (string= (org-to-markdown--string "#+title: Document title")
                   "# Document title")))

(ert-deftest test-org-to-md-title-extra-spaces ()
  "#+title: with extra spaces."
  (should (string= (org-to-markdown--string "#+title:   Spaced title  ")
                   "# Spaced title")))

;; --- Startup lines (dropped) ---

(ert-deftest test-org-to-md-startup-removed ()
  "#+startup: lines are removed."
  (should (string= (org-to-markdown--string "#+startup: overview\n#+title: Doc")
                   "# Doc")))

(ert-deftest test-org-to-md-startup-multiple ()
  "Multiple #+startup: lines are all removed."
  (should (string= (org-to-markdown--string "#+startup: overview\n#+startup: indent\n* Heading")
                   "## Heading")))

;; --- Babel mermaid results (dropped) ---

(ert-deftest test-org-to-md-babel-results-removed ()
  "#+RESULTS: and its image line are removed."
  (should (string= (org-to-markdown--string
                    "some text\n#+RESULTS:\n[[file:diagrams/foo.png]]\nmore text")
                   "some text\nmore text")))

(ert-deftest test-org-to-md-babel-results-only ()
  "#+RESULTS: block alone produces empty output."
  (should (string= (org-to-markdown--string
                    "#+RESULTS:\n[[file:diagrams/some-file.png]]")
                   "")))

(ert-deftest test-org-to-md-babel-results-no-image ()
  "#+RESULTS: followed by a non-image line keeps the next line."
  (should (string= (org-to-markdown--string
                    "#+RESULTS:\nplain text")
                   "plain text")))

;; --- Heading conversion ---

(ert-deftest test-org-to-md-heading-1 ()
  "Single star becomes ##."
  (should (string= (org-to-markdown--string "* Section")
                   "## Section")))

(ert-deftest test-org-to-md-heading-2 ()
  "Two stars becomes ###."
  (should (string= (org-to-markdown--string "** Subsection")
                   "### Subsection")))

(ert-deftest test-org-to-md-heading-3 ()
  "Three stars becomes ####."
  (should (string= (org-to-markdown--string "*** Deep")
                   "#### Deep")))

(ert-deftest test-org-to-md-heading-4 ()
  "Four stars becomes #####."
  (should (string= (org-to-markdown--string "**** Deeper")
                   "##### Deeper")))

(ert-deftest test-org-to-md-heading-5 ()
  "Five stars becomes ######."
  (should (string= (org-to-markdown--string "***** Deepest")
                   "###### Deepest")))

(ert-deftest test-org-to-md-heading-multiline ()
  "Multiple headings in a document."
  (should (string= (org-to-markdown--string
                    "#+title: Title\n\n* Header\n\n** Sub-header")
                   "# Title\n\n## Header\n\n### Sub-header")))

;; --- Plain text passthrough ---

(ert-deftest test-org-to-md-plain-text ()
  "Plain text passes through unchanged."
  (should (string= (org-to-markdown--string "Hello world")
                   "Hello world")))

(ert-deftest test-org-to-md-not-a-heading ()
  "Lone star without space is not a heading."
  (should (string= (org-to-markdown--string "*not-a-heading")
                   "*not-a-heading")))

;; --- Bold/emphasis conversion ---

(ert-deftest test-org-to-md-bold ()
  "*bold* becomes **bold**."
  (should (string= (org-to-markdown--string "This is *Bold*.")
                   "This is **Bold**.")))

(ert-deftest test-org-to-md-italic ()
  "/italic/ becomes *italic*."
  (should (string= (org-to-markdown--string "This is /italic/.")
                   "This is *italic*.")))

;; --- Inline code ---

(ert-deftest test-org-to-md-inline-code ()
  "`code` stays as `code`."
  (should (string= (org-to-markdown--string "Use `foo` here.")
                   "Use `foo` here.")))

;; --- Strikethrough ---

(ert-deftest test-org-to-md-strikethrough ()
  "+del+ becomes ~~del~~."
  (should (string= (org-to-markdown--string "This is +deleted+.")
                   "This is ~~deleted~~.")))

;; --- Links ---

(ert-deftest test-org-to-md-link ()
  "[[url][text]] becomes [text](url)."
  (should (string= (org-to-markdown--string "[[http://example.com][Click here]]")
                   "[Click here](http://example.com)")))

(ert-deftest test-org-to-md-image-link ()
  "[[url]] becomes ![](url)."
  (should (string= (org-to-markdown--string "[[http://img.png]]")
                   "![](http://img.png)")))

;; --- Source code blocks ---

(ert-deftest test-org-to-md-src-block ()
  "Source block with language."
  (should (string= (org-to-markdown--string
                    "#+begin_src python\nprint('hello')\n#+end_src")
                   "```python\nprint('hello')\n```")))

(ert-deftest test-org-to-md-src-block-no-lang ()
  "Source block without language."
  (should (string= (org-to-markdown--string
                    "#+begin_src\nsome code\n#+end_src")
                   "```\nsome code\n```")))

(ert-deftest test-org-to-md-src-preserves-content ()
  "Content inside source blocks is not converted."
  (should (string= (org-to-markdown--string
                    "#+begin_src\n*not bold*\n#+end_src")
                   "```\n*not bold*\n```")))

;; --- Blockquotes ---

(ert-deftest test-org-to-md-blockquote ()
  ": text becomes > text."
  (should (string= (org-to-markdown--string ": This is quoted")
                   "> This is quoted")))

;; --- Lists ---

(ert-deftest test-org-to-md-unordered-list ()
  "Unordered list items."
  (should (string= (org-to-markdown--string "- Item one\n- Item two")
                   "- Item one\n- Item two")))

(ert-deftest test-org-to-md-ordered-list ()
  "Ordered list items."
  (should (string= (org-to-markdown--string "1. First\n2. Second")
                   "1. First\n2. Second")))

;; --- Horizontal rules ---

(ert-deftest test-org-to-md-horizontal-rule ()
  "----- becomes ---."
  (should (string= (org-to-markdown--string "-----")
                   "---")))

(ert-deftest test-org-to-md-horizontal-rule-long ()
  "Longer rule also becomes ---."
  (should (string= (org-to-markdown--string "----------")
                   "---")))

;; --- Full document ---

(ert-deftest test-org-to-md-full-document ()
  "Full document conversion."
  (let ((input "#+title: Document title\n\n* This is a header\n\n** This is a sub-header\n\nThis is a text with *Bold*.")
        (expected "# Document title\n\n## This is a header\n\n### This is a sub-header\n\nThis is a text with **Bold**."))
    (should (string= (org-to-markdown--string input) expected))))

;; --- Empty / edge cases ---

(ert-deftest test-org-to-md-empty-string ()
  "Empty string."
  (should (string= (org-to-markdown--string "") "")))

(ert-deftest test-org-to-md-blank-lines ()
  "Blank lines are preserved."
  (should (string= (org-to-markdown--string "a\n\nb") "a\n\nb")))

(ert-deftest test-org-to-md-unclosed-src ()
  "Unclosed source block gets auto-closed."
  (should (string= (org-to-markdown--string "#+begin_src\ncode")
                   "```\ncode\n```")))

(provide 'org-to-markdown.t)

;;; org-to-markdown.t.el ends here
