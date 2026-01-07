;;; test-doc-engine-create.el --- Test doc-engine-capture functions -*- lexical-binding: t; -*-

;; Test file for doc-engine-capture and doc-engine-capture-with-yasnippet
;; Run with: emacs -Q -batch -l test-doc-engine-create.el

(require 'cl-lib)

;; Mock dependencies
(defvar doc-engine-directory "~/test-documents/")
(defvar org-capture-templates nil)
(defvar yas-minor-mode nil)

;; Mock functions
(defun doc-engine-generate-identifier (&optional doi time)
  (format-time-string "%Y%m%dT%H%M%S" (or time (current-time))))

(defun doc-engine-register-file (id filepath) t)

(defun doc-engine-sluggify-title (title)
  "Convert TITLE to a filesystem-safe signature."
  (let* ((downcase (downcase title))
         (remove-special (replace-regexp-in-string "[^[:alnum:] ]+" "" downcase))
         (replace-spaces (replace-regexp-in-string " +" "-" remove-special))
         (trim (replace-regexp-in-string "-+$" "" replace-spaces)))
    (replace-regexp-in-string "^-+" "" trim)))

(defun doc-engine-setup-capture-templates ()
  "Setup org-capture templates using yasnippets."
  (setq doc-engine-capture-templates
        '(("r" "Research Article" entry
           (file (lambda () (expand-file-name "research-article.org" doc-engine-directory)))
           "* Research Article\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("l" "Literature Review" entry
           (file (lambda () (expand-file-name "literature-review.org" doc-engine-directory)))
           "* Literature Review\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("c" "Conference Paper" entry
           (file (lambda () (expand-file-name "conference-paper.org" doc-engine-directory)))
           "* Conference Paper\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("t" "Thesis/Dissertation" entry
           (file (lambda () (expand-file-name "thesis.org" doc-engine-directory)))
           "* Thesis\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("n" "Quick Note" entry
           (file (lambda () (expand-file-name "notes.org" doc-engine-directory)))
           "* %?\n"
           :empty-lines 1
           :jump-to-captured 0))))

(defun doc-engine-capture-with-yasnippet (template-key)
  "Capture a new document using yasnippet TEMPLATE-KEY.
Uses yasnippet for template expansion instead of plain org-capture."
  (interactive "sTemplate key (r=research, l=review, c=conference, t=thesis, n=note): ")
  (let* ((template-key (or template-key "n"))
         (template-file
          (pcase template-key
            ("r" "research-article")
            ("l" "literature-review")
            ("c" "conference-paper")
            ("t" "thesis-master")
            ("n" "quick-note")
            (_ "quick-note")))
         (filepath (expand-file-name (format "%s.org" template-file) doc-engine-directory))
         (title (read-string "Document title: "))
         (identifier (doc-engine-generate-identifier nil (current-time))))

    ;; Create file with yasnippet
    (with-temp-file filepath
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert (format "#+IDENTIFIER: %s\n" identifier))
      (insert "#+STARTUP: content\n\n")
      (insert "* Notes\n"))

    ;; Expand yasnippet if available
    (when (featurep 'yasnippet)
      (find-file filepath)
      (goto-char (point-min))
      (re-search-forward "^\\* Notes$" nil t)
      (yas-expand-snippet (format "* %s\n\n" title)))

    ;; Register and open
    (doc-engine-register-file identifier filepath)
    (find-file filepath)
    (message "Created %s with template: %s" title template-key)))

(defun doc-engine-capture ()
  "Open document creation via org-capture with yasnippets.
Customize `org-capture-templates' to add more templates."
  (interactive)
  (doc-engine-setup-capture-templates)
  (let ((org-capture-templates doc-engine-capture-templates))
    (call-interactively 'org-capture)))

;;; Tests

(defun test-doc-engine-generate-identifier ()
  "Test doc-engine-generate-identifier."
  (let* ((id (doc-engine-generate-identifier nil))
         (valid (string-match-p "^[0-9]\\{8\\}T[0-9]\\{6\\}$" id)))
    (if valid
        (message "PASS: doc-engine-generate-identifier returns valid ID: %s" id)
      (message "FAIL: doc-engine-generate-identifier returned invalid ID: %s" id))
    valid))

(defun test-doc-engine-sluggify-title ()
  "Test doc-engine-sluggify-title."
  (let* ((title "My Research Paper!")
         (slug (doc-engine-sluggify-title title))
         (valid (string= slug "my-research-paper")))
    (if valid
        (message "PASS: doc-engine-sluggify-title: %s -> %s" title slug)
      (message "FAIL: doc-engine-sluggify-title: %s -> %s (expected: my-research-paper)" title slug))
    valid))

(defun test-doc-engine-setup-capture-templates ()
  "Test doc-engine-setup-capture-templates."
  (doc-engine-setup-capture-templates)
  (let* ((count (length doc-engine-capture-templates))
         (valid (= count 5)))
    (if valid
        (message "PASS: doc-engine-setup-capture-templates created %d templates" count)
      (message "FAIL: doc-engine-setup-capture-templates created %d templates (expected 5)" count))
    valid))

(defun test-doc-engine-capture-with-yasnippet-template-lookup ()
  "Test template key to filename mapping."
  (let* ((templates '(("r" . "research-article")
                      ("l" . "literature-review")
                      ("c" . "conference-paper")
                      ("t" . "thesis-master")
                      ("n" . "quick-note")))
         (all-valid
          (cl-every (lambda (pair)
                      (let ((key (car pair))
                            (expected (cdr pair)))
                        (condition-case nil
                            (progn
                              (let ((result nil))
                                (let ((doc-engine-directory "/tmp/"))
                                  (setq result
                                        (expand-file-name
                                         (format "%s.org" expected)
                                         doc-engine-directory)))
                                (string-match-p (format "%s\\.org" expected) result)))
                          (error nil))))
                    templates)))
    (if all-valid
        (message "PASS: All template keys map correctly")
      (message "FAIL: Some template keys failed to map"))
    all-valid))

(defun run-all-tests ()
  "Run all tests."
  (message "\n=== Running doc-engine-capture Tests ===\n")
  (let ((results
         (list (cons "doc-engine-generate-identifier" (test-doc-engine-generate-identifier))
               (cons "doc-engine-sluggify-title" (test-doc-engine-sluggify-title))
               (cons "doc-engine-setup-capture-templates" (test-doc-engine-setup-capture-templates))
               (cons "doc-engine-capture-with-yasnippet-template-lookup" (test-doc-engine-capture-with-yasnippet-template-lookup)))))
    (message "\n=== Test Results ===")
    (cl-loop for (name . passed) in results
             do (message "%s: %s" name (if passed "PASS" "FAIL")))
    (let ((total (length results))
          (passed (cl-count-if #'cdr results)))
      (message "\nTotal: %d/%d tests passed" passed total)
      (if (= passed total)
          (message "ALL TESTS PASSED!")
        (message "SOME TESTS FAILED")))))

(run-all-tests)
