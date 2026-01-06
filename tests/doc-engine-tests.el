;;; tests/doc-engine-tests.el --- Tests for doc-engine -*- lexical-binding: t -*-

(require 'ert)
(require 'doc-engine)

(ert-deftest doc-engine-create-test ()
  "Test creating a scientific document."
  (let ((test-dir (expand-file-name "test-docs" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq doc-engine-directory test-dir)
          (let ((filepath (doc-engine-create :title "Test Document")))
            (should (file-exists-p filepath))
            (should (string-match-p "Test Document" (file-name-nondirectory filepath)))))
      (delete-directory test-dir t))))

(ert-deftest doc-engine-identifier-test ()
  "Test DOI-based identifier generation."
  (should (doc-engine-validate-doi "10.1000/test.doi"))
  (should-not (doc-engine-validate-doi "invalid.doi"))
  (should (string= (doc-engine-doi-to-identifier "10.1000/test.doi") "test.doi")))

(ert-deftest doc-engine-filename-test ()
  "Test filename generation."
  (let ((filename (doc-engine-format-filename "20230101T120000" "test-title" '("keyword1" "keyword2") 'org)))
    (should (string-match-p "^20230101T120000--test-title__keyword1-keyword2\\.org$" filename))))

(ert-deftest doc-engine-sluggify-title-test ()
  "Test title slugification."
  (should (string= (doc-engine-sluggify-title "Test Title With Spaces") "test-title-with-spaces"))
  (should (string= (doc-engine-sluggify-title "Test: Title (with) Punctuation!") "test-title-with-punctuation"))
  (should (string= (doc-engine-sluggify-title "UPPERCASE title") "uppercase-title")))

(ert-deftest doc-engine-extract-metadata-test ()
  "Test metadata extraction from files."
  (let ((test-file (expand-file-name "test-doc.org" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "#+TITLE: Test Document\n")
            (insert "#+DOI: 10.1000/test.doi\n")
            (insert "#+KEYWORDS: keyword1, keyword2\n")
            (insert "\nContent here."))
          (let ((metadata (doc-engine-extract-metadata test-file)))
            (should (string= (alist-get 'title metadata) "Test Document"))
            (should (string= (alist-get 'doi metadata) "10.1000/test.doi"))
            (should (equal (alist-get 'keywords metadata) '("keyword1" "keyword2")))))
      (delete-file test-file))))

(ert-deftest doc-engine-search-test ()
  "Test document search functionality."
  (let ((test-dir (expand-file-name "test-search" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq doc-engine-directory test-dir)
          ;; Create test files
          (with-temp-file (expand-file-name "doc1.org" test-dir)
            (insert "#+TITLE: Machine Learning Paper\n#+KEYWORDS: ml, ai\n\nContent about ML."))
          (with-temp-file (expand-file-name "doc2.org" test-dir)
            (insert "#+TITLE: Deep Learning Study\n#+KEYWORDS: dl, neural\n\nContent about DL."))
          (let ((results (doc-engine-search "learning")))
            (should (>= (length results) 2))))
      (delete-directory test-dir t))))

(ert-deftest doc-engine-stats-test ()
  "Test statistics generation."
  (let ((test-dir (expand-file-name "test-stats" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq doc-engine-directory test-dir)
          ;; Create test files
          (with-temp-file (expand-file-name "doc1.org" test-dir)
            (insert "#+TITLE: Test Doc 1\n#+KEYWORDS: test\n\nContent."))
          (with-temp-file (expand-file-name "doc2.org" test-dir)
            (insert "#+TITLE: Test Doc 2\n#+KEYWORDS: test\n\nContent."))
          (let ((stats (doc-engine-stats test-dir)))
            (should (numberp (alist-get 'total-files stats)))
            (should (>= (alist-get 'total-files stats) 2))))
      (delete-directory test-dir t))))

(ert-deftest doc-engine-create-link-test ()
  "Test link creation."
  (let ((test-file (expand-file-name "test-link.org" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "#+TITLE: Target Document\n#+DOI: 10.1000/target.doi\n\nContent."))
          (let ((link (doc-engine-create-link test-file "Test Link")))
            (should (string-match-p "\\[\\[.*\\]\\[Test Link\\]\\]" link))))
      (delete-file test-file))))

(provide 'doc-engine-tests)