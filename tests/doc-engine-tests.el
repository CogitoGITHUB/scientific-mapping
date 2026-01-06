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

(provide 'doc-engine-engine-tests)