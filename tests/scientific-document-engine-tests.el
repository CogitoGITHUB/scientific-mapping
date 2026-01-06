;;; tests/scientific-document-engine-tests.el --- Tests for scientific-document-engine -*- lexical-binding: t -*-

(require 'ert)
(require 'scientific-document-engine)

(ert-deftest scientific-document-create-test ()
  "Test creating a scientific document."
  (let ((test-dir (expand-file-name "test-docs" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq scientific-document-directory test-dir)
          (let ((filepath (scientific-document-create :title "Test Document")))
            (should (file-exists-p filepath))
            (should (string-match-p "Test Document" (file-name-nondirectory filepath)))))
      (delete-directory test-dir t))))

(ert-deftest scientific-document-identifier-test ()
  "Test DOI-based identifier generation."
  (should (scientific-document-validate-doi "10.1000/test.doi"))
  (should-not (scientific-document-validate-doi "invalid.doi"))
  (should (string= (scientific-document-doi-to-identifier "10.1000/test.doi") "test.doi")))

(ert-deftest scientific-document-filename-test ()
  "Test filename generation."
  (let ((filename (scientific-document-format-filename "20230101T120000" "test-title" '("keyword1" "keyword2") 'org)))
    (should (string-match-p "^20230101T120000--test-title__keyword1-keyword2\\.org$" filename))))

(provide 'scientific-document-engine-tests)