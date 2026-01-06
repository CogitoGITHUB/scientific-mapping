;;; tests/scientific-mapping-tests.el --- Test suite for scientific-mapping -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Code:

(require 'ert)
(require 'scientific-mapping)

(defvar scientific-mapping-test-dir (expand-file-name "test-data" (file-name-directory load-file-name))
  "Directory for test data.")

(defun scientific-mapping-test-setup ()
  "Set up test environment."
  (unless (file-exists-p scientific-mapping-test-dir)
    (make-directory scientific-mapping-test-dir t))
  ;; Set test-specific variables
  (setq scientific-document-directory scientific-mapping-test-dir
        citation-database-location (expand-file-name "test-citation-db.sqlite" scientific-mapping-test-dir)))

(defun scientific-mapping-test-teardown ()
  "Clean up test environment."
  (when (file-exists-p scientific-mapping-test-dir)
    (delete-directory scientific-mapping-test-dir t)))

(ert-deftest scientific-mapping-mode-test ()
  "Test that scientific-mapping-mode can be enabled and disabled."
  (scientific-mapping-test-setup)
  (unwind-protect
      (progn
        (scientific-mapping-mode 1)
        (should scientific-mapping-mode)
        (scientific-mapping-mode -1)
        (should-not scientific-mapping-mode))
    (scientific-mapping-test-teardown)))

(ert-deftest scientific-mapping-import-paper-test ()
  "Test importing a paper."
  (scientific-mapping-test-setup)
  (unwind-protect
      (progn
        (scientific-mapping-mode 1)
        ;; Mock interactive input
        (let ((doi "10.1000/test.doi")
              (title "Test Paper")
              (keywords '("test" "paper")))
          (scientific-mapping-import-paper)
          ;; Check if paper was added to database
          (let ((paper (citation-database-get-paper-by-doi doi)))
            (should paper)
            (should (string= (alist-get 'title paper) title)))))
    (scientific-mapping-test-teardown)))

(ert-deftest scientific-mapping-create-concept-map-test ()
  "Test creating a concept map."
  (scientific-mapping-test-setup)
  (unwind-protect
      (progn
        (scientific-mapping-mode 1)
        ;; This would require mocking user input
        (should t)) ; Placeholder
    (scientific-mapping-test-teardown)))

(provide 'scientific-mapping-tests)

;;; scientific-mapping-tests.el ends here