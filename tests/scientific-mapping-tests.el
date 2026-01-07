;;; tests/scientific-mapping-tests.el --- Test suite for scientific-mapping -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Code:

(require 'ert)
(require 'scientific-mapping)
(require 'doc-engine)
(require 'cite/index)
;; (require 'citation-database)
;; (require 'viz-engine)
(require 'viz/viz-engine nil t)
;; (require 'concept-relationships)

(defvar scientific-mapping-test-dir (expand-file-name "test-data" (file-name-directory load-file-name))
  "Directory for test data.")

(defun scientific-mapping-test-setup ()
  "Set up test environment."
  (unless (file-exists-p scientific-mapping-test-dir)
    (make-directory scientific-mapping-test-dir t))
  ;; Set test-specific variables
  (setq doc-engine-directory scientific-mapping-test-dir
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

;;; Keybinding tests

(ert-deftest scientific-mapping-keybindings-test ()
  "Test that keybindings are properly defined."
  (scientific-mapping-test-setup)
  (unwind-protect
      (progn
        (scientific-mapping-mode 1)
        ;; Test that keymap exists
        (should (boundp 'scientific-mapping-mode-map))
        ;; Test specific keybindings
        (should (commandp 'doc-engine-capture))
        (should (commandp 'doc-engine-capture-with-yasnippet))
        (should (commandp 'doc-engine-quick-note))
        ;; Verify keybindings are bound
        (let ((map scientific-mapping-mode-map))
          (should (eq (lookup-key map (kbd "C-c s C")) 'doc-engine-capture))
          (should (eq (lookup-key map (kbd "C-c s y")) 'doc-engine-capture-with-yasnippet))))
    (scientific-mapping-test-teardown)))

(ert-deftest scientific-mapping-org-capture-integration-test ()
  "Test org-capture integration."
  (scientific-mapping-test-setup)
  (unwind-protect
      (progn
        (require 'org-capture)
        (should (featurep 'org-capture))
        ;; Test that capture templates can be set up
        (let ((test-dir (expand-file-name "test-capture" temporary-file-directory)))
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq doc-engine-directory test-dir)
          (doc-engine-setup-capture-templates)
          (should doc-engine-capture-templates)
          (should (= (length doc-engine-capture-templates) 5))
          (delete-directory test-dir t)))
    (scientific-mapping-test-teardown)))

(provide 'scientific-mapping-tests)

;;; scientific-mapping-tests.el ends here