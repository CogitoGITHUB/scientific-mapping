;;; tests/concept-relationships-tests.el --- Tests for concept-relationships -*- lexical-binding: t -*-

(require 'ert)
(require 'concept-relationships)

(ert-deftest concept-relationships-create-entry-test ()
  "Test creating a concept entry."
  (let ((test-dir (expand-file-name "test-concepts" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq concept-relationships-path test-dir)
          (let ((id (concept-relationships-create-entry "Test Concept")))
            (should id)
            (should (string-match-p "^[0-9a-f]+$" id))))
      (delete-directory test-dir t))))

(ert-deftest concept-relationships-add-relationship-test ()
  "Test adding relationships between concepts."
  (let ((test-dir (expand-file-name "test-relations" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq concept-relationships-path test-dir)
          ;; Create test entries
          (let ((id1 (concept-relationships-create-entry "Parent Concept"))
                (id2 (concept-relationships-create-entry "Child Concept")))
            (concept-relationships-add-relationship id1 id2 'child)
            (should t))) ; Check no errors
      (delete-directory test-dir t))))

(provide 'concept-relationships-tests)