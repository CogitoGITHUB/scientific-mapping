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

(ert-deftest concept-relationships-get-entry-test ()
  "Test retrieving concept entries."
  (let ((test-dir (expand-file-name "test-get-entry" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq concept-relationships-path test-dir)
          (let ((id (concept-relationships-create-entry "Test Concept" '((description . "A test concept")))))
            (let ((entry (concept-relationships-get-entry id)))
              (should entry)
              (should (string= (alist-get 'title entry) "Test Concept"))
              (should (string= (alist-get 'description entry) "A test concept")))))
      (delete-directory test-dir t))))

(ert-deftest concept-relationships-relationship-types-test ()
  "Test different relationship types."
  (let ((test-dir (expand-file-name "test-types" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq concept-relationships-path test-dir)
          (let ((id1 (concept-relationships-create-entry "Concept A"))
                (id2 (concept-relationships-create-entry "Concept B")))
            ;; Test different relationship types
            (concept-relationships-add-relationship id1 id2 'child)
            (concept-relationships-add-relationship id1 id2 'parent)
            (concept-relationships-add-relationship id1 id2 'friendship)
            (concept-relationships-add-relationship id1 id2 'evidence)
            (should t))) ; Check no errors for all types
      (delete-directory test-dir t))))

(ert-deftest concept-relationships-remove-relationship-test ()
  "Test removing relationships."
  (let ((test-dir (expand-file-name "test-remove" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq concept-relationships-path test-dir)
          (let ((id1 (concept-relationships-create-entry "Source"))
                (id2 (concept-relationships-create-entry "Target")))
            (concept-relationships-add-relationship id1 id2 'child)
            (concept-relationships-remove-relationship id1 id2 'child)
            (should t))) ; Check no errors
      (delete-directory test-dir t))))

(ert-deftest concept-relationships-headlines-test ()
  "Test retrieving concept headlines."
  (let ((test-dir (expand-file-name "test-headlines" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq concept-relationships-path test-dir)
          ;; Create test concept files
          (with-temp-file (expand-file-name "concept1.org" test-dir)
            (insert "* Concept One\n:PROPERTIES:\n:ID: test-id-1\n:END:\n\nDescription of concept one."))
          (with-temp-file (expand-file-name "concept2.org" test-dir)
            (insert "* Concept Two\n:PROPERTIES:\n:ID: test-id-2\n:END:\n\nDescription of concept two."))
          (let ((headlines (concept-relationships-headlines)))
            (should (>= (length headlines) 2))))
      (delete-directory test-dir t))))

(ert-deftest concept-relationships-visualize-test ()
  "Test visualization generation."
  (let ((test-dir (expand-file-name "test-viz" temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p test-dir)
            (make-directory test-dir))
          (setq concept-relationships-path test-dir)
          (let ((id (concept-relationships-create-entry "Test Concept")))
            ;; Visualization should not error
            (concept-relationships-visualize id)
            (should t)))
      (delete-directory test-dir t))))

(provide 'concept-relationships-tests)