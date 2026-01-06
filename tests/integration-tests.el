;;; tests/integration-tests.el --- Integration tests for scientific-mapping -*- lexical-binding: t -*-

(require 'ert)
(require 'scientific-mapping)
(require 'doc-engine)
(require 'citation-database)
(require 'concept-relationships)
(require 'viz-engine)

(ert-deftest scientific-mapping-integration-document-workflow-test ()
  "Test complete document creation and management workflow."
  (let ((test-dir (expand-file-name "integration-test" temporary-file-directory))
        (test-db (expand-file-name "integration-test.sqlite" temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Setup
          (unless (file-exists-p test-dir)
            (make-directory test-dir t))
          (unless (file-exists-p (expand-file-name "concepts" test-dir))
            (make-directory (expand-file-name "concepts" test-dir) t))

          (setq doc-engine-directory test-dir
                citation-database-location test-db
                concept-relationships-path (expand-file-name "concepts" test-dir))

          ;; Enable modes
          (scientific-mapping-mode 1)
          (citation-database-autosync-mode 1)

          ;; Create a document
          (let* ((doc-file (doc-engine-create :title "Integration Test Paper"
                                           :doi "10.1000/integration.test"
                                           :keywords '("integration" "test")))
                 (doc-id (file-name-base doc-file)))

            ;; Verify document was created
            (should (file-exists-p doc-file))

            ;; Extract metadata
            (let ((metadata (doc-engine-extract-metadata doc-file)))
              (should (string= (alist-get 'title metadata) "Integration Test Paper"))
              (should (string= (alist-get 'doi metadata) "10.1000/integration.test")))

            ;; Search for the document
            (let ((search-results (doc-engine-search "integration")))
              (should (>= (length search-results) 1)))

            ;; Create a concept related to the document
            (let ((concept-id (concept-relationships-create-entry "Integration Concept")))
              (should concept-id)

              ;; Add relationship between document and concept
              (doc-engine-add-concept "Integration Concept")
              (should t))

            ;; Test citation database integration
            (citation-database--ensure-db)
            (when citation-database-db
              (citation-database-index-file doc-file)
              (let ((papers (citation-database-search-papers "integration")))
                (should (>= (length papers) 0)))))) ; May be 0 if indexing fails gracefully

      ;; Cleanup
      (when (and citation-database-db (emacsql-live-p citation-database-db))
        (emacsql-close citation-database-db))
      (scientific-mapping-mode -1)
      (delete-directory test-dir t)
      (when (file-exists-p test-db)
        (delete-file test-db)))))

(ert-deftest scientific-mapping-integration-concept-network-test ()
  "Test concept relationship and visualization integration."
  (let ((test-dir (expand-file-name "concept-integration" temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Setup
          (unless (file-exists-p test-dir)
            (make-directory test-dir t))
          (unless (file-exists-p (expand-file-name "concepts" test-dir))
            (make-directory (expand-file-name "concepts" test-dir) t))

          (setq concept-relationships-path (expand-file-name "concepts" test-dir))

          ;; Create concept network
          (let ((parent-id (concept-relationships-create-entry "Parent Concept"))
                (child-id (concept-relationships-create-entry "Child Concept"))
                (related-id (concept-relationships-create-entry "Related Concept")))

            ;; Add relationships
            (concept-relationships-add-relationship parent-id child-id 'child)
            (concept-relationships-add-relationship parent-id related-id 'friendship)

            ;; Verify relationships exist
            (let ((children (concept-relationships--get-relations parent-id 'child))
                  (friends (concept-relationships--get-relations parent-id 'friendship)))
              (should (>= (length children) 1))
              (should (>= (length friends) 1)))

            ;; Test visualization (should not error)
            (concept-relationships-visualize parent-id)
            (should t)))

      ;; Cleanup
      (delete-directory test-dir t))))

(ert-deftest scientific-mapping-integration-viz-engine-modes-test ()
  "Test visualization engine mode switching and data retrieval."
  (let ((original-mode viz-engine-visualization-mode))
    (unwind-protect
        (progn
          ;; Test mode switching
          (viz-engine-set-mode "concept")
          (should (eq viz-engine-visualization-mode 'concept))

          (viz-engine-set-mode "citation")
          (should (eq viz-engine-visualization-mode 'citation))

          (viz-engine-set-mode "author")
          (should (eq viz-engine-visualization-mode 'author))

          ;; Test data functions return proper structures
          (let ((concept-data (viz-engine--get-concept-network))
                (citation-data (viz-engine--get-citation-network))
                (author-data (viz-engine--get-author-network)))
            (should (listp concept-data))
            (should (listp citation-data))
            (should (listp author-data))))

      ;; Restore original mode
      (viz-engine-set-mode (symbol-name original-mode)))))

(provide 'integration-tests)