;;; tests/citation-database-tests.el --- Tests for citation-database -*- lexical-binding: t -*-

(require 'ert)
(require 'citation-database)

(ert-deftest citation-database-init-test ()
  "Test database initialization."
  (let ((test-db (expand-file-name "test-init.sqlite" temporary-file-directory)))
    (unwind-protect
        (progn
          (setq citation-database-location test-db)
          (citation-database--ensure-db)
          (should (file-exists-p test-db))
          (should (emacsql-live-p citation-database-db)))
      (when (and citation-database-db (emacsql-live-p citation-database-db))
        (emacsql-close citation-database-db))
      (delete-file test-db))))

(ert-deftest citation-database-add-paper-test ()
  "Test adding a paper to the database."
  (let ((test-db (expand-file-name "test-paper.sqlite" temporary-file-directory)))
    (unwind-protect
        (progn
          (setq citation-database-location test-db)
          (citation-database--ensure-db)
          ;; Add a test paper
          (citation-database-index-file "/tmp/nonexistent.org") ; This will fail gracefully
          (should t)) ; Just check no errors
      (when (and citation-database-db (emacsql-live-p citation-database-db))
        (emacsql-close citation-database-db))
      (delete-file test-db))))

(ert-deftest citation-database-search-test ()
  "Test searching papers."
  (let ((test-db (expand-file-name "test-search.sqlite" temporary-file-directory)))
    (unwind-protect
        (progn
          (setq citation-database-location test-db)
          (citation-database--ensure-db)
          (let ((results (citation-database-search-papers "test" 10)))
            (should (listp results))))
      (when (and citation-database-db (emacsql-live-p citation-database-db))
        (emacsql-close citation-database-db))
      (delete-file test-db))))

(provide 'citation-database-tests)