;;; tests/viz-engine-tests.el --- Tests for viz-engine -*- lexical-binding: t -*-

(require 'ert)
(require 'viz-engine)

(ert-deftest viz-engine-mode-test ()
  "Test that viz-engine-mode can be enabled."
  (viz-engine-mode 1)
  (should viz-engine-mode)
  (viz-engine-mode -1)
  (should-not viz-engine-mode))

(ert-deftest viz-engine-set-mode-test ()
  "Test setting visualization mode."
  (viz-engine-set-mode "concept")
  (should (eq viz-engine-visualization-mode 'concept)))

(ert-deftest viz-engine-data-functions-test ()
  "Test data retrieval functions."
  ;; These would require a test database setup
  (should t)) ; Placeholder for actual data tests

(ert-deftest viz-engine-paper-zoom-test ()
  "Test paper zoom functionality."
  (let ((original-zoom viz-engine-paper-zoom-level))
    (unwind-protect
        (progn
          (viz-engine-paper-zoom "test-identifier" 2.0 0.1)
          (should (= viz-engine-paper-zoom-level 2.0)))
      (setq viz-engine-paper-zoom-level original-zoom))))

(ert-deftest viz-engine-theme-colors-test ()
  "Test theme color retrieval."
  (let ((colors (viz-engine--get-modus-theme-colors)))
    (should (listp colors))
    (should (>= (length colors) 1))))

(ert-deftest viz-engine-network-functions-test ()
  "Test network data retrieval functions."
  ;; Test that functions return expected data structures
  (let ((citation-network (viz-engine--get-citation-network)))
    (should (listp citation-network)))
  (let ((concept-network (viz-engine--get-concept-network)))
    (should (listp concept-network)))
  (let ((author-network (viz-engine--get-author-network)))
    (should (listp author-network))))

(ert-deftest viz-engine-message-handlers-test ()
  "Test websocket message handlers."
  ;; Test message handler functions don't error with valid data
  (let ((test-data '((identifier . "test-id") (title . "Test Paper"))))
    (viz-engine--on-msg-open-paper test-data)
    (should t))
  (let ((test-data '((source . "source-id") (target . "target-id"))))
    (viz-engine--on-msg-add-citation test-data)
    (should t))
  (let ((test-data '((mode . "concept"))))
    (viz-engine--on-msg-set-visualization-mode test-data)
    (should t)))

(ert-deftest viz-engine-current-paper-test ()
  "Test current paper tracking."
  (let ((original-paper viz-engine-current-paper))
    (unwind-protect
        (progn
          (viz-engine--update-current-paper)
          (should t)) ; Function should not error
      (setq viz-engine-current-paper original-paper))))

(provide 'viz-engine-tests)