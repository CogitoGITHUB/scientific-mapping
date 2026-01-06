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

(provide 'viz-engine-tests)