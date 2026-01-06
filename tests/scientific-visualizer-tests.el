;;; tests/scientific-visualizer-tests.el --- Tests for scientific-visualizer -*- lexical-binding: t -*-

(require 'ert)
(require 'scientific-visualizer)

(ert-deftest scientific-visualizer-mode-test ()
  "Test that scientific-visualizer-mode can be enabled."
  (scientific-visualizer-mode 1)
  (should scientific-visualizer-mode)
  (scientific-visualizer-mode -1)
  (should-not scientific-visualizer-mode))

(ert-deftest scientific-visualizer-set-mode-test ()
  "Test setting visualization mode."
  (scientific-visualizer-set-mode "concept")
  (should (eq scientific-visualizer-visualization-mode 'concept)))

(provide 'scientific-visualizer-tests)