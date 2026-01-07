;;; core/mode.el --- Scientific Mapping Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Commentary:

;; This file defines the main scientific-mapping-mode minor mode.

;;; Code:

(defvar scientific-mapping-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for scientific-mapping-mode.")

(defvar scientific-mapping-mode nil
  "Non-nil if scientific-mapping-mode is active.")

(defvar scientific-mapping-agenda-files nil
  "List of agenda files for scientific mapping.")

(define-minor-mode scientific-mapping-mode
  "Toggle scientific-mapping-mode.
This mode enables the scientific knowledge mapping system with all
centralized key bindings and automation features."
  :lighter " SciMap"
  :keymap scientific-mapping-mode-map
  :group 'scientific-mapping
  :global t
  :init-value nil
  (cond
   (scientific-mapping-mode
    (message "Scientific mapping mode enabled"))
   (t
    (message "Scientific mapping mode disabled"))))

(provide 'core/mode)
;;; core/mode.el ends here
