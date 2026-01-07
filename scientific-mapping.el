;;; scientific-mapping.el --- Scientific Knowledge Mapping System  -*- lexical-binding: t; -*-

;; This is the main entry point for the scientific-mapping package.
;; All components are loaded automatically via this file.

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/scientific-mapping
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (emacsql "3.0.0") (org "9.5") (yasnippet "0.14.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Scientific Knowledge Mapping System - An integrated platform for
;; scientific research, literature management, and knowledge mapping.
;;
;; This file auto-loads all components from the scientific-mapping/ subdirectory.
;;
;; Usage:
;;   (require 'scientific-mapping)
;;   (scientific-mapping-mode 1)
;;
;; Components are organized in subdirectories under scientific-mapping/:
;;   core/     - Core utilities and helpers
;;   doc/      - Document management (doc-engine)
;;   cite/     - Citation database
;;   viz/      - 3D visualization
;;   ai/       - AI integration
;;   workflow/ - Workflow automation
;;   ui/       - User interface components
;;   integrations/ - External service integrations

;;; Code:

;;;; Auto-loading Framework

(defvar scientific-mapping-root-dir
  (expand-file-name "scientific-mapping" (file-name-directory load-file-name))
  "Root directory of the scientific-mapping package.")

(defun scientific-mapping--load-directory (dir)
  "Load all .el files in DIR."
  (let ((default-directory dir))
    (load (expand-file-name "index" dir) t t)))

;; Load all components from the scientific-mapping/ subdirectory
(scientific-mapping--load-directory (expand-file-name "core" scientific-mapping-root-dir))
(scientific-mapping--load-directory (expand-file-name "doc" scientific-mapping-root-dir))
(scientific-mapping--load-directory (expand-file-name "cite" scientific-mapping-root-dir))
(scientific-mapping--load-directory (expand-file-name "viz" scientific-mapping-root-dir))
(scientific-mapping--load-directory (expand-file-name "ai" scientific-mapping-root-dir))
(scientific-mapping--load-directory (expand-file-name "workflow" scientific-mapping-root-dir))
(scientific-mapping--load-directory (expand-file-name "ui" scientific-mapping-root-dir))
(scientific-mapping--load-directory (expand-file-name "integrations" scientific-mapping-root-dir))

;;;; Version and Package Info

(defvar scientific-mapping-version "1.0.0"
  "Version number of scientific-mapping.")

(defvar scientific-mapping-package-aliases
  '(("scientific-mapping" . scientific-mapping))
  "Package name aliases for compatibility.")

(provide 'scientific-mapping)

;;; scientific-mapping.el ends here
