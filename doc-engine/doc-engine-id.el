;;; doc-engine-id.el --- Internal ID mapping system -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/doc-engine
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Internal ID mapping system for document management.

;;; Code:

(require 'doc-engine-core)

;;;; Internal ID Mapping System

(defvar doc-engine-id-map (make-hash-table :test 'equal)
  "Internal mapping from document IDs to file paths.")

(defvar doc-engine-id-map-file (expand-file-name "doc-engine-id-map.el" user-emacs-directory)
  "File to persist ID mappings.")

(defun doc-engine-register-file (id filepath)
  "Register ID to file path mapping."
  (puthash id filepath doc-engine-id-map)
  (doc-engine-save-id-map))

(defun doc-engine-find-file-by-id (id)
  "Find file path for given ID."
  (gethash id doc-engine-id-map))

(defun doc-engine-get-file-id (filepath)
  "Get ID for given file path."
  (let ((filepath (expand-file-name filepath)))
    (catch 'found
      (maphash (lambda (id path)
                 (when (string= (expand-file-name path) filepath)
                   (throw 'found id)))
               doc-engine-id-map)
      nil)))

(defun doc-engine-save-id-map ()
  "Save ID mapping to disk."
  (with-temp-file doc-engine-id-map-file
    (insert ";; doc-engine ID to file mapping - Auto-generated\n")
    (insert "(setq doc-engine-id-map ")
    (prin1 doc-engine-id-map (current-buffer))
    (insert ")\n")))

(defun doc-engine-load-id-map ()
  "Load ID mapping from disk."
  (when (file-exists-p doc-engine-id-map-file)
    (load doc-engine-id-map-file)))

;; Load mappings on startup
(doc-engine-load-id-map)

(provide 'doc-engine-id)

;;; doc-engine-id.el ends here