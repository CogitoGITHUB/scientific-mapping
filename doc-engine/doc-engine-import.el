;;; doc-engine-import.el --- Smart import functionality -*- lexical-binding: t -*-

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

;; Smart import functionality for various sources.

;;; Code:

(require 'doc-engine-core)
(require 'doc-engine-create)

;;;###autoload
(defun doc-engine-smart-import (source)
  "Smart import from various sources with automatic processing."
  (interactive
   (list (completing-read "Import from: " '("clipboard" "url" "doi" "file") nil t)))
  (cond
   ((string= source "clipboard")
    (doc-engine-import-from-clipboard))
   ((string= source "url")
    (doc-engine-import-from-url))
   ((string= source "doi")
    (call-interactively #'academic-apis-resolve-doi))
   ((string= source "file")
    (doc-engine-import-from-file))))

(defun doc-engine-import-from-clipboard ()
  "Import content from clipboard as new document."
  (let ((content (current-kill 0)))
    (if (string-empty-p content)
        (message "Clipboard is empty")
      (let ((title (read-string "Document title: " (doc-engine-extract-title-from-content content))))
        (doc-engine-create :title title :abstract content)))))

(defun doc-engine-extract-title-from-content (content)
  "Extract potential title from content."
  (let ((lines (split-string content "\n" t)))
    (or (car (seq-filter (lambda (line) (> (length line) 10)) lines))
        "Imported Document")))

(defun doc-engine-import-from-url (url)
  "Import content from URL."
  (interactive "sURL to import: ")
  (message "URL import not yet implemented - use academic APIs for now"))

(defun doc-engine-import-from-file (file)
  "Import and convert external file."
  (interactive "fFile to import: ")
  (message "File import not yet implemented - copy content manually for now"))

(provide 'doc-engine-import)

;;; doc-engine-import.el ends here