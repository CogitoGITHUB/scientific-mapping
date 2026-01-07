;;; doc-engine-manage.el --- Document management and operations -*- lexical-binding: t -*-

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

;; Document management, metadata extraction, linking, concepts, search, status, stats, PDF.

;;; Code:

(require 'doc-engine-core)
(require 'doc-engine-create)

;;;; Scientific Document Metadata Extraction

(defun doc-engine-extract-metadata (file)
  "Extract metadata from scientific document FILE."
  (with-current-buffer (find-file-noselect file)
    (let ((title (org-entry-get (point-min) "TITLE"))
          (doi (org-entry-get (point-min) "DOI"))
          (identifier (org-entry-get (point-min) "IDENTIFIER"))
          (date (org-entry-get (point-min) "DATE"))
          (keywords (org-entry-get (point-min) "SCIENTIFIC_TAGS")))
      (list
       :title title
       :doi doi
       :identifier identifier
       :date date
       :keywords (when keywords (split-string keywords ":"))
       :file file))))

;;;; Scientific Document Linking

(defun doc-engine-create-link (target-file &optional description)
  "Create a link to TARGET-FILE with optional DESCRIPTION.
Link is inserted at point."
  (interactive
   (let ((file (completing-read "Link to document: "
                                (doc-engine-all-files)))
         (desc (read-string "Link description (optional): ")))
     (list file (when (> (length desc) 0) desc))))

  (let* ((metadata (doc-engine-extract-metadata target-file))
         (title (or description (plist-get metadata :title)))
         (identifier (plist-get metadata :identifier)))
    (insert (format "[[scientific:%s][%s]]" identifier title))
    (doc-engine-register-citation identifier)))

(defun doc-engine-register-citation (target-identifier)
  "Register citation to TARGET-IDENTIFIER in current document."
  (let ((citations (org-entry-get (point-min) doc-engine-citations-property-name))
        (new-citations (if citations
                          (concat citations " " target-identifier)
                        target-identifier)))
    (org-entry-put (point-min) doc-engine-citations-property-name new-citations)))

;;;; Scientific Document Concepts

(defun doc-engine-add-concept (concept-name &optional properties)
  "Add a concept link with PROPERTIES to current document.
PROPERTIES is a plist of concept attributes."
  (interactive "sConcept name: ")
  (let ((concept-link (format "[[concept:%s][%s]]" concept-name concept-name)))
    (insert concept-link)
    (message "Added concept: %s" concept-name)))

(defun doc-engine-extract-concepts ()
  "Extract concepts from abstract and content of current document."
  (interactive)
  (let ((abstract (org-entry-get (point-min) "ABSTRACT"))
        (content (buffer-substring (point-min) (point-max)))
        (concepts '()))

    ;; Simple concept extraction - can be enhanced with NLP
    (when abstract
      (setq concepts (append concepts (doc-engine--extract-keywords abstract))))

    (setq concepts (append concepts (doc-engine--extract-keywords content)))

    ;; Remove duplicates and add to document
    (setq concepts (seq-uniq concepts))
    (mapc 'doc-engine-add-concept concepts)
    (message "Extracted %d concepts" (length concepts))))

(defun doc-engine--extract-keywords (text)
  "Extract potential keywords from TEXT.
This is a simple implementation - can be enhanced with NLP."
  (let* ((words (split-string text "[^[:alnum:]]+"))
         (filtered (seq-filter
                   (lambda (word)
                     (and (> (length word) 5)  ; Longer words are more likely concepts
                          (not (member word
                                      '("abstract" "introduction" "conclusion"
                                        "methodology" "results" "discussion"
                                        "this" "that" "these" "those")))))
                   words)))
    (seq-uniq filtered)))

;;;; Scientific Document Search

(defun doc-engine-search (query)
  "Search scientific documents for QUERY."
  (interactive "sSearch query: ")
  (let ((all-files (doc-engine-all-files))
        (matches '()))
    (dolist (file all-files)
      (let* ((buffer (find-file-noselect file))
             (content (with-current-buffer buffer
                        (buffer-substring (point-min) (point-max)))))
        (when (string-match-p (regexp-quote query) content)
          (push file matches))))
    (if matches
        (let ((choice (completing-read "Open matching document: " matches)))
          (find-file choice))
      (message "No matches found for: %s" query))))

;;;; Scientific Document Reading Status

(defconst doc-engine-reading-statuses
  '("unread" "reading" "annotated" "completed" "reviewed")
  "Possible reading status values for scientific documents.")

(defun doc-engine-set-reading-status (status)
  "Set reading status of current document to STATUS."
  (interactive
   (list (completing-read "Reading status: "
                          doc-engine-reading-statuses)))
  (org-entry-put (point-min) "READING_STATUS" status)
  (message "Reading status set to: %s" status))

(defun doc-engine-get-reading-status ()
  "Get reading status of current document."
  (org-entry-get (point-min) "READING_STATUS"))

(defun doc-engine-set-priority (priority)
  "Set priority of current document to PRIORITY."
  (interactive
   (list (completing-read "Priority: " '("low" "medium" "high" "critical"))))
  (org-entry-put (point-min) "PRIORITY" priority)
  (message "Priority set to: %s" priority))

;;;; Scientific Document Statistics

(defun doc-engine-stats (&optional directory)
  "Display statistics for scientific documents in DIRECTORY."
  (interactive)
  (let* ((files (doc-engine-all-files directory))
         (total (length files))
         (by-status (make-hash-table :test 'equal))
         (by-priority (make-hash-table :test 'equal)))

    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (let ((status (doc-engine-get-reading-status))
              (priority (org-entry-get (point-min) "PRIORITY")))
          (when status
            (puthash status (1+ (gethash status by-status 0)) by-status))
          (when priority
            (puthash priority (1+ (gethash priority by-priority 0)) by-priority)))))

    (message "Scientific Document Statistics:")
    (message "  Total documents: %d" total)
    (message "  By reading status:")
    (maphash (lambda (key value)
               (message "    %s: %d" key value))
             by-status)
    (message "  By priority:")
    (maphash (lambda (key value)
               (message "    %s: %d" key value))
             by-priority)))

;;;; Scientific Document PDF Management

(defun doc-engine-attach-pdf (pdf-file)
  "Attach PDF-FILE to current scientific document."
  (interactive
   (list (read-file-name "PDF file to attach: ")))
  (let* ((doc-file (buffer-file-name))
         (pdf-basename (file-name-nondirectory pdf-file))
         (pdf-dest (concat (file-name-sans-extension doc-file)
                          ".pdf")))
    (copy-file pdf-file pdf-dest)
    (message "Attached PDF: %s" pdf-dest)))

(defun doc-engine-open-pdf ()
  "Open PDF attached to current scientific document."
  (interactive)
  (let* ((doc-file (buffer-file-name))
         (pdf-file (concat (file-name-sans-extension doc-file)
                          ".pdf")))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "No PDF found for this document"))))

(provide 'doc-engine-manage)

;;; doc-engine-manage.el ends here