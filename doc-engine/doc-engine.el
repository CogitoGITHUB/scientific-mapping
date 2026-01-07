;;; doc-engine.el --- Document management system -*- lexical-binding: t -*-

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

;; doc-engine provides a comprehensive system for managing documents,
;; papers, research notes, and academic documents. It is based on the idea that
;; documents should follow a predictable, DOI-based naming scheme that
;; facilitates easy identification, citation, and linking between documents.
;;
;; The system supports:
;; - Papers with DOI-based identifiers
;; - Research notes and annotations
;; - Automatic metadata extraction
;; - Citation management
;; - Concept extraction and linking
;; - PDF attachment handling
;; - Multi-format support (org, markdown, text)
;;
;; doc-engine streamlines the creation of documents while
;; providing facilities to link between them, extract concepts, and manage
;; bibliographic information.

;;; Code:

;; Load submodules
(require 'doc-engine-core)
(require 'doc-engine-id)
(require 'doc-engine-auto)
(require 'doc-engine-create)
(require 'doc-engine-manage)
(require 'doc-engine-import)
(require 'doc-engine-utils)

;;;; Scientific Document Creation

(defun doc-engine-create (&rest args)
  "Create a new scientific document with optional properties.
ARGS is a plist of: `:title', `:doi', `:keywords', `:subdirectory',
`:citation', `:abstract', `:methodology', `:results', `:date', `:file-type'.

When called interactively, prompt for information based on
`doc-engine-prompts'."
  (interactive)
  (let* ((title (or (plist-get args :title)
                    (when (memq 'title doc-engine-prompts)
                      (doc-engine-read-prompt "Title of paper/note: "))))
         (doi (or (plist-get args :doi)
                  (when (memq 'doi doc-engine-prompts)
                    (doc-engine-read-doi-prompt))))
         (keywords (or (plist-get args :keywords)
                       (when (memq 'keywords doc-engine-prompts)
                         (completing-read-multiple
                          "Enter keywords (comma-separated): "
                          '("machine-learning" "nlp" "deep-learning" "ai" "statistics"
                            "experimental" "theoretical" "review" "methodology")))))
         (subdir (or (plist-get args :subdirectory)
                     (when (memq 'subdirectory doc-engine-prompts)
                       (completing-read
                        "Subdirectory (optional): "
                        (doc-engine-directories)))))
         (citation (or (plist-get args :citation)
                       (when (memq 'citation doc-engine-prompts)
                         (read-string "Citation key (optional): "))))
         (abstract (or (plist-get args :abstract)
                       (when (memq 'abstract doc-engine-prompts)
                         (read-string "Abstract (optional): "))))
         (methodology (or (plist-get args :methodology)
                          (when (memq 'methodology doc-engine-prompts)
                            (read-string "Methodology (optional): "))))
         (results (or (plist-get args :results)
                      (when (memq 'results doc-engine-prompts)
                        (read-string "Results (optional): "))))
         (date (or (plist-get args :date)
                   (current-time)))
         (file-type (or (plist-get args :file-type)
                        doc-engine-file-type))
         (target-dir (if subdir
                        (expand-file-name subdir
                                        (car (if (listp doc-engine-directory)
                                                doc-engine-directory
                                              (list doc-engine-directory))))
                      (car (if (listp doc-engine-directory)
                               doc-engine-directory
                             (list doc-engine-directory)))))

    (unless title
      (error "Title is required for creating a scientific document"))

    (unless (file-exists-p target-dir)
      (make-directory target-dir t))

    (let* ((identifier (doc-engine-generate-identifier doi date))
           (signature (doc-engine-sluggify-title title))
           (display-filename (doc-engine-format-filename signature keywords file-type))
           (actual-filename (format "%s.%s" display-filename (doc-engine-file-extension file-type)))
           (filepath (expand-file-name actual-filename target-dir))
           (front-matter (doc-engine-format-front-matter
                          title doi keywords identifier date
                          citation abstract methodology results file-type)))

      ;; Handle filename conflicts by adding number suffix
      (let ((counter 1)
            (base-display-filename display-filename))
        (while (file-exists-p filepath)
          (setq display-filename (format "%s-%d" base-display-filename counter))
          (setq actual-filename (format "%s.%s" display-filename (doc-engine-file-extension file-type)))
          (setq filepath (expand-file-name actual-filename target-dir))
          (setq counter (1+ counter))))

      (with-temp-file filepath
        (insert front-matter)
        (unless (eq file-type 'text)
          (insert "\n\n")
          (insert "* Notes\n\n")
          (insert "* Concepts\n\n")
          (insert "* Citations\n\n")
          (insert "* Questions\n\n")
          (insert "* Future Work\n\n")))

      ;; Store ID-to-file mapping for internal use
      (doc-engine-register-file identifier filepath)

      (find-file filepath)
      (message "Created scientific document: %s" display-filename)
      filepath)))

(defun doc-engine-read-prompt (prompt)
  "Read string input with PROMPT."
  (read-string prompt))

(defun doc-engine-read-doi-prompt ()
  "Read DOI with validation."
  (let ((doi (read-string "DOI (optional, format 10.xxxx/...): ")))
    (when (and doi (not (doc-engine-validate-doi doi)))
      (error "Invalid DOI format: %s" doi))
    doi))

(provide 'doc-engine)

;;; doc-engine.el ends here