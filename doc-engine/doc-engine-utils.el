;;; doc-engine-utils.el --- Utility functions and data structures -*- lexical-binding: t -*-

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

;; Utility functions, data structures, directories, and validation.

;;; Code:

(require 'doc-engine-core)

;;;; Document metadata properties

(defcustom doc-engine-concepts-property-name "SCIENTIFIC_CONCEPTS"
  "The name for org-mode property in which concept links are stored."
  :group 'doc-engine
  :type 'string)

(defcustom doc-engine-citations-property-name "SCIENTIFIC_CITATIONS"
  "The name for org-mode property in which citation links are stored."
  :group 'doc-engine
  :type 'string)

(defcustom doc-engine-doi-property-name "DOI"
  "The name for org-mode property in which DOI is stored."
  :group 'doc-engine
  :type 'string)

;;;; User options

;;###autoload (put 'doc-engine-directory 'safe-local-variable (lambda (val) (or (stringp val) (listp val) (eq val 'local) (eq val 'default-directory))))
(defcustom doc-engine-directory (expand-file-name "~/documents/")
  "Directory, as a string, for storing documents.
This is the destination `doc-engine' and all other
document-creating commands use.

The value can also be a list of directories as strings. In that case,
`doc-engine' and related commands will pick the first one
among them, unless user option `doc-engine-prompts' is configured
to prompt for a directory.

If target directory does not exist, `doc-engine-create' and related
commands will create it."
  :group 'doc-engine
  :type '(choice (directory :tag "Single directory")
                 (repeat :tag "Multiple directories" directory)))

(defcustom doc-engine-file-type 'org
  "The default file type extension for creating documents.
Supported types are `org', `markdown-yaml', `markdown-toml', and `text'."
  :group 'doc-engine
  :type '(choice (const :tag "Org mode" org)
                 (const :tag "Markdown with YAML" markdown-yaml)
                 (const :tag "Markdown with TOML" markdown-toml)
                 (const :tag "Plain text" text)))

(defcustom doc-engine-prompts '(title doi keywords subdirectory)
  "User prompts for `doc-engine-create'.
Prompt options include: `title', `doi', `keywords', `subdirectory',
`citation', `abstract', `methodology', `results'."
  :group 'doc-engine
  :type '(set (const :tag "Title prompt" title)
              (const :tag "DOI prompt" doi)
              (const :tag "Keywords prompt" keywords)
              (const :tag "Subdirectory prompt" subdirectory)
              (const :tag "Citation prompt" citation)
              (const :tag "Abstract prompt" abstract)
              (const :tag "Methodology prompt" methodology)
              (const :tag "Results prompt" results)))

;;;; Scientific Document Data Structures

(defconst doc-engine-file-type-extensions
  '((org . "org")
    (markdown-yaml . "md")
    (markdown-toml . "md")
    (text . "txt"))
  "Alist mapping file types to their corresponding file extensions.")

(defun doc-engine-file-extension (&optional file-type)
  "Return extension for FILE-TYPE.
If FILE-TYPE is nil, use `doc-engine-file-type'."
  (cdr (assq (or file-type doc-engine-file-type)
             doc-engine-file-type-extensions)))

(defconst doc-engine-file-type-hashtags
  '((org . "#")
    (markdown-yaml . "#")
    (markdown-toml . "#")
    (text . "#"))
  "Alist mapping file types to their comment characters.")

(defun doc-engine--get-hashtag (&optional file-type)
  "Return comment hashtag for FILE-TYPE."
  (cdr (assq (or file-type doc-engine-file-type)
             doc-engine-file-type-hashtags)))

;;;; Scientific Document Directories

(defun doc-engine-directories ()
  "Return list of all scientific document directories."
  (if (listp doc-engine-directory)
      doc-engine-directory
    (list doc-engine-directory)))

(defun doc-engine-all-files (&optional dirs)
  "Return all scientific document files in DIRS."
  (let ((directories (or dirs (doc-engine-directories))))
    (seq-reduce
     (lambda (acc dir)
        (seq-into
         (append acc
                 (seq-filter
                  (lambda (file)
                    (member (file-name-extension file)
                            (mapcar 'cdr doc-engine-file-type-extensions)))
                  (directory-files-recursively dir nil)))
         'list))
      directories
      '())))

;;;; Backwards Compatibility Aliases

(defvar-alias 'scientific-document-directory 'doc-engine-directory)
(defvar-alias 'scientific-document-prompts 'doc-engine-prompts)

(define-obsolete-function-alias 'scientific-document-all-files #'doc-engine-all-files "1.0")
(define-obsolete-function-alias 'scientific-document-extract-metadata #'doc-engine-extract-metadata "1.0")

(provide 'doc-engine-utils)

;;; doc-engine-utils.el ends here