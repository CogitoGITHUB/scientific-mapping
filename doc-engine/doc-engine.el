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

(require 'seq)
(require 'xref)
(require 'dired)
(require 'org-id)
(eval-when-compile (require 'subr-x))

;; Load citation-database
(add-to-list 'load-path (expand-file-name "../citation-database" (file-name-directory load-file-name)))
(require 'citation-database)
(citation-database-autosync-mode 1)

;; Associate .orgtex with org-mode
(add-to-list 'auto-mode-alist '("\\.orgtex\\'" . org-mode))

(defgroup doc-engine ()
  "Document and paper management system."
  :group 'files
  :link '(info-link "(doc-engine) Top")
  :link '(url-link :tag "Homepage" "https://scientific-mapping.org"))

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
           (filename (doc-engine-format-filename identifier signature keywords file-type))
           (filepath (expand-file-name filename target-dir))
           (front-matter (doc-engine-format-front-matter
                          title doi keywords identifier date
                          citation abstract methodology results file-type)))
      
      (when (file-exists-p filepath)
        (error "File already exists: %s" filepath))
      
      (with-temp-file filepath
        (insert front-matter)
        (unless (eq file-type 'text)
          (insert "\n\n")
          (insert "* Notes\n\n")
          (insert "* Concepts\n\n")
          (insert "* Citations\n\n")
          (insert "* Questions\n\n")
          (insert "* Future Work\n\n")))
      
      (find-file filepath)
      (message "Created scientific document: %s" filepath)
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

;;;; Scientific Document Identifiers

(defun doc-engine-generate-identifier (&optional doi time)
  "Generate unique identifier for scientific document.
If DOI is provided, use DOI-based identifier. Otherwise use timestamp format."
  (if (and doi (doc-engine-validate-doi doi))
      (doc-engine-doi-to-identifier doi)
    (format-time-string "%Y%m%dT%H%M%S" (or time (current-time)))))

(defun doc-engine-doi-to-identifier (doi)
  "Convert DOI to filesystem-safe identifier."
  (let ((clean-doi (replace-regexp-in-string "/" "_" doi))
        (remove-prefix (replace-regexp-in-string "^[0-9]+\\.[0-9]+/" "" doi)))
    (replace-regexp-in-string "/" "_" remove-prefix)))

(defun doc-engine-validate-doi (doi)
  "Validate DOI format."
  (and (stringp doi)
       (string-match-p "10\\.[0-9]\\{4,9\\}/[^[:space:]]" doi)))

(defun doc-engine-identifier-p (identifier)
  "Check if STRING is a valid scientific document identifier."
  (string-match-p "^[0-9]\\{8\\}T[0-9]\\{6\\}$" identifier))

;;;; Scientific Document Filenames

(defun doc-engine-format-filename (identifier signature keywords file-type)
  "Format filename for scientific document.
Format: IDENTIFIER--SIGNATURE__KEYWORDS.EXTENSION"
  (let ((keyword-str (if keywords
                       (concat "__"
                               (mapconcat 'downcase keywords "-"))
                     "")))
    (format "%s--%s%s.%s"
            identifier
            (downcase signature)
            keyword-str
            (doc-engine-file-extension file-type))))

(defun doc-engine-sluggify-title (title)
  "Convert TITLE to a filesystem-safe signature."
  (let* ((downcase (downcase title))
         (remove-special (replace-regexp-in-string "[^[:alnum:] ]+" "" downcase))
         (replace-spaces (replace-regexp-in-string " +" "-" remove-special))
         (trim (replace-regexp-in-string "-+$" "" replace-spaces)))
    (replace-regexp-in-string "^-+" "" trim)))

;;;; Scientific Document Front Matter

(defun doc-engine-format-front-matter (title doi keywords identifier date
                                                    citation abstract methodology results file-type)
  "Format front matter for scientific document based on FILE-TYPE."
  (pcase file-type
    ('org (doc-engine-format-org-front-matter title doi keywords identifier
                                                          date citation abstract methodology results))
    ((or 'markdown-yaml 'markdown-toml)
     (doc-engine-format-markdown-front-matter title doi keywords identifier
                                                          date citation abstract methodology results file-type))
    ('text (doc-engine-format-text-front-matter title doi keywords identifier
                                                         date citation abstract methodology results))
    (_ (error "Unsupported file type: %s" file-type))))

(defun doc-engine-format-org-front-matter (title doi keywords identifier
                                                    date citation abstract methodology results)
  "Format Org mode front matter for scientific document."
  (concat
   "#+TITLE: " title "\n"
   "#+DATE: [" (format-time-string "%Y-%m-%d %a %H:%M" date) "]\n"
   (when keywords
     (concat "#+SCIENTIFIC_TAGS: :"
             (mapconcat 'downcase keywords ":") ":\n"))
   "#+IDENTIFIER: " identifier "\n"
   (when doi
     (concat "#+DOI: " doi "\n"))
   (when citation
     (concat "#+CITATION: " citation "\n"))
   (when abstract
     (concat "#+ABSTRACT: " abstract "\n"))
   (when methodology
     (concat "#+METHODOLOGY: " methodology "\n"))
   (when results
     (concat "#+RESULTS: " results "\n"))
   "#+STARTUP: content\n"))

(defun doc-engine-format-markdown-front-matter (title doi keywords identifier
                                                        date citation abstract methodology results file-type)
  "Format Markdown front matter for scientific document."
  (let* ((delimiter (if (eq file-type 'markdown-yaml) "---\n" "+++\n"))
         (properties (list
                    (cons "title" title)
                    (cons "date" (format-time-string "%Y-%m-%d %H:%M:%S" date))
                    (when keywords
                      (cons "keywords" (string-join keywords ", ")))
                    (cons "identifier" identifier)
                    (when doi
                      (cons "doi" doi))
                    (when citation
                      (cons "citation" citation))
                    (when abstract
                      (cons "abstract" abstract))
                    (when methodology
                      (cons "methodology" methodology))
                    (when results
                      (cons "results" results))))
         (props-string (mapconcat
                       (lambda (pair)
                         (format "%s: %s"
                                 (car pair)
                                 (doc-engine-markdown-escape (cdr pair))))
                       (remove 'nil properties)
                       "\n")))
    (concat delimiter props-string delimiter "\n\n")))

(defun doc-engine-format-text-front-matter (title doi keywords identifier
                                                       date citation abstract methodology results)
  "Format plain text front matter for scientific document."
  (concat
   "Title: " title "\n"
   "Date: " (format-time-string "%Y-%m-%d %H:%M" date) "\n"
   (when keywords
     (concat "Keywords: " (string-join keywords ", ") "\n"))
   "Identifier: " identifier "\n"
   (when doi
     (concat "DOI: " doi "\n"))
   (when citation
     (concat "Citation: " citation "\n"))
   (when abstract
     (concat "Abstract: " abstract "\n"))
   (when methodology
     (concat "Methodology: " methodology "\n"))
   (when results
     (concat "Results: " results "\n"))
   "\n"))

(defun doc-engine-markdown-escape (str)
  "Escape special characters in STR for Markdown YAML/TOML."
  (if (stringp str)
      (replace-regexp-in-string "\"" "\\\"" str)
    str))

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

(provide 'doc-engine-engine)

;;; doc-engine-engine.el ends here