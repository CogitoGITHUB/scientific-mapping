;;; concept-relationships.el --- Concept and relationship management for scientific knowledge -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/concept-relationships
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (org "9.2"))

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

;; concept-relationships implements a variant of concept mapping for scientific
;; research. It is heavily inspired by scientific knowledge representation
;; methods, where each node is an org-mode file or headline, and
;; relationships represent theoretical, methodological, or evidence connections.
;;
;; Entries can be linked together, and you can then view the network of
;; relationships as a concept map, using `M-x concept-relationships-visualize'.
;;
;; Relationship types supported:
;; - Parent-Child: Hierarchical knowledge organization (generalization-specialization)
;; - Sibling: Related concepts at same abstraction level
;; - Friend: Cross-disciplinary connections
;; - Evidence: Empirical evidence supporting or refuting claims
;; - Method: Methodological connections between studies
;; - Evolution: Historical evolution of concepts

;;; Code:

(require 'org-element)
(require 'org-attach)
(require 'org-agenda)
(require 'org-macs)
(require 'org-id)
(require 'picture)
(require 'subr-x)
(require 'seq)

(defgroup concept-relationships ()
  "Concept and relationship management for scientific knowledge."
  :prefix "concept-relationships-"
  :group 'org)

;;;; Custom Vars

(defcustom concept-relationships-path (file-truename (expand-file-name "concepts" scientific-document-directory))
  "The root directory of your concept relationships.

`scientific-document' files placed in this directory, or its subdirectories,
will be considered concept nodes."
  :group 'concept-relationships
  :type '(directory))

(defcustom concept-relationships-scan-directories-recursively t
  "If subdirectories inside `concept-relationships-path' are considered part of concepts."
  :group 'concept-relationships
  :type '(boolean))

(defcustom concept-relationships-ignored-resource-links '("fuzzy" "radio" "concept-child" "concept-parent" "concept-friend" "concept-evidence")
  "`org-link-types' which shouldn't be shown as resources in `concept-relationships-visualize'."
  :group 'concept-relationships
  :type '(repeat string))

(defcustom concept-relationships-backlink nil
  "If backlink resource should be added when creating a concept link."
  :group 'concept-relationships
  :type '(restricted-sexp :match-alternatives
           (stringp 't 'nil)))

(defcustom concept-relationships-data-file (file-truename (expand-file-name ".concept-relationships-data.el" concept-relationships-path))
  "Where concept-relationships data is saved."
  :group 'concept-relationships
  :type 'file)

;;;; Relationship Properties

(defcustom concept-relationships-parents-property-name "CONCEPT_PARENTS"
  "The name for org-mode property in which parent relationships are stored."
  :group 'concept-relationships
  :type 'string)

(defcustom concept-relationships-children-property-name "CONCEPT_CHILDREN"
  "The name for org-mode property in which child relationships are stored."
  :group 'concept-relationships
  :type 'string)

(defcustom concept-relationships-siblings-property-name "CONCEPT_SIBLINGS"
  "The name for org-mode property in which sibling relationships are stored."
  :group 'concept-relationships
  :type 'string)

(defcustom concept-relationships-friends-property-name "CONCEPT_FRIENDS"
  "The name for org-mode property in which friend relationships are stored."
  :group 'concept-relationships
  :type 'string)

(defcustom concept-relationships-evidence-property-name "CONCEPT_EVIDENCE"
  "The name for org-mode property in which evidence relationships are stored."
  :group 'concept-relationships
  :type 'string)

(defcustom concept-relationships-methods-property-name "CONCEPT_METHODS"
  "The name for org-mode property in which method relationships are stored."
  :group 'concept-relationships
  :type 'string)

(defcustom concept-relationships-evolution-property-name "CONCEPT_EVOLUTION"
  "The name for org-mode property in which evolution relationships are stored."
  :group 'concept-relationships
  :type 'string)

;;;; Concept Entry Types

(defcustom concept-relationships-include-file-entries t
  "If non-nil, include .org files as concept nodes."
  :group 'concept-relationships
  :type 'boolean)

(defcustom concept-relationships-scan-for-header-entries t
  "If non-nil, scan for headline entries in addition to file entries."
  :group 'concept-relationships
  :type 'boolean)

;;;; Visualization Options

(defcustom concept-relationships-visualize-default-choices 'all
  "Default entries to show in `concept-relationships-visualize'."
  :group 'concept-relationships
  :type '(radio (const :tag "All entries" all)
                  (const :tag "Files only" files)
                  (const :tag "Files in root only" root-files)))

(defcustom concept-relationships-show-resources t
  "If non-nil, show resources in `concept-relationships-visualize'."
  :group 'concept-relationships
  :type 'boolean)

(defcustom concept-relationships-show-text t
  "If non-nil, show entry text in `concept-relationships-visualize'."
  :group 'concept-relationships
  :type 'boolean)

(defcustom concept-relationships-title-max-length 50
  "Maximum length to display of entry titles."
  :group 'concept-relationships
  :type 'integer)

;;;; Internal Variables

(defvar concept-relationships-data nil
  "Hash table storing concept relationship data.")

(defvar concept-relationships-files-cache nil
  "Cache for concept files.")

(defvar concept-relationships-headline-cache nil
  "Cache for concept headlines.")

(defvar concept-relationships-vis-current-entry nil
  "Currently visualized entry.")

;;;; Core Functions

(defun concept-relationships-ensure-data ()
  "Ensure concept relationships data is loaded."
  (unless concept-relationships-data
    (concept-relationships-load-data)))

(defun concept-relationships-load-data ()
  "Load concept relationships data from file."
  (if (file-exists-p concept-relationships-data-file)
      (with-temp-buffer
        (insert-file-contents concept-relationships-data-file)
        (setq concept-relationships-data (read (current-buffer))))
    (setq concept-relationships-data (make-hash-table :test 'equal))))

(defun concept-relationships-save-data ()
  "Save concept relationships data to file."
  (with-temp-file concept-relationships-data-file
    (insert ";;; concept-relationships-data.el --- Concept relationship data -*- coding: utf-8 -*-\n\n")
    (insert "(setq concept-relationships-data\n")
    (insert (prin1-to-string concept-relationships-data))
    (insert ")\n"))
  (message "Concept relationships data saved"))

(defun concept-relationships-refresh-cache ()
  "Refresh caches for concept files and headlines."
  (setq concept-relationships-files-cache nil)
  (setq concept-relationships-headline-cache nil)
  (concept-relationships--index-entries))

(defun concept-relationships--index-entries ()
  "Index all concept entries (files and headlines)."
  (let ((files (concept-relationships-files))
        (headlines (concept-relationships-headlines)))
    (setq concept-relationships-files-cache files)
    (setq concept-relationships-headline-cache headlines)))

(defun concept-relationships-files ()
  "Return list of all concept file entries."
  (unless concept-relationships-files-cache
    (setq concept-relationships-files-cache
          (if concept-relationships-scan-directories-recursively
              (directory-files-recursively concept-relationships-paths nil
                                   (lambda (f)
                                     (and (string-match-p "\\.org$" f)
                                          (not (string-match-p "/\\." f)))))
            (seq-filter
             (lambda (f)
               (string-match-p "\\.org$" f))
             (directory-files concept-relationships-path)))))
  concept-relationships-files-cache)

(defun concept-relationships-headlines ()
  "Return list of all concept headline entries."
  (unless concept-relationships-headline-cache
    (setq concept-relationships-headline-cache
          (when concept-relationships-scan-for-header-entries
            (seq-mapcat
             (lambda (file)
               (concept-relationships--headlines-in-file file))
             (concept-relationships-files)))))
  concept-relationships-headline-cache)

(defun concept-relationships--headlines-in-file (file)
  "Extract headlines with IDs from FILE."
  (with-current-buffer (find-file-noselect file)
    (org-element-map-map 'headline
      (lambda (headline)
        (when (org-element-property :ID headline)
          (list
           :file file
           :headline (org-element-property :raw-value headline)
           :id (org-element-property :ID headline)
           :level (org-element-property :level headline)
           :pos (org-element-property :begin headline)))))))

;;;; Entry Management

(defun concept-relationships-create-entry (title &optional properties)
  "Create a new concept entry with TITLE and PROPERTIES."
  (interactive "sConcept title: ")
  (let* ((identifier (org-id-uuid))
         (filename (concat (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" title))
                         ".org"))
         (filepath (expand-file-name filename concept-relationships-path))
         (org-content (format "* %s
:PROPERTIES:
:ID: %s
:CREATED: [%s]
:END:\n\n"
                              title
                              identifier
                              (format-time-string "%Y-%m-%d %a %H:%M"))))
    (when (file-exists-p filepath)
      (error "Concept entry already exists: %s" filepath))
    (with-temp-file filepath
      (insert org-content))
    (find-file filepath)
    identifier))

(defun concept-relationships-get-entry (&optional id)
  "Get concept entry by ID or use current buffer entry."
  (let ((entry-id (or id
                       (when (buffer-file-name)
                         (org-id-get-create)))))
    (when entry-id
      (or (seq-find
            (lambda (e)
              (string= (plist-get e :id) entry-id))
            (append (concept-relationships-files)
                    (concept-relationships-headlines)))
          (error "Concept entry not found: %s" entry-id)))))

;;;; Relationship Management

(defun concept-relationships-add-relationship (source target type &optional properties)
  "Add relationship of TYPE from SOURCE to TARGET with PROPERTIES."
  (let* ((source-entry (concept-relationships-get-entry source))
         (target-id (if (stringp target)
                        target
                      (plist-get (concept-relationships-get-entry target) :id)))
         (property-name (concept-relationships--type-to-property type)))
    (when (and source-entry target-id)
      (with-current-buffer (find-file-noselect (plist-get source-entry :file))
        (let ((existing-relationships (org-entry-get (point-min) property-name)))
          (when (or (not existing-relationships)
                    (not (string-match-p (regexp-quote target-id) existing-relationships)))
            (org-entry-put (point-min) property-name
                            (concat (or existing-relationships "")
                                    (unless existing-relationships "")
                                    target-id " "))
            (message "Added %s relationship: %s → %s" type source target))))
      (concept-relationships-save-data))))

(defun concept-relationships--type-to-property (type)
  "Convert relationship TYPE to property name."
  (pcase type
    ('parent concept-relationships-parents-property-name)
    ('child concept-relationships-children-property-name)
    ('sibling concept-relationships-siblings-property-name)
    ('friend concept-relationships-friends-property-name)
    ('evidence concept-relationships-evidence-property-name)
    ('method concept-relationships-methods-property-name)
    ('evolution concept-relationships-evolution-property-name)
    (_ (error "Unknown relationship type: %s" type))))

(defun concept-relationships-remove-relationship (source target type)
  "Remove relationship of TYPE from SOURCE to TARGET."
  (let* ((source-entry (concept-relationships-get-entry source))
         (target-id (if (stringp target)
                        target
                      (plist-get (concept-relationships-get-entry target) :id)))
         (property-name (concept-relationships--type-to-property type)))
    (when (and source-entry target-id)
      (with-current-buffer (find-file-noselect (plist-get source-entry :file))
        (let* ((existing-relationships (org-entry-get (point-min) property-name))
                (new-relationships (replace-regexp-in-string
                                   (format "\\s*%s\\s*" target-id)
                                   ""
                                   existing-relationships)))
          (org-entry-put (point-min) property-name
                          (string-trim new-relationships))
          (message "Removed %s relationship: %s → %s" type source target))))))

;;;; Interactive Commands

;;;###autoload
(defun concept-relationships-visualize (&optional entry)
  "Visualize concept ENTRY and its relationships."
  (interactive)
  (let* ((target-entry (or entry
                         (when (buffer-file-name)
                           (concept-relationships--current-entry))
                         (concept-relationships-read-entry "Visualize concept: ")))
         (vis-buffer (get-buffer-create "*Concept Relationships*")))
    (switch-to-buffer vis-buffer)
    (setq-local concept-relationships-vis-current-entry target-entry)
    (concept-relationships-mode)
    (concept-relationships--update-visualization)))

(defun concept-relationships--current-entry ()
  "Get concept entry from current buffer."
  (when (and (buffer-file-name)
               (member (file-name-extension (buffer-file-name)) '("org")))
    (let ((id (org-id-get)))
      (when id
        (concept-relationships-get-entry id)))))

(defun concept-relationships-read-entry (prompt)
  "Read and return a concept entry."
  (let* ((all-entries (append (concept-relationships-files)
                               (concept-relationships-headlines)))
         (entry-names (mapcar
                      (lambda (e)
                        (or (plist-get e :headline)
                              (file-name-base (plist-get e :file))))
                      all-entries))
         (selection (completing-read prompt entry-names)))
    (seq-find
     (lambda (e)
       (string= (or (plist-get e :headline)
                     (file-name-base (plist-get e :file)))
                selection))
     all-entries)))

;;;###autoload
(defun concept-relationships-add-child (&optional entry)
  "Add child relationship to ENTRY."
  (interactive)
  (let* ((source (or entry
                     (concept-relationships--current-entry)))
         (source-id (plist-get source :id))
         (target (concept-relationships-read-entry "Add child to %s: "
                                            (or (plist-get source :headline)
                                                  (file-name-base (plist-get source :file)))))
         (target-id (plist-get target :id)))
    (concept-relationships-add-relationship source-id target-id 'child)
    (concept-relationships-add-relationship target-id source-id 'parent)
    (when concept-relationships-vis-current-entry
      (concept-relationships--update-visualization))))

;;;###autoload
(defun concept-relationships-add-parent (&optional entry)
  "Add parent relationship to ENTRY."
  (interactive)
  (let* ((source (or entry
                     (concept-relationships--current-entry)))
         (source-id (plist-get source :id))
         (target (concept-relationships-read-entry "Add parent to %s: "
                                            (or (plist-get source :headline)
                                                  (file-name-base (plist-get source :file)))))
         (target-id (plist-get target :id)))
    (concept-relationships-add-relationship source-id target-id 'parent)
    (concept-relationships-add-relationship target-id source-id 'child)
    (when concept-relationships-vis-current-entry
      (concept-relationships--update-visualization))))

;;;###autoload
(defun concept-relationships-add-friendship (&optional entry)
  "Add friend (cross-disciplinary) relationship to ENTRY."
  (interactive)
  (let* ((source (or entry
                     (concept-relationships--current-entry)))
         (source-id (plist-get source :id))
         (target (concept-relationships-read-entry "Add friend to %s: "
                                            (or (plist-get source :headline)
                                                  (file-name-base (plist-get source :file)))))
         (target-id (plist-get target :id)))
    (concept-relationships-add-relationship source-id target-id 'friend)
    (concept-relationships-add-relationship target-id source-id 'friend)
    (when concept-relationships-vis-current-entry
      (concept-relationships--update-visualization))))

;;;###autoload
(defun concept-relationships-add-evidence (&optional entry)
  "Add evidence relationship to ENTRY."
  (interactive)
  (let* ((source (or entry
                     (concept-relationships--current-entry)))
         (source-id (plist-get source :id))
         (target (concept-relationships-read-entry "Add evidence to %s: "
                                            (or (plist-get source :headline)
                                                  (file-name-base (plist-get source :file)))))
         (target-id (plist-get target :id)))
    (concept-relationships-add-relationship source-id target-id 'evidence)
    (when concept-relationships-vis-current-entry
      (concept-relationships--update-visualization))))

;;;; Visualization Mode

(defvar concept-relationships-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'concept-relationships-add-child)
    (define-key map "p" 'concept-relationships-add-parent)
    (define-key map "f" 'concept-relationships-add-friendship)
    (define-key map "e" 'concept-relationships-add-evidence)
    (define-key map "v" 'concept-relationships-visualize)
    (define-key map "g" 'concept-relationships--update-visualization)
    map)
  "Keymap for `concept-relationships-mode'.")

(define-derived-mode concept-relationships-mode special-mode "Concept Relationships"
  "Major mode for visualizing concept relationships."
  :group 'concept-relationships
  (setq header-line-format "Concept Relationships: Press 'c' to add child, 'p' for parent, 'f' for friend, 'e' for evidence, 'v' to visualize")
  (use-local-map concept-relationships-mode-map))

(defun concept-relationships--update-visualization ()
  "Update the concept relationships visualization."
  (unless concept-relationships-vis-current-entry
    (error "No current concept to visualize"))
  (let* ((entry concept-relationships-vis-current-entry)
         (title (or (plist-get entry :headline)
                     (file-name-base (plist-get entry :file)))
         (parents (concept-relationships--get-relations entry 'parent))
         (children (concept-relationships--get-relations entry 'child))
         (siblings (concept-relationships--get-relations entry 'sibling))
         (friends (concept-relationships--get-relations entry 'friend))
         (evidence (concept-relationships--get-relations entry 'evidence)))
    (erase-buffer)
    (insert "=== " title " ===\n\n")
    (concept-relationships--draw-section "Parents" parents)
    (concept-relationships--draw-section "Children" children)
    (concept-relationships--draw-section "Siblings" siblings)
    (concept-relationships--draw-section "Friends (Cross-disciplinary)" friends)
    (concept-relationships--draw-section "Evidence" evidence)
    (goto-char (point-min))))

(defun concept-relationships--get-relations (entry type)
  "Get relations of TYPE for ENTRY."
  (let* ((id (plist-get entry :id))
         (property-name (concept-relationships--type-to-property type))
         (relations-string (with-current-buffer (find-file-noselect (plist-get entry :file))
                               (org-entry-get (point-min) property-name)))
         (relation-ids (when relations-string
                          (split-string relations-string)))
         (relations '()))
    (dolist (rel-id relation-ids)
      (let ((rel-entry (concept-relationships-get-entry rel-id)))
        (when rel-entry
          (push rel-entry relations))))
    (nreverse relations)))

(defun concept-relationships--draw-section (title entries)
  "Draw visualization section with TITLE and ENTRIES."
  (insert "\n--- " title " ---\n")
  (if (null entries)
      (insert "  (none)\n")
    (dolist (entry entries)
      (let* ((name (or (plist-get entry :headline)
                         (file-name-base (plist-get entry :file))))
        (insert "  * " name "\n")))))

(provide 'concept-relationships)

;;; concept-relationships.el ends here