;;; citation-database-core.el --- Core database setup for citation management -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/citation-database
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (emacsql "3.0.0"))

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

;; Core database configuration and schema setup.

;;; Code:

(require 'emacsql)
(require 'emacsql-sqlite)
(require 'seq)
(eval-when-compile (require 'subr-x))

(defgroup citation-database ()
  "Bibliographic database for scientific citations."
  :group 'files
  :link '(info-link "(citation-database) Top")
  :link '(url-link :tag "Homepage" "https://scientific-mapping.org"))

;;;; Database Configuration

(defcustom citation-database-location
  (expand-file-name "citation-db.sqlite" user-emacs-directory)
  "Location of the citation database file."
  :group 'citation-database
  :type 'file)

(defcustom citation-database-autosync t
  "If non-nil, automatically sync database when files are saved."
  :group 'citation-database
  :type 'boolean)

;;;; Database Schema

(defvar citation-database-db nil
  "The SQLite database connection object.")

(defvar citation-database-schema-version 1
  "The current schema version of citation-database.")

(defun citation-database--ensure-db ()
  "Ensure the citation database is initialized and connected."
  (unless (and citation-database-db
               (emacsql-live-p citation-database-db))
    (setq citation-database-db
          (emacsql-sqlite citation-database-location))
    (citation-database--init-schema)))

(defun citation-database--init-schema ()
  "Initialize the database schema for citation-database."
  (let ((db citation-database-db))

    ;; Nodes table (documents, papers, notes)
    (emacsql db [:create-table nodes
                  ((id integer :primary-key)
                    (file text :unique)
                    (title text)
                    (doi text)
                    (authors text)
                    (journal text)
                    (year integer)
                    (volume text)
                    (issue text)
                    (pages text)
                    (abstract text)
                    (keywords text)
                    (filetype text)
                    (identifier text)
                    (reading-status text)
                    (priority text)
                    (relevance integer)
                    (citation-count integer)
                    (impact-factor real)
                    (created-at integer)
                    (updated-at integer))) ]

    ;; Citations table (citation relationships)
    (emacsql db [:create-table citations
                  ((id integer :primary-key)
                    (source-id integer :not-null)
                    (target-id integer :not-null)
                    (citation-type text)
                    (context text)
                    (created-at integer))) ]

    ;; Authors table
    (emacsql db [:create-table authors
                  ((id integer :primary-key)
                    (name text :not-null)
                    (affiliation text)
                    (orcid text)
                    (h-index integer)
                    (total-citations integer))) ]

    ;; Journals table
    (emacsql db [:create-table journals
                  ((id integer :primary-key)
                    (name text :unique :not-null)
                    (abbreviation text)
                    (impact-factor real)
                    (eissn text)
                    (issn text)
                    (publisher text)
                    (domain text))) ]

     ;; Concepts table
     (emacsql db [:create-table concepts
                   ((id integer :primary-key)
                     (name text :unique :not-null)
                     (definition text)
                     (domain text)
                     (origin-paper text)
                     (frequency integer)
                     (created-at integer))) ]

     ;; Keywords table
     (emacsql db [:create-table keywords
                   ((id integer :primary-key)
                     (keyword text :unique :not-null)
                     (category text)
                     (created-at integer))) ]

     ;; Node-keywords relationship table
     (emacsql db [:create-table node-keywords
                   ((node-id integer :not-null)
                     (keyword-id integer :not-null)
                     (created-at integer))) ]

     ;; Node-concepts relationship table
     (emacsql db [:create-table node-concepts
                   ((node-id integer :not-null)
                     (concept-id integer :not-null)
                     (created-at integer))) ]

     ;; Node-authors relationship table
     (emacsql db [:create-table node-authors
                   ((node-id integer :not-null)
                     (author-id integer :not-null)
                     (author-order integer))) ]

    ;; Create indexes for performance
    (emacsql db [:create-index nodes-identifier :on nodes :columns [identifier]])
    (emacsql db [:create-index nodes-doi :on nodes :columns [doi]])
    (emacsql db [:create-index nodes-year :on nodes :columns [year]])
    (emacsql db [:create-index citations-source :on citations :columns [source-id]])
    (emacsql db [:create-index citations-target :on citations :columns [target-id]])
    (emacsql db [:create-index authors-name :on authors :columns [name]])
    (emacsql db [:create-index journals-name :on journals :columns [name]])
    (emacsql db [:create-index concepts-name :on concepts :columns [name]])
    (emacsql db [:create-index keywords-keyword :on keywords :columns [keyword]])))

(provide 'citation-database-core)

;;; citation-database-core.el ends here