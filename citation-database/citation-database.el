;;; citation-database.el --- Bibliographic database for scientific citations -*- lexical-binding: t -*-

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

;; citation-database provides an SQLite backend for managing scientific citations,
;; bibliographic metadata, and citation relationships. It serves as the data layer
;; for the scientific knowledge mapping system, enabling efficient storage, indexing,
;; and querying of academic literature and research notes.
;;
;; The database stores:
;; - Papers and documents with DOIs and identifiers
;; - Citation relationships between documents
;; - Authors, journals, and affiliations
;; - Keywords, concepts, and tags
;; - Reading status and priority information
;; - Impact factors and citation counts

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
                  ([(id integer :primary-key)
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
                    (updated-at integer)])])
    
    ;; Citations table (citation relationships)
    (emacsql db [:create-table citations
                  ([(id integer :primary-key)
                    (source-id integer :not-null)
                    (target-id integer :not-null)
                    (citation-type text)
                    (context text)
                    (created-at integer)])])
    
    ;; Authors table
    (emacsql db [:create-table authors
                  ([(id integer :primary-key)
                    (name text :not-null)
                    (affiliation text)
                    (orcid text)
                    (h-index integer)
                    (total-citations integer)])])
    
    ;; Journals table
    (emacsql db [:create-table journals
                  ([(id integer :primary-key)
                    (name text :unique :not-null)
                    (abbreviation text)
                    (impact-factor real)
                    (eissn text)
                    (issn text)
                    (publisher text)
                    (domain text)])])
    
    ;; Concepts table
    (emacsql db [:create-table concepts
                  ([(id integer :primary-key)
                    (name text :unique :not-null)
                    (definition text)
                    (domain text)
                    (origin-paper text)
                    (frequency integer)
                    (created-at integer)])])
    
    ;; Keywords table
    (emacsql db [:create-table keywords
                  ([(id integer :primary-key)
                    (keyword text :unique :not-null)
                    (category text)
                    (created-at integer)])])
    
    ;; Node-keywords relationship table
    (emacsql db [:create-table node-keywords
                  ([(node-id integer :not-null)
                    (keyword-id integer :not-null)
                    (created-at integer)]))
    
    ;; Node-concepts relationship table
    (emacsql db [:create-table node-concepts
                  ([(node-id integer :not-null)
                    (concept-id integer :not-null)
                    (created-at integer)]))
    
    ;; Node-authors relationship table
    (emacsql db [:create-table node-authors
                  ([(node-id integer :not-null)
                    (author-id integer :not-null)
                    (author-order integer)])])
    
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

;;;; Database Operations

(defun citation-database-sync (&optional file)
  "Sync citation database with FILE or all tracked files."
  (interactive)
  (citation-database--ensure-db)
  (let* ((files (if file
                     (list file)
                   (scientific-document-all-files))))
    (dolist (file files)
      (citation-database-index-file file)))
  (message "Citation database synced"))

(defun citation-database-index-file (file)
  "Index a single scientific document FILE."
  (citation-database--ensure-db)
  (let* ((metadata (scientific-document-extract-metadata file))
         (identifier (plist-get metadata :identifier))
         (title (plist-get metadata :title))
         (doi (plist-get metadata :doi))
         (file-type (file-name-extension file)))
    
    ;; Insert or update node
    (let ((existing-node (car (emacsql citation-database-db
                               [:select id :from nodes
                                :where (= identifier $s1)]
                               identifier))))
      (if existing-node
          (emacsql citation-database-db
                   [:update nodes
                    :set (= title $s1)
                        (= doi $s2)
                        (= updated-at $s3)
                    :where (= id $i4)]
                   title doi (float-time) existing-node)
        (emacsql citation-database-db
                 [:insert-into nodes
                  :values ($s1 $s2 $s3 $s4 $s5 $s6)]
                 file title doi identifier file-type (float-time))))))

(defun citation-database-clear ()
  "Clear all data from citation database."
  (interactive)
  (when (y-or-n-p "Clear all data from citation database? ")
    (citation-database--ensure-db)
    (emacsql citation-database-db [:delete :from nodes])
    (emacsql citation-database-db [:delete :from citations])
    (emacsql citation-database-db [:delete :from authors])
    (emacsql citation-database-db [:delete :from journals])
    (emacsql citation-database-db [:delete :from concepts])
    (emacsql citation-database-db [:delete :from keywords])
    (emacsql citation-database-db [:delete :from node-keywords])
    (emacsql citation-database-db [:delete :from node-concepts])
    (emacsql citation-database-db [:delete :from node-authors])
    (message "Citation database cleared")))

;;;; Citation Queries

(defun citation-database-get-paper-by-doi (doi)
  "Retrieve paper metadata by DOI."
  (citation-database--ensure-db)
  (car (emacsql citation-database-db
            [:select * :from nodes
             :where (= doi $s1)]
            doi)))

(defun citation-database-get-paper-by-identifier (identifier)
  "Retrieve paper metadata by identifier."
  (citation-database--ensure-db)
  (car (emacsql citation-database-db
            [:select * :from nodes
             :where (= identifier $s1)]
            identifier)))

(defun citation-database-search-papers (query &optional limit)
  "Search papers for QUERY string. Return up to LIMIT results."
  (citation-database--ensure-db)
  (let* ((search-query (concat "%" query "%"))
         (results (emacsql citation-database-db
                    [:select * :from nodes
                     :where (:or
                              (:like title $s1)
                              (:like abstract $s1)
                              (:like keywords $s1))
                     :order-by [(desc citation-count)]
                     :limit $i2]
                    search-query (or limit 50))))
    results))

(defun citation-database-get-citations (paper-id)
  "Get all citations from PAPER-ID to other papers."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:select [target-id citation-type context]
            :from citations
            :where (= source-id $i1)]
           paper-id))

(defun citation-database-get-cited-by (paper-id)
  "Get all papers that cite PAPER-ID."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:select [source-id citation-type context]
            :from citations
            :where (= target-id $i1)]
           paper-id))

(defun citation-database-add-citation (source-id target-id &optional citation-type context)
  "Add citation from SOURCE-ID to TARGET-ID with optional TYPE and CONTEXT."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:insert-into citations
            :values ($null $i1 $i2 $s3 $s4 $i5)]
           source-id target-id citation-type context (float-time)))

;;;; Author Management

(defun citation-database-add-author (name &optional affiliation orcid)
  "Add author with NAME, optional AFFILIATION and ORCID."
  (citation-database--ensure-db)
  (let ((existing-author (car (emacsql citation-database-db
                                  [:select id :from authors
                                   :where (= name $s1)]
                                  name))))
    (if existing-author
        existing-author
      (let ((author-id (car (emacsql citation-database-db
                               [:insert-into authors
                                :values ($null $s1 $s2 $s3 0 0)]
                               name affiliation orcid))))
        author-id))))

(defun citation-database-link-author-to-paper (author-id paper-id &optional author-order)
  "Link AUTHOR-ID to PAPER-ID with optional AUTHOR-ORDER."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:insert-into node-authors
            :values ($i1 $i2 $i3)]
           paper-id author-id (or author-order 0)))

(defun citation-database-get-papers-by-author (author-id &optional limit)
  "Get papers by AUTHOR-ID, limited to LIMIT results."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:select [nodes.*]
            :from nodes
            :inner-join node-authors :on (= nodes.id node-authors.node-id)
            :where (= node-authors.author-id $i1)
            :order-by [(desc nodes.year)]
            :limit $i2]
           author-id (or limit 50)))

;;;; Journal Management

(defun citation-database-add-journal (name &optional abbreviation impact-factor)
  "Add journal with NAME, optional ABBREVIATION and IMPACT-FACTOR."
  (citation-database--ensure-db)
  (let ((existing-journal (car (emacsql citation-database-db
                                  [:select id :from journals
                                   :where (= name $s1)]
                                  name))))
    (if existing-journal
        existing-journal
      (let ((journal-id (car (emacsql citation-database-db
                                [:insert-into journals
                                 :values ($null $s1 $s2 $f3)]
                                 name abbreviation impact-factor))))
        journal-id))))

(defun citation-database-get-papers-by-journal (journal-name &optional limit)
  "Get papers published in JOURNAL-NAME, limited to LIMIT results."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:select * :from nodes
            :where (= journal $s1)
            :order-by [(desc year)]
            :limit $i2]
           journal-name (or limit 50)))

;;;; Concept Management

(defun citation-database-add-concept (name &optional definition domain)
  "Add concept with NAME, optional DEFINITION and DOMAIN."
  (citation-database--ensure-db)
  (let ((existing-concept (car (emacsql citation-database-db
                                   [:select id :from concepts
                                    :where (= name $s1)]
                                   name))))
    (if existing-concept
        (progn
          ;; Update frequency
          (emacsql citation-database-db
                   [:update concepts
                    :set (= frequency (+ frequency 1))
                    :where (= id $i1)]
                   existing-concept)
          existing-concept)
      (let ((concept-id (car (emacsql citation-database-db
                                [:insert-into concepts
                                 :values ($null $s1 $s2 $s3 0 $i4)]
                                 name definition domain (float-time)))))
        concept-id))))

(defun citation-database-link-concept-to-paper (concept-id paper-id)
  "Link CONCEPT-ID to PAPER-ID."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:insert-into node-concepts
            :values ($i1 $i2 $i3)]
           paper-id concept-id (float-time)))

(defun citation-database-search-concepts (query &optional limit)
  "Search concepts for QUERY string. Return up to LIMIT results."
  (citation-database--ensure-db)
  (let* ((search-query (concat "%" query "%"))
         (results (emacsql citation-database-db
                    [:select * :from concepts
                     :where (:or
                              (:like name $s1)
                              (:like definition $s1))
                     :order-by [(desc frequency)]
                     :limit $i2]
                    search-query (or limit 50))))
    results))

;;;; Keyword Management

(defun citation-database-add-keyword (keyword &optional category)
  "Add keyword with optional CATEGORY."
  (citation-database--ensure-db)
  (let ((existing-keyword (car (emacsql citation-database-db
                                   [:select id :from keywords
                                    :where (= keyword $s1)]
                                   keyword))))
    (if existing-keyword
        existing-keyword
      (let ((keyword-id (car (emacsql citation-database-db
                                [:insert-into keywords
                                 :values ($null $s1 $s2 $i3)]
                                 keyword category (float-time)))))
        keyword-id))))

(defun citation-database-link-keyword-to-paper (keyword-id paper-id)
  "Link KEYWORD-ID to PAPER-ID."
  (citation-database--ensure-db)
  (emacsql citation-database-db
           [:insert-into node-keywords
            :values ($i1 $i2 $i3)]
           paper-id keyword-id (float-time)))

;;;; Citation Network Analysis

(defun citation-database-citation-network (depth &optional max-nodes)
  "Build citation network starting from all nodes, up to DEPTH levels.
Limit to MAX-NODES results."
  (citation-database--ensure-db)
  (let* ((nodes (emacsql citation-database-db
                   [:select [id identifier title doi]
                    :from nodes
                    :order-by [(desc citation-count)]
                    :limit $i1]
                   (or max-nodes 500)))
         (edges '()))
    
    ;; Collect edges
    (dolist (node nodes)
      (let ((node-id (elt node 0))
            (citations (citation-database-get-citations node-id)))
        (dolist (citation citations)
          (push (list node-id (elt citation 0)) edges))))
    
    (list :nodes nodes :edges edges)))

(defun citation-database-co-citation-analysis (paper-ids &optional threshold)
  "Find papers frequently cited together with PAPER-IDS.
Return pairs with co-citation count above THRESHOLD."
  (citation-database--ensure-db)
  (let* ((paper-list (mapconcat
                     (lambda (id) (format "%s" id))
                     paper-ids
                     ","))
         (query (format "
SELECT t1.source_id, t2.source_id, COUNT(*) as co_citation_count
FROM citations t1
INNER JOIN citations t2 ON t1.target_id = t2.target_id
WHERE t1.source_id IN (%s) AND t2.source_id IN (%s)
  AND t1.source_id < t2.source_id
GROUP BY t1.source_id, t2.source_id
HAVING co_citation_count >= %d
ORDER BY co_citation_count DESC" paper-list paper-list (or threshold 2)))
         (results (emacsql citation-database-db query)))
    results))

;;;; Statistics and Reports

(defun citation-database-stats ()
  "Generate citation database statistics."
  (interactive)
  (citation-database--ensure-db)
  (let* ((total-papers (caar (emacsql citation-database-db
                                 [:select (funcall count *) :from nodes])))
         (total-citations (caar (emacsql citation-database-db
                                   [:select (funcall sum citation-count)
                                    :from nodes])))
         (total-authors (caar (emacsql citation-database-db
                                 [:select (funcall count *) :from authors])))
         (total-journals (caar (emacsql citation-database-db
                                   [:select (funcall count *) :from journals])))
         (total-concepts (caar (emacsql citation-database-db
                                  [:select (funcall count *) :from concepts])))
    
    (message "Citation Database Statistics:")
    (message "  Total papers: %d" total-papers)
    (message "  Total citations: %d" total-citations)
    (message "  Total authors: %d" total-authors)
    (message "  Total journals: %d" total-journals)
    (message "  Total concepts: %d" total-concepts)
    
    (list
     :total-papers total-papers
     :total-citations total-citations
     :total-authors total-authors
     :total-journals total-journals
     :total-concepts total-concepts)))

;;;; Autosync Mode

(define-minor-mode citation-database-autosync-mode
  "Auto-sync citation database when files are saved."
  :lighter " CitationDB"
  :global t
  (if citation-database-autosync-mode
      (add-hook 'after-save-hook #'citation-database--autosync-handler)
    (remove-hook 'after-save-hook #'citation-database--autosync-handler)))

(defun citation-database--autosync-handler ()
  "Handle autosync on file save."
  (when (and citation-database-autosync
             (buffer-file-name)
             (member (file-name-extension (buffer-file-name))
                     '("org" "md" "txt")))
    (citation-database-index-file (buffer-file-name))))

(provide 'citation-database)

;;; citation-database.el ends here