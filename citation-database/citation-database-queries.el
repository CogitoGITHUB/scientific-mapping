;;; citation-database-queries.el --- Database queries and operations -*- lexical-binding: t -*-

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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Database operations, queries, and citation network analysis.

;;; Code:

(require 'citation-database-core)

;;;; Database Operations

(defun citation-database-sync (&optional file)
  "Sync citation database with FILE or all tracked files."
  (interactive)
  (citation-database--ensure-db)
  (let* ((files (if file
                    (list file)
                  (doc-engine-all-files))))
    (dolist (file files)
      (citation-database-index-file file)))
  (message "Citation database synced"))

(defun citation-database-index-file (file)
  "Index a single FILE from doc-engine."
  (citation-database--ensure-db)
  (let* ((metadata (doc-engine-extract-metadata file))
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

(provide 'citation-database-queries)

;;; citation-database-queries.el ends here