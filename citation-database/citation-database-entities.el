;;; citation-database-entities.el --- Entity management for citation database -*- lexical-binding: t -*-

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

;; Management of authors, journals, concepts, and keywords.

;;; Code:

(require 'citation-database-core)

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

(provide 'citation-database-entities)

;;; citation-database-entities.el ends here