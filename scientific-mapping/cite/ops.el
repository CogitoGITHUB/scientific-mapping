;;; cite/ops.el --- Citation database operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Citation database operations.
;; - Add citations
;; - Remove citations
;; - Update citations

;;; Code:

(defun scientific-mapping-cite-add (citation)
  "Add CITATION to database.
CITATION is a plist with :title, :authors, :journal, :year, :doi, :keywords, :abstract."
  (scientific-mapping-cite--ensure-db)
  (let* ((title (plist-get citation :title))
         (authors (plist-get citation :authors))
         (journal (plist-get citation :journal))
         (year (plist-get citation :year))
         (doi (plist-get citation :doi))
         (keywords (plist-get citation :keywords))
         (abstract (plist-get citation :abstract)))
    (emacsql scientific-mapping-cite--db
             [:insert-into citations
              :values ($s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8)]
             title authors journal year doi
             (when keywords (mapconcat 'identity keywords ", "))
             abstract
             (float-time))))

(defun scientific-mapping-cite-remove (doi)
  "Remove citation with DOI from database."
  (scientific-mapping-cite--ensure-db)
  (emacsql scientific-mapping-cite--db
           [:delete :from citations :where (= doi $s1)]
           doi))

(defun scientific-mapping-cite-update (doi citation)
  "Update citation with DOI using CITATION data."
  (scientific-mapping-cite--ensure-db)
  (let* ((title (plist-get citation :title))
         (authors (plist-get citation :authors))
         (journal (plist-get citation :journal))
         (year (plist-get citation :year))
         (keywords (plist-get citation :keywords))
         (abstract (plist-get citation :abstract)))
    (emacsql scientific-mapping-cite--db
             [:update citations
              :set (= title $s1) (= authors $s2) (= journal $s3)
                   (= year $s4) (= keywords $s5) (= abstract $s6)
              :where (= doi $s7)]
             title authors journal year
             (when keywords (mapconcat 'identity keywords ", "))
             abstract doi)))

(provide 'cite/ops)
;;; cite/ops.el ends here
