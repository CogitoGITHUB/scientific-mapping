;;; cite/queries.el --- Citation database queries  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Citation database query functions.
;; - Search citations
;; - Get by DOI
;; - Statistics

;;; Code:

(defun scientific-mapping-cite-get (doi)
  "Get citation by DOI."
  (scientific-mapping-cite--ensure-db)
  (car (emacsql scientific-mapping-cite--db
                [:select * :from citations :where (= doi $s1)]
                doi)))

(defun scientific-mapping-cite-search (query &optional limit)
  "Search citations containing QUERY. LIMIT results."
  (scientific-mapping-cite--ensure-db)
  (let* ((pattern (format "%%%s%%" query))
         (results (emacsql scientific-mapping-cite--db
                           [:select [*] :from citations
                            :where (or (like title $s1)
                                      (like authors $s2)
                                      (like journal $s3))]
                           pattern pattern pattern)))
    (if limit
        (cl-subseq results 0 (min limit (length results)))
      results)))

(defun scientific-mapping-cite-count ()
  "Return total citation count."
  (scientific-mapping-cite--ensure-db)
  (car (emacsql scientific-mapping-cite--db
                [:select (funcall count *) :from citations])))

(defun scientific-mapping-cite-by-year (year)
  "Get citations from YEAR."
  (scientific-mapping-cite--ensure-db)
  (emacsql scientific-mapping-cite--db
           [:select [*] :from citations :where (= year $s1)]
           year))

(defun scientific-mapping-cite-recent (&optional n)
  "Get N most recent citations."
  (scientific-mapping-cite--ensure-db)
  (let ((results (emacsql scientific-mapping-cite--db
                          [:select [*] :from citations
                           :order-by (:desc added-at)
                           :limit $s1]
                          (or n 10))))
    results))

(provide 'cite/queries)
;;; cite/queries.el ends here
