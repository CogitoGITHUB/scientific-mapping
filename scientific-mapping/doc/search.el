;;; doc/search.el --- Document search  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Document search functions.
;; - Full-text search
;; - Metadata search
;; - Filtered listings

;;; Code:

(defun scientific-mapping-doc-search (query)
  "Search documents containing QUERY."
  (interactive "sSearch: ")
  (let ((results nil))
    (dolist (doc (scientific-mapping-doc--find-all))
      (let* ((path (alist-get 'path doc))
             (content (scientific-mapping--read-file path))
             (title (alist-get 'title doc)))
        (when (or (string-match-p query content)
                  (string-match-p query title))
          (push (cons title path) results))))
    (nreverse results)))

(defun scientific-mapping-doc-search-keywords (keywords)
  "Search documents matching any of KEYWORDS (list)."
  (let ((results nil))
    (dolist (doc (scientific-mapping-doc--find-all))
      (let ((doc-keywords (alist-get 'keywords doc)))
        (when (cl-some (lambda (k) (member k doc-keywords)) keywords)
          (push doc results))))
    (nreverse results)))

(defun scientific-mapping-doc-search-doi (doi)
  "Search for document by DOI."
  (cl-find doi (scientific-mapping-doc--find-all)
           :test (lambda (doi doc)
                   (string= doi (alist-get 'doi doc)))))

(defun scientific-mapping-doc-search-title (title)
  "Search for document by title substring."
  (cl-remove-if-not
   (lambda (doc) (string-match-p title (alist-get 'title doc)))
   (scientific-mapping-doc--find-all)))

(provide 'doc/search)
;;; doc/search.el ends here
