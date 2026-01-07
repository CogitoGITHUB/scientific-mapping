;;; doc/manage.el --- Document management  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Document management functions.
;; - Listing documents
;; - Document metadata
;; - Document updates

;;; Code:

(defun scientific-mapping-doc-list (&optional filter)
  "List all documents. FILTER by keyword if provided."
  (let ((docs (scientific-mapping-doc--find-all)))
    (when filter
      (setq docs (cl-remove-if-not
                  (lambda (d) (string-match-p filter (alist-get 'title d)))
                  docs)))
    docs))

(defun scientific-mapping-doc--find-all ()
  "Find all documents in document directory."
  (let ((files (scientific-mapping--find-files
               scientific-mapping-doc-directory
               "\\.org$" t))
        (docs nil))
    (dolist (file files)
      (let ((title (scientific-mapping--file-title file))
            (id (scientific-mapping--file-identifier file))
            (doi (scientific-mapping--file-doi file))
            (keywords (scientific-mapping--file-keywords file)))
        (when title
          (push (list (cons 'path file)
                     (cons 'title title)
                     (cons 'id id)
                     (cons 'doi doi)
                     (cons 'keywords keywords))
                docs))))
    (nreverse docs)))

(defun scientific-mapping-doc-get (id)
  "Get document by ID."
  (let ((docs (scientific-mapping-doc-list)))
    (cl-find id docs
             :test (lambda (id doc)
                     (string= id (alist-get 'id doc))))))

(defun scientific-mapping-doc-count ()
  "Return count of all documents."
  (length (scientific-mapping-doc--find-all)))

(defun scientific-mapping-doc-recent (&optional n)
  "Return N most recent documents (default: 5)."
  (let ((docs (scientific-mapping-doc--find-all)))
    (setcdr (nthcdr (min n (length docs)) docs) nil)
    docs))

(provide 'doc/manage)
;;; doc/manage.el ends here
