;;; cite/db.el --- Citation database setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Citation database setup and schema.
;; Requires emacsql package.

;;; Code:

(require 'emacsql nil t)
(require 'emacsql-sqlite nil t)

(defgroup scientific-mapping-cite nil
  "Citation database settings."
  :group 'scientific-mapping)

(defcustom scientific-mapping-cite-db-path
  (expand-file-name "citations.sqlite" user-emacs-directory)
  "Path to citation database."
  :group 'scientific-mapping-cite
  :type 'file)

(defvar scientific-mapping-cite--db nil
  "Database connection object.")

(defun scientific-mapping-cite--ensure-db ()
  "Ensure database is initialized."
  (unless (and scientific-mapping-cite--db
               (emacsql-live-p scientific-mapping-cite--db))
    (setq scientific-mapping-cite--db
          (emacsql-sqlite scientific-mapping-cite-db-path))
    (scientific-mapping-cite--init-schema)))

(defun scientific-mapping-cite--init-schema ()
  "Initialize database schema."
  (let ((db scientific-mapping-cite--db))
    ;; Create tables
    (emacsql db [:create-table citations
                 ((id integer :primary-key)
                  (title text)
                  (authors text)
                  (journal text)
                  (year integer)
                  (doi text :unique)
                  (keywords text)
                  (abstract text)
                  (added-at integer))])
    (emacsql db [:create-index citations-doi :on citations :columns [doi]])))

(provide 'cite/db)
;;; cite/db.el ends here
