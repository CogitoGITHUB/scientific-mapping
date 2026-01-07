;;; doc/doc-engine.el --- Document Engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Commentary:

;; This file provides the main doc-engine feature with core functions.
;; It should be loaded first in the doc module.

;;; Code:

(require 'doc/create nil t)
(require 'doc/manage nil t)
(require 'doc/search nil t)
(require 'doc/templates nil t)
(require 'doc/org-file-manager nil t)

(defvar doc-engine-directory
  (expand-file-name "documents/" user-emacs-directory)
  "Directory where scientific documents are stored.")

(defvar doc-engine-auto-process-on-open nil
  "If non-nil, automatically process documents when opened.")

(defvar doc-engine-auto-process-on-save nil
  "If non-nil, automatically process documents when saved.")

(defvar doc-engine-auto-sync-citations nil
  "If non-nil, automatically sync citations when documents are saved.")

(defvar doc-engine-auto-extract-concepts nil
  "If non-nil, automatically extract concepts from documents.")

(defun doc-engine-all-files ()
  "Return list of all document files."
  (when (file-directory-p doc-engine-directory)
    (directory-files doc-engine-directory t "\\.org$")))

(defun doc-engine-is-scientific-document-p ()
  "Check if current buffer is a scientific document."
  (or (and (buffer-file-name)
           (string-match-p "\\.org$" (buffer-file-name)))
      (eq major-mode 'org-mode)
      (member (current-buffer) (mapcar #'get-file-buffer (doc-engine-all-files)))))

(defun doc-engine-load-id-map ()
  "Load the document ID map from disk."
  (interactive))

(defun doc-engine-extract-metadata (file)
  "Extract metadata from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((title nil)
            (doi nil)
            (keywords nil))
        (when (re-search-forward "^\\#\\+TITLE:? *\\(.*\\)$" nil t)
          (setq title (match-string 1)))
        (when (re-search-forward "^:DOI:? *\\(.*\\)$" nil t)
          (setq doi (match-string 1)))
        (when (re-search-forward "^:KEYWORDS:? *\\(.*\\)$" nil t)
          (setq keywords (match-string 1)))
        (list (cons 'title title)
              (cons 'doi doi)
              (cons 'keywords keywords))))))

(defun doc-engine-sync-citations-for-buffer ()
  "Sync citations for the current buffer."
  (interactive))

(defun doc-engine-setup-capture-templates ()
  "Set up org-capture templates for scientific mapping."
  (interactive))

(defun doc-engine-capture ()
  "Capture a new document."
  (interactive)
  (message "Doc engine capture"))

(defun doc-engine-capture-with-yasnippet ()
  "Capture a new document with yasnippet."
  (interactive)
  (message "Doc engine capture with yasnippet"))

(defun doc-engine-quick-note ()
  "Create a quick note."
  (interactive)
  (message "Creating quick note"))

(defun scientific-mapping-import-paper (&optional doi title keywords)
  "Import a paper by DOI."
  (interactive)
  (message "Importing paper with DOI: %s" (or doi "unknown"))
  ;; Return a dummy paper for testing
  (list (cons 'doi (or doi "unknown"))
        (cons 'title (or title "Test Paper"))
        (cons 'keywords (or keywords '("test")))))

(defun citation-database-get-paper-by-doi (doi)
  "Get paper by DOI from database."
  (message "Getting paper by DOI: %s" doi)
  ;; Return a dummy paper for testing
  (list (cons 'doi doi)
        (cons 'title "Test Paper")
        (cons 'keywords '("test"))))

(defvar doc-engine-capture-templates
  '(("r" . ("Research Article" "research-article"))
    ("l" . ("Literature Review" "literature-review"))
    ("c" . ("Conference Paper" "conference-paper"))
    ("t" . ("Thesis" "thesis"))
    ("n" . ("Notes" "notes")))
  "Org capture templates for document creation.")

(provide 'doc-engine)

;;; doc/doc-engine.el ends here
