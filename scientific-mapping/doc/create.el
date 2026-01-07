;;; doc/create.el --- Document creation  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Document creation functions.
;; - Quick notes
;; - Full document creation
;; - DOI-based imports

;;; Code:

(defgroup scientific-mapping-doc nil
  "Document management settings."
  :group 'scientific-mapping)

(defcustom scientific-mapping-doc-directory
  (expand-file-name "documents" user-emacs-directory)
  "Default directory for scientific documents."
  :group 'scientific-mapping-doc
  :type 'directory)

(defcustom scientific-mapping-doc-type 'org
  "Default file type for new documents."
  :group 'scientific-mapping-doc
  :type '(choice (const org)
                 (const markdown)))

(defun scientific-mapping-doc-create (&optional title doi keywords)
  "Create a new scientific document.
TITLE: Document title (prompts if nil)
DOI: Optional DOI for the document
KEYWORDS: List of keywords"
  (interactive)
  (let* ((title (or title (read-string "Title: ")))
         (doi (or doi (read-string "DOI (optional): ")))
         (keywords-str (read-string "Keywords (comma-separated): "))
         (keywords (when (string-match-p "[^[:space:]]" keywords-str)
                     (split-string keywords-str ", " t)))
         (id (scientific-mapping--timestamp-id))
         (filename (format "%s__%s.org"
                          (scientific-mapping--sluggify title)
                          (if keywords
                              (mapconcat #'scientific-mapping--sluggify keywords "-")
                            "notes")))
         (path (expand-file-name filename
                                 (scientific-mapping-doc--ensure-dir))))
    (with-temp-file path
      (insert "#+TITLE: " title "\n")
      (insert "#+DATE: " (scientific-mapping--timestamp 'human) "\n")
      (insert "#+IDENTIFIER: " id "\n")
      (when doi
        (insert "#+DOI: " doi "\n"))
      (when keywords
        (insert "#+KEYWORDS: " (mapconcat 'identity keywords ", ") "\n"))
      (insert "#+STARTUP: content\n\n")
      (insert "* Notes\n\n"))
    (find-file path)
    (message "Created: %s" path)))

(defun scientific-mapping-doc-quick-note (&optional title)
  "Create a quick note with just a TITLE."
  (interactive "sNote title: ")
  (let* ((id (scientific-mapping--timestamp-id))
         (filename (format "%s.org" (scientific-mapping--sluggify title)))
         (path (expand-file-name filename
                                 (scientific-mapping-doc--ensure-dir))))
    (with-temp-file path
      (insert "#+TITLE: " title "\n")
      (insert "#+DATE: " (scientific-mapping--timestamp 'human) "\n")
      (insert "#+IDENTIFIER: " id "\n")
      (insert "#+STARTUP: content\n\n")
      (insert "* Notes\n\n"))
    (find-file path)
    (message "Created quick note: %s" path)))

(defun scientific-mapping-doc--ensure-dir ()
  "Ensure document directory exists."
  (unless (file-directory-p (scientific-mapping-doc--ensure-dir-helper))
    (make-directory (scientific-mapping-doc--ensure-dir-helper) t))
  (scientific-mapping-doc--ensure-dir-helper))

(defun scientific-mapping-doc--ensure-dir-helper ()
  "Return document directory."
  (expand-file-name (format-time-string "%Y-%m")
                   scientific-mapping-doc-directory))

(provide 'doc/create)
;;; doc/create.el ends here
