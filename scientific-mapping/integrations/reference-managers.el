;;; reference-managers.el --- Integration with reference managers -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This file provides integration with popular reference managers:
;; - Zotero: Sync libraries and collections
;; - Mendeley: Import references and annotations
;; - EndNote: Export and import references
;; - BibTeX: Direct BibTeX file support

;;; Code:

(require 'json)
(require 'org)
(eval-when-compile (require 'subr-x))

(defgroup reference-managers ()
  "Integration with reference managers."
  :group 'scientific-mapping)

;;;; Zotero Integration

(defcustom reference-managers-zotero-api-key ""
  "Zotero API key for library access."
  :group 'reference-managers
  :type 'string)

(defcustom reference-managers-zotero-user-id ""
  "Zotero user ID or group ID."
  :group 'reference-managers
  :type 'string)

(defcustom reference-managers-zotero-library-type "user"
  "Zotero library type: 'user' or 'group'."
  :group 'reference-managers
  :type '(choice (const "user") (const "group")))

(defun reference-managers-zotero-get-collections ()
  "Retrieve Zotero collections."
  (interactive)
  (if (or (string-empty-p reference-managers-zotero-api-key)
          (string-empty-p reference-managers-zotero-user-id))
      (message "Zotero API key and user ID required")
    (let* ((url (format "https://api.zotero.org/%s/%s/collections"
                       reference-managers-zotero-library-type
                       reference-managers-zotero-user-id))
           (url-request-extra-headers
            `(("Authorization" . ,(format "Bearer %s" reference-managers-zotero-api-key)))
            ("Zotero-API-Version" . "3"))
           (response (url-retrieve-synchronously url)))
      (if response
          (with-current-buffer response
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (let ((collections (json-read)))
                (reference-managers-display-zotero-collections collections))))
        (message "Failed to connect to Zotero API")))))

(defun reference-managers-display-zotero-collections (collections)
  "Display Zotero collections in an Org buffer."
  (let ((buffer (get-buffer-create "*Zotero Collections*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Zotero Collections\n\n")
      (insert "* Collections\n\n")
      (dolist (collection collections)
        (let ((name (cdr (assoc 'name collection)))
              (key (cdr (assoc 'key collection)))
              (parent (cdr (assoc 'parentCollection collection))))
          (insert (format "** %s\n" name))
          (insert ":PROPERTIES:\n")
          (insert (format ":ZOTERO_KEY: %s\n" key))
          (insert (format ":PARENT: %s\n" (or parent "Root")))
          (insert ":END:\n")
          (insert "- [ ] Sync items from this collection\n")
          (insert "- [ ] Import to scientific mapping\n\n")))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun reference-managers-zotero-get-items (collection-key)
  "Retrieve items from a Zotero collection."
  (interactive "sCollection key: ")
  (if (or (string-empty-p reference-managers-zotero-api-key)
          (string-empty-p reference-managers-zotero-user-id))
      (message "Zotero API key and user ID required")
    (let* ((url (format "https://api.zotero.org/%s/%s/collections/%s/items"
                       reference-managers-zotero-library-type
                       reference-managers-zotero-user-id
                       collection-key))
           (url-request-extra-headers
            `(("Authorization" . ,(format "Bearer %s" reference-managers-zotero-api-key)))
            ("Zotero-API-Version" . "3"))
           (response (url-retrieve-synchronously url)))
      (if response
          (with-current-buffer response
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (let ((items (json-read)))
                (reference-managers-display-zotero-items items collection-key))))
        (message "Failed to retrieve Zotero items")))))

(defun reference-managers-display-zotero-items (items collection-key)
  "Display Zotero items in an Org buffer."
  (let ((buffer (get-buffer-create "*Zotero Items*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Zotero Items\n\n")
      (insert (format "* Items from Collection %s\n\n" collection-key))

      (dolist (item items)
        (let* ((data (cdr (assoc 'data item)))
               (title (cdr (assoc 'title data)))
               (creators (cdr (assoc 'creators data)))
               (date (cdr (assoc 'date data)))
               (item-type (cdr (assoc 'itemType data)))
               (doi (cdr (assoc 'DOI data)))
               (url (cdr (assoc 'url data))))
          (insert (format "** %s\n" (or title "Untitled")))
          (insert ":PROPERTIES:\n")
          (insert (format ":TYPE: %s\n" (or item-type "")))
          (insert (format ":DOI: %s\n" (or doi "")))
          (insert (format ":URL: %s\n" (or url "")))
          (insert (format ":DATE: %s\n" (or date "")))
          (insert ":SOURCE: Zotero\n")
          (insert ":END:\n")

          (when creators
            (insert "*** Authors\n")
            (dolist (creator creators)
              (let ((first-name (cdr (assoc 'firstName creator)))
                    (last-name (cdr (assoc 'lastName creator))))
                (insert (format "- %s %s\n" (or first-name "") (or last-name "")))))
            (insert "\n"))

          (insert "*** Actions\n")
          (insert "- [ ] Import to scientific mapping\n")
          (insert "- [ ] Add to citation database\n")
          (insert "- [ ] Download PDF\n")
          (insert "- [ ] Review and annotate\n\n")))

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; BibTeX Integration

(defun reference-managers-import-bibtex (file)
  "Import references from a BibTeX FILE."
  (interactive "fSelect BibTeX file: ")
  (if (not (file-exists-p file))
      (message "File does not exist: %s" file)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (let ((entries '()))
        (while (re-search-forward "@\\(\\w+\\){\\([^,]+\\)," nil t)
          (let ((entry-type (match-string 1))
                (entry-key (match-string 2))
                (entry-start (match-beginning 0)))
            (when (re-search-forward "^}" nil t)
              (let ((entry-text (buffer-substring entry-start (match-end 0))))
                (push (reference-managers-parse-bibtex-entry entry-text entry-type entry-key) entries)))))
        (reference-managers-display-bibtex-entries entries file)))))

(defun reference-managers-parse-bibtex-entry (text type key)
  "Parse a BibTeX entry into structured data."
  (let ((entry `((type . ,type) (key . ,key))))
    ;; Extract fields
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\w+\\)\\s-*=\\s-*[{" nil t)
        (let ((field-name (match-string 1))
              (field-start (match-end 0)))
          (when (re-search-forward "[}]" nil t)
            (let ((field-value (buffer-substring field-start (1- (match-end 0)))))
              (push `(,(intern field-name) . ,field-value) entry))))))
    entry))

(defun reference-managers-display-bibtex-entries (entries file)
  "Display BibTeX entries in an Org buffer."
  (let ((buffer (get-buffer-create "*BibTeX Import*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: BibTeX Import\n\n")
      (insert (format "* Imported from: %s\n\n" file))
      (insert (format "** %d entries found\n\n" (length entries)))

      (dolist (entry entries)
        (let ((title (cdr (assoc 'title entry)))
              (authors (cdr (assoc 'author entry)))
              (year (cdr (assoc 'year entry)))
              (journal (cdr (assoc 'journal entry)))
              (doi (cdr (assoc 'doi entry)))
              (type (cdr (assoc 'type entry))))
          (insert (format "** %s\n" (or title "Untitled")))
          (insert ":PROPERTIES:\n")
          (insert (format ":TYPE: %s\n" (or type "")))
          (insert (format ":DOI: %s\n" (or doi "")))
          (insert (format ":YEAR: %s\n" (or year "")))
          (insert (format ":JOURNAL: %s\n" (or journal "")))
          (insert ":SOURCE: BibTeX\n")
          (insert ":END:\n")

          (when authors
            (insert "*** Authors\n")
            (insert (split-string authors " and ")))
          (insert "\n")

          (insert "*** Actions\n")
          (insert "- [ ] Import to scientific mapping\n")
          (insert "- [ ] Add to citation database\n")
          (insert "- [ ] Verify metadata\n\n")))

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; Integration with Scientific Mapping

(defun reference-managers-import-to-scientific-mapping (source)
  "Import references from SOURCE reference manager."
  (interactive
   (list (completing-read "Import from: " '("Zotero" "BibTeX") nil t)))
  (cond
   ((string= source "Zotero")
    (call-interactively 'reference-managers-zotero-get-collections))
   ((string= source "BibTeX")
    (call-interactively 'reference-managers-import-bibtex))))

(provide 'reference-managers)

;;; reference-managers.el ends here