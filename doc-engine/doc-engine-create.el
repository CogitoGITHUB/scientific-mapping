;;; doc-engine-create.el --- Document creation and formatting -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/doc-engine
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Document creation, identifiers, filenames, and front matter formatting.

;;; Code:

(require 'doc-engine-core)
(require 'doc-engine-id)

;;;; Quick Document Creation (Ultra-Frictionless)

;;;###autoload
(defun doc-engine-quick-note (title)
  "Create a quick note with just a title - everything else automated."
  (interactive "sNote title: ")
  (let* ((signature (doc-engine-sluggify-title title))
         (display-filename signature)
         (actual-filename (format "%s.org" signature))
         (target-dir (car (if (listp doc-engine-directory)
                             doc-engine-directory
                           (list doc-engine-directory))))
         (filepath (expand-file-name actual-filename target-dir)))

    ;; Handle conflicts
    (let ((counter 1))
      (while (file-exists-p filepath)
        (setq display-filename (format "%s-%d" signature counter))
        (setq actual-filename (format "%s.org" display-filename))
        (setq filepath (expand-file-name actual-filename target-dir))
        (setq counter (1+ counter))))

    ;; Create minimal document
    (with-temp-file filepath
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert (format "#+IDENTIFIER: %s\n" (doc-engine-generate-identifier nil (current-time))))
      (insert "\n* Notes\n\n"))

    ;; Register in ID map
    (let ((id (format-time-string "%Y%m%dT%H%M%S" (current-time))))
      (doc-engine-register-file id filepath))

    ;; Open file
    (find-file filepath)
    (message "Created quick note: %s" display-filename)))

;;;; Scientific Document Identifiers

(defun doc-engine-generate-identifier (&optional doi time)
  "Generate unique identifier for scientific document.
If DOI is provided, use DOI-based identifier. Otherwise use timestamp format."
  (if (and doi (doc-engine-validate-doi doi))
      (doc-engine-doi-to-identifier doi)
    (format-time-string "%Y%m%dT%H%M%S" (or time (current-time)))))

(defun doc-engine-doi-to-identifier (doi)
  "Convert DOI to filesystem-safe identifier."
  (let ((clean-doi (replace-regexp-in-string "/" "_" doi))
        (remove-prefix (replace-regexp-in-string "^[0-9]+\\.[0-9]+/" "" doi)))
    (replace-regexp-in-string "/" "_" remove-prefix)))

(defun doc-engine-validate-doi (doi)
  "Validate DOI format."
  (and (stringp doi)
       (string-match-p "10\\.[0-9]\\{4,9\\}/[^[:space:]]" doi)))

(defun doc-engine-identifier-p (identifier)
  "Check if STRING is a valid scientific document identifier."
  (string-match-p "^[0-9]\\{8\\}T[0-9]\\{6\\}$" identifier))

;;;; Scientific Document Filenames

(defun doc-engine-format-filename (signature keywords file-type)
  "Format ultra-clean filename for scientific document.
Format: SIGNATURE__KEYWORDS (no extension, no visible IDs)"
  (let ((keyword-str (if keywords
                        (concat "__"
                                (mapconcat 'downcase keywords "-"))
                      "")))
    (format "%s%s"
            (downcase signature)
            keyword-str)))

(defun doc-engine-sluggify-title (title)
  "Convert TITLE to a filesystem-safe signature."
  (let* ((downcase (downcase title))
         (remove-special (replace-regexp-in-string "[^[:alnum:] ]+" "" downcase))
         (replace-spaces (replace-regexp-in-string " +" "-" remove-special))
         (trim (replace-regexp-in-string "-+$" "" replace-spaces)))
    (replace-regexp-in-string "^-+" "" trim)))

;;;; Scientific Document Front Matter

(defun doc-engine-format-front-matter (title doi keywords identifier date
                                            citation abstract methodology results file-type)
  "Format front matter for scientific document based on FILE-TYPE."
  (pcase file-type
    ('org (doc-engine-format-org-front-matter title doi keywords identifier
                                              date citation abstract methodology results))
    ((or 'markdown-yaml 'markdown-toml)
     (doc-engine-format-markdown-front-matter title doi keywords identifier
                                              date citation abstract methodology results file-type))
    ('text (doc-engine-format-text-front-matter title doi keywords identifier
                                                date citation abstract methodology results))
    (_ (error "Unsupported file type: %s" file-type))))

(defun doc-engine-format-org-front-matter (title doi keywords identifier
                                              date citation abstract methodology results)
  "Format Org mode front matter for scientific document."
  (concat
   "#+TITLE: " title "\n"
   "#+DATE: [" (format-time-string "%Y-%m-%d %a %H:%M" date) "]\n"
   (when keywords
     (concat "#+SCIENTIFIC_TAGS: :"
             (mapconcat 'downcase keywords ":") ":\n"))
   "#+IDENTIFIER: " identifier "\n"
   (when doi
     (concat "#+DOI: " doi "\n"))
   (when citation
     (concat "#+CITATION: " citation "\n"))
   (when abstract
     (concat "#+ABSTRACT: " abstract "\n"))
   (when methodology
     (concat "#+METHODOLOGY: " methodology "\n"))
   (when results
     (concat "#+RESULTS: " results "\n"))
   "#+STARTUP: content\n"))

(defun doc-engine-format-markdown-front-matter (title doi keywords identifier
                                                date citation abstract methodology results file-type)
  "Format Markdown front matter for scientific document."
  (let* ((delimiter (if (eq file-type 'markdown-yaml) "---\n" "+++\n"))
         (properties (list
                     (cons "title" title)
                     (cons "date" (format-time-string "%Y-%m-%d %H:%M:%S" date))
                     (when keywords
                       (cons "keywords" (string-join keywords ", ")))
                     (cons "identifier" identifier)
                     (when doi
                       (cons "doi" doi))
                     (when citation
                       (cons "citation" citation))
                     (when abstract
                       (cons "abstract" abstract))
                     (when methodology
                       (cons "methodology" methodology))
                     (when results
                       (cons "results" results))))
         (props-string (mapconcat
                       (lambda (pair)
                         (format "%s: %s"
                                 (car pair)
                                 (doc-engine-markdown-escape (cdr pair))))
                       (remove 'nil properties)
                       "\n")))
    (concat delimiter props-string delimiter "\n\n")))

(defun doc-engine-format-text-front-matter (title doi keywords identifier
                                               date citation abstract methodology results)
  "Format plain text front matter for scientific document."
  (concat
   "Title: " title "\n"
   "Date: " (format-time-string "%Y-%m-%d %H:%M" date) "\n"
   (when keywords
     (concat "Keywords: " (string-join keywords ", ") "\n"))
   "Identifier: " identifier "\n"
   (when doi
     (concat "DOI: " doi "\n"))
   (when citation
     (concat "Citation: " citation "\n"))
   (when abstract
     (concat "Abstract: " abstract "\n"))
   (when methodology
     (concat "Methodology: " methodology "\n"))
   (when results
     (concat "Results: " results "\n"))
   "\n"))

(defun doc-engine-markdown-escape (str)
  "Escape special characters in STR for Markdown YAML/TOML."
  (if (stringp str)
      (replace-regexp-in-string "\"" "\\\"" str)
    str))

;;;; Org-Capture Integration with YASnippets

(require 'org-capture)
(require 'yasnippet)

(defvar doc-engine-capture-templates nil
  "Org-capture templates for scientific document creation.
See `org-capture-templates' for format.")

(defun doc-engine-setup-capture-templates ()
  "Setup org-capture templates using yasnippets."
  (setq doc-engine-capture-templates
        '(("r" "Research Article" entry
           (file (lambda () (expand-file-name "research-article.org" doc-engine-directory)))
           "* Research Article\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("l" "Literature Review" entry
           (file (lambda () (expand-file-name "literature-review.org" doc-engine-directory)))
           "* Literature Review\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("c" "Conference Paper" entry
           (file (lambda () (expand-file-name "conference-paper.org" doc-engine-directory)))
           "* Conference Paper\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("t" "Thesis/Dissertation" entry
           (file (lambda () (expand-file-name "thesis.org" doc-engine-directory)))
           "* Thesis\n%?"
           :empty-lines 1
           :jump-to-captured 0)

          ("n" "Quick Note" entry
           (file (lambda () (expand-file-name "notes.org" doc-engine-directory)))
           "* %?\n"
           :empty-lines 1
           :jump-to-captured 0))))

(defun doc-engine-capture-with-yasnippet (template-key)
  "Capture a new document using yasnippet TEMPLATE-KEY.
Uses yasnippet for template expansion instead of plain org-capture."
  (interactive "sTemplate key (r=research, l=review, c=conference, t=thesis, n=note): ")
  (let* ((template-key (or template-key "n"))
         (template-file
          (pcase template-key
            ("r" "research-article")
            ("l" "literature-review")
            ("c" "conference-paper")
            ("t" "thesis-master")
            ("n" "quick-note")
            (_ "quick-note")))
         (filepath (expand-file-name (format "%s.org" template-file) doc-engine-directory))
         (title (read-string "Document title: "))
         (identifier (doc-engine-generate-identifier nil (current-time))))

    ;; Create file with yasnippet
    (with-temp-file filepath
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert (format "#+IDENTIFIER: %s\n" identifier))
      (insert "#+STARTUP: content\n\n")
      (insert "* Notes\n"))

    ;; Expand yasnippet if available
    (when (featurep 'yasnippet)
      (find-file filepath)
      (goto-char (point-min))
      (re-search-forward "^\\* Notes$" nil t)
      (yas-expand-snippet (format "* %s\n\n" title)))

    ;; Register and open
    (doc-engine-register-file identifier filepath)
    (find-file filepath)
    (message "Created %s with template: %s" title template-key)))

;;;###autoload
(defun doc-engine-capture ()
  "Open document creation via org-capture with yasnippets.
Customize `org-capture-templates' to add more templates."
  (interactive)
  (doc-engine-setup-capture-templates)
  (let ((org-capture-templates doc-engine-capture-templates))
    (call-interactively 'org-capture)))

(provide 'doc-engine-create)

;;; doc-engine-create.el ends here