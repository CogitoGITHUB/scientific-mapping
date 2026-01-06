;;; org-file-manager.el --- Org-mode file manager with structured views -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

;;; Commentary:
;; Org File Manager provides a structured, Org-mode-aware interface for managing
;; scientific documents. Unlike plain file managers, it displays Org files with
;; their titles, properties, and content structure, making it easy to navigate
;; and manage research documents.

;; Features:
;; - Org-aware file display with titles and properties
;; - Hierarchical view of document structure
;; - Quick access to document metadata
;; - Seamless integration with scientific mapping workflow
;; - Visual indicators for document status (analyzed, cited, etc.)

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)

;;;; Configuration

(defgroup org-file-manager ()
  "Org-mode aware file manager for scientific documents."
  :group 'scientific-mapping)

(defcustom org-file-manager-directory nil
  "Directory to manage. If nil, uses current directory."
  :group 'org-file-manager
  :type 'directory)

(defcustom org-file-manager-show-properties t
  "Show Org properties in file listings."
  :group 'org-file-manager
  :type 'boolean)

(defcustom org-file-manager-show-headings t
  "Show top-level headings in file previews."
  :group 'org-file-manager
  :type 'boolean)

(defcustom org-file-manager-max-preview-lines 20
  "Maximum lines to show in file preview."
  :group 'org-file-manager
  :type 'integer)

(defcustom org-file-manager-sort-method 'title
  "Method for sorting files.
Available options:
  - `title': By Org title
  - `date': By file modification date
  - `name': By filename
  - `status': By document status (analyzed, cited, etc.)"
  :group 'org-file-manager
  :type '(choice (const :tag "By Title" title)
                 (const :tag "By Date" date)
                 (const :tag "By Name" name)
                 (const :tag "By Status" status)))

;;;; Core Data Structures

(defvar-local org-file-manager--current-dir nil
  "Current directory being displayed.")

(defvar-local org-file-manager--file-cache nil
  "Cache of parsed Org file information.")

(defvar-local org-file-manager--buffer-dirty-p nil
  "Non-nil if buffer has unsaved changes.")

;;;; Org File Parsing

(defun org-file-manager--parse-org-file (filepath)
  "Parse Org file at FILEPATH and return structured data."
  (when (and (file-exists-p filepath)
             (string-match-p "\\.org$" filepath))
    (with-temp-buffer
      (insert-file-contents filepath)
      (org-mode)
      (let* ((title (org-get-title))
             (properties (org-entry-properties nil))
             (headings (org-file-manager--extract-headings))
             (stats (org-file-manager--get-file-stats))
             (status (org-file-manager--get-document-status properties)))
        `((title . ,(or title (file-name-base filepath)))
          (filepath . ,filepath)
          (properties . ,properties)
          (headings . ,headings)
          (stats . ,stats)
          (status . ,status)
          (modified . ,(nth 5 (file-attributes filepath))))))))

(defun org-file-manager--extract-headings ()
  "Extract top-level headings from current buffer."
  (let ((headings '()))
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (let ((heading (org-get-heading t t)))
        (when heading
          (push heading headings))))
    (reverse headings)))

(defun org-file-manager--get-file-stats ()
  "Get statistics about the current Org file."
  `((word-count . ,(count-words (point-min) (point-max)))
    (heading-count . ,(length (org-file-manager--extract-headings)))
    (property-count . ,(length (org-entry-properties nil)))
    (line-count . ,(count-lines (point-min) (point-max)))))

(defun org-file-manager--get-document-status (properties)
  "Determine document status from properties."
  (cond
   ((plist-get properties "AI_ANALYZED") 'analyzed)
   ((plist-get properties "DOI") 'published)
   ((plist-get properties "SCIENTIFIC_CONCEPTS") 'conceptualized)
   (t 'draft)))

;;;; File Display Functions

(defun org-file-manager--format-file-entry (file-data)
  "Format a file entry for display."
  (let* ((title (cdr (assoc 'title file-data)))
         (filepath (cdr (assoc 'filepath file-data)))
         (status (cdr (assoc 'status file-data)))
         (stats (cdr (assoc 'stats file-data)))
         (filename (file-name-nondirectory filepath))
         (status-icon (org-file-manager--status-icon status))
         (word-count (cdr (assoc 'word-count stats))))
    (format "%s %s [%dw] %s"
            status-icon
            title
            word-count
            (if org-file-manager-show-properties
                (org-file-manager--format-properties-preview file-data)
              ""))))

(defun org-file-manager--status-icon (status)
  "Return icon for document status."
  (pcase status
    ('analyzed "🧠")
    ('published "📄")
    ('conceptualized "💡")
    ('draft "📝")
    (_ "📄")))

(defun org-file-manager--format-properties-preview (file-data)
  "Format a preview of Org properties."
  (let* ((properties (cdr (assoc 'properties file-data)))
         (doi (cdr (assoc "DOI" properties)))
         (date (cdr (assoc "DATE" properties)))
         (tags (cdr (assoc "SCIENTIFIC_CONCEPTS" properties))))
    (concat
     (when doi (format " | DOI: %s" doi))
     (when date (format " | %s" date))
     (when tags (format " | Tags: %s" tags)))))

;;;; Main Interface

;;;###autoload
(defun org-file-manager ()
  "Open Org File Manager for current or specified directory."
  (interactive)
  (let* ((dir (or org-file-manager-directory
                  (read-directory-name "Directory: " default-directory)))
         (buffer-name (format "*Org Files: %s*" (file-name-nondirectory
                                                (directory-file-name dir)))))
    (switch-to-buffer buffer-name)
    (org-file-manager-mode)
    (org-file-manager--populate-buffer dir)))

(define-derived-mode org-file-manager-mode org-mode "Org-Files"
  "Major mode for managing Org files with structured views."
  (setq-local org-file-manager--current-dir (or org-file-manager-directory default-directory))
  (org-file-manager--setup-keybindings))

(defun org-file-manager--setup-keybindings ()
  "Setup keybindings for org-file-manager-mode."
  (local-set-key (kbd "RET") 'org-file-manager-open-file)
  (local-set-key (kbd "o") 'org-file-manager-open-file-other-window)
  (local-set-key (kbd "v") 'org-file-manager-preview-file)
  (local-set-key (kbd "g") 'org-file-manager-refresh)
  (local-set-key (kbd "s") 'org-file-manager-sort)
  (local-set-key (kbd "f") 'org-file-manager-filter)
  (local-set-key (kbd "c") 'org-file-manager-create-file)
  (local-set-key (kbd "d") 'org-file-manager-delete-file)
  (local-set-key (kbd "r") 'org-file-manager-rename-file)
  (local-set-key (kbd "q") 'org-file-manager-quit))

(defun org-file-manager--populate-buffer (dir)
  "Populate buffer with Org files from DIR."
  (let ((files (org-file-manager--get-org-files dir)))
    (erase-buffer)
    (insert "#+TITLE: Org File Manager\n")
    (insert (format "#+DESCRIPTION: Managing Org files in %s\n\n" dir))
    (insert "* File Overview\n\n")

    ;; Summary statistics
    (org-file-manager--insert-summary files)

    (insert "* Files\n\n")

    ;; File listings
    (dolist (file files)
      (let ((file-data (org-file-manager--parse-org-file file)))
        (when file-data
          (insert "** ")
          (insert (org-file-manager--format-file-entry file-data))
          (insert "\n")
          (insert (format "   :PROPERTIES:\n"))
          (insert (format "   :FILE: %s\n" file))
          (insert (format "   :END:\n"))
          (when org-file-manager-show-headings
            (org-file-manager--insert-headings-preview file-data))
          (insert "\n"))))

    (goto-char (point-min))
    (org-content 3)))

(defun org-file-manager--get-org-files (dir)
  "Get list of Org files in DIR."
  (let ((files '()))
    (dolist (file (directory-files dir t "\\.org$" t))
      (when (file-regular-p file)
        (push file files)))
    (org-file-manager--sort-files files)))

(defun org-file-manager--sort-files (files)
  "Sort FILES according to org-file-manager-sort-method."
  (sort files
        (lambda (a b)
          (let ((data-a (org-file-manager--parse-org-file a))
                (data-b (org-file-manager--parse-org-file b)))
            (pcase org-file-manager-sort-method
              ('title (string< (or (cdr (assoc 'title data-a)) "")
                              (or (cdr (assoc 'title data-b)) "")))
              ('date (time-less-p (cdr (assoc 'modified data-b))
                                 (cdr (assoc 'modified data-a))))
              ('name (string< (file-name-nondirectory a)
                             (file-name-nondirectory b)))
              ('status (org-file-manager--status-sort (cdr (assoc 'status data-a))
                                                     (cdr (assoc 'status data-b))))
              (_ (string< a b)))))))

(defun org-file-manager--status-sort (status-a status-b)
  "Sort by document status priority."
  (let ((priority '(analyzed published conceptualized draft)))
    (< (cl-position status-a priority)
       (cl-position status-b priority))))

(defun org-file-manager--insert-summary (files)
  "Insert summary statistics for FILES."
  (let ((total-files (length files))
        (analyzed 0)
        (published 0)
        (total-words 0))
    (dolist (file files)
      (let ((data (org-file-manager--parse-org-file file)))
        (when data
          (let ((status (cdr (assoc 'status data)))
                (stats (cdr (assoc 'stats data))))
            (when (eq status 'analyzed) (cl-incf analyzed))
            (when (eq status 'published) (cl-incf published))
            (cl-incf total-words (cdr (assoc 'word-count stats)))))))
    (insert (format "- Total files: %d\n" total-files))
    (insert (format "- Analyzed: %d\n" analyzed))
    (insert (format "- Published: %d\n" published))
    (insert (format "- Total words: %d\n\n" total-words))))

(defun org-file-manager--insert-headings-preview (file-data)
  "Insert preview of headings for FILE-DATA."
  (let ((headings (cdr (assoc 'headings file-data))))
    (when headings
      (insert "   Top headings:\n")
      (dolist (heading (seq-take headings 3))
        (insert (format "   - %s\n" heading)))
      (when (> (length headings) 3)
        (insert (format "   - ... and %d more\n" (- (length headings) 3)))))))

;;;; Interactive Commands

(defun org-file-manager-open-file ()
  "Open the Org file at point."
  (interactive)
  (let ((file (org-file-manager--get-file-at-point)))
    (when file
      (find-file file))))

(defun org-file-manager-open-file-other-window ()
  "Open the Org file at point in other window."
  (interactive)
  (let ((file (org-file-manager--get-file-at-point)))
    (when file
      (find-file-other-window file))))

(defun org-file-manager-preview-file ()
  "Preview the Org file at point."
  (interactive)
  (let ((file (org-file-manager--get-file-at-point)))
    (when file
      (with-current-buffer (get-buffer-create "*Org File Preview*")
        (erase-buffer)
        (insert-file-contents file nil 0 (* org-file-manager-max-preview-lines 80))
        (org-mode)
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))))

(defun org-file-manager-refresh ()
  "Refresh the file listing."
  (interactive)
  (org-file-manager--populate-buffer org-file-manager--current-dir))

(defun org-file-manager-sort ()
  "Change sort method and refresh."
  (interactive)
  (let ((method (completing-read "Sort by: "
                                '(("title" . title)
                                  ("date" . date)
                                  ("name" . name)
                                  ("status" . status))
                                nil t)))
    (when method
      (setq-local org-file-manager-sort-method (intern method))
      (org-file-manager-refresh))))

(defun org-file-manager-filter ()
  "Filter files by criteria."
  (interactive)
  (let ((filter (read-string "Filter (status:analyzed, tag:concept): ")))
    (message "Filtering not yet implemented: %s" filter)))

(defun org-file-manager-create-file ()
  "Create a new Org file."
  (interactive)
  (call-interactively 'doc-engine-quick-note)
  (org-file-manager-refresh))

(defun org-file-manager-delete-file ()
  "Delete the Org file at point."
  (interactive)
  (let ((file (org-file-manager--get-file-at-point)))
    (when (and file (yes-or-no-p (format "Delete %s? " (file-name-nondirectory file))))
      (delete-file file)
      (org-file-manager-refresh))))

(defun org-file-manager-rename-file ()
  "Rename the Org file at point."
  (interactive)
  (let* ((old-file (org-file-manager--get-file-at-point))
         (new-name (read-string "New name: " (file-name-base old-file))))
    (when (and old-file new-name)
      (let ((new-file (expand-file-name (concat new-name ".org")
                                       (file-name-directory old-file))))
        (rename-file old-file new-file)
        (org-file-manager-refresh)))))

(defun org-file-manager-quit ()
  "Quit Org File Manager."
  (interactive)
  (kill-buffer))

(defun org-file-manager--get-file-at-point ()
  "Get the file path at current point."
  (let ((element (org-element-at-point)))
    (when (eq (org-element-type element) 'headline)
      (org-entry-get nil "FILE"))))

(provide 'org-file-manager)

;;; org-file-manager.el ends here

;;; Usage:
;;
;; Open Org File Manager:
;; M-x org-file-manager
;;
;; Key bindings in org-file-manager-mode:
;; RET - Open file
;; o   - Open file in other window
;; v   - Preview file
;; g   - Refresh listing
;; s   - Change sort method
;; f   - Filter files
;; c   - Create new file
;; d   - Delete file
;; r   - Rename file
;; q   - Quit