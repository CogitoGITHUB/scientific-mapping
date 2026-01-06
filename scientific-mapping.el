;;; scientific-mapping.el --- Main entry point for scientific knowledge mapping system -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/scientific-mapping
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
;; You should have a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; scientific-mapping provides an integrated platform for scientific research,
;; literature review, and knowledge mapping. It combines four major components:
;;
;; 1. scientific-document-engine - Paper and note management
;; 2. citation-database - Bibliographic database and citation tracking
;; 3. scientific-visualizer - 3D visualization of knowledge networks
;; 4. concept-relationships - Concept mapping and theoretical relationships
;;
;; The system is designed for researchers, academics, and scientists who need to:
;; - Manage scientific literature efficiently
;; - Track citations and bibliographic information
;; - Build concept maps of theoretical frameworks
;; - Visualize research networks and relationships
;; - Extract insights from large literature collections

;;; Code:

;; Add all components to load path
(let ((root-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "doc-engine" root-dir))
  (add-to-list 'load-path (expand-file-name "citation-database" root-dir))
  (add-to-list 'load-path (expand-file-name "viz-engine" root-dir))
  (add-to-list 'load-path (expand-file-name "concept-relationships" root-dir))
  (add-to-list 'load-path (expand-file-name "timeline-engine" root-dir))
  (add-to-list 'load-path (expand-file-name "concept-tree" root-dir))
  (add-to-list 'load-path (expand-file-name "snippet-engine" root-dir))
  (add-to-list 'load-path (expand-file-name "core" root-dir)))

;; Load core modules
(require 'doc-engine)
(require 'citation-database)
(require 'viz-engine)
(require 'concept-relationships)
(require 'timeline-engine)
(require 'concept-tree)
(require 'yasnippet)

(defconst scientific-mapping-version "1.0.0"
  "Version of the scientific-mapping system.")

(defgroup scientific-mapping ()
  "Integrated scientific knowledge mapping system."
  :group 'applications
  :link '(info-link "(scientific-mapping) Top")
  :link '(url-link :tag "Homepage" "https://scientific-mapping.org"))

;;;; Configuration

(defcustom scientific-mapping-auto-enable t
  "If non-nil, automatically enable all components when loading scientific-mapping."
  :group 'scientific-mapping
  :type 'boolean)

(defcustom scientific-mapping-components
  '(doc-engine citation-database viz-engine concept-relationships timeline-engine concept-tree yas)
  "List of components to load and enable."
  :group 'scientific-mapping
  :type '(set (const :tag "Document Engine" doc-engine)
                   (const :tag "Citation Database" citation-database)
                   (const :tag "Visualization Engine" viz-engine)
                   (const :tag "Concept Relationships" concept-relationships)
                   (const :tag "Timeline Engine" timeline-engine)
                   (const :tag "Concept Tree" concept-tree)
                   (const :tag "YASnippet Templates" yas)))

;;;; Main Mode

;;;###autoload
(define-minor-mode scientific-mapping-mode
  "Enable the complete scientific knowledge mapping system."
  :lighter " SciMap"
  :global t
  :group 'scientific-mapping
  :init-value nil
  (cond
   (scientific-mapping-mode
    ;; Enable selected components
    (when (memq 'doc-engine scientific-mapping-components)
      (message "Loading doc-engine..."))
    (when (memq 'citation-database scientific-mapping-components)
      (citation-database-autosync-mode 1)
      (message "Loading citation-database..."))
    (when (memq 'viz-engine scientific-mapping-components)
      (viz-engine-mode 1)
      (message "Loading viz-engine..."))
    (when (memq 'concept-relationships scientific-mapping-components)
      (message "Loading concept-relationships..."))
    (when (memq 'timeline-engine scientific-mapping-components)
      (message "Loading timeline-engine..."))
    (when (memq 'concept-tree scientific-mapping-components)
      (message "Loading concept-tree..."))
    (when (memq 'yas scientific-mapping-components)
      (yas-global-mode 1)
      (message "Loading yas templates..."))

    (message "Scientific Knowledge Mapping System enabled. Use M-x scientific-mapping-help for commands."))
   (t
    ;; Disable all components
    (citation-database-autosync-mode -1)
    (viz-engine-mode -1)
    (message "Scientific Knowledge Mapping System disabled."))))

;;;; Interactive Commands

;;;###autoload
(defun scientific-mapping-start ()
  "Start the scientific knowledge mapping system."
  (interactive)
  (scientific-mapping-mode 1)
  (when (y-or-n-p "Open visualizer interface? ")
    (scientific-visualizer-open))
  (message "Scientific Knowledge Mapping System started."))

;;;###autoload
(defun scientific-mapping-stop ()
  "Stop the scientific knowledge mapping system."
  (interactive)
  (scientific-mapping-mode -1)
  (message "Scientific Knowledge Mapping System stopped."))

;;;###autoload
(defun scientific-mapping-help ()
  "Display help for scientific-mapping commands."
  (interactive)
  (describe-mode 'scientific-mapping-mode))

;;;###autoload
(defun scientific-mapping-status ()
  "Show status of all scientific-mapping components."
  (interactive)
  (let ((buffer (get-buffer-create "*Scientific Mapping Status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Scientific Knowledge Mapping System Status ===\n\n")
      
       (insert "Components:\n")
       (insert (format "  Document Engine: %s\n"
                       (if (memq 'doc-engine scientific-mapping-components)
                           "Enabled" "Disabled")))
       (insert (format "  Citation Database: %s\n"
                       (if citation-database-autosync-mode
                           "Active" "Inactive")))
       (insert (format "  Visualization Engine: %s\n"
                       (if viz-engine-mode
                           "Running" "Stopped")))
       (insert (format "  Concept Relationships: %s\n"
                       (if (memq 'concept-relationships scientific-mapping-components)
                           "Enabled" "Disabled")))
      
      (insert "\nDatabase Statistics:\n")
      (let ((stats (citation-database-stats)))
        (when stats
          (insert (format "  Total Papers: %d\n"
                          (plist-get stats :total-papers)))
          (insert (format "  Total Citations: %d\n"
                          (plist-get stats :total-citations)))
          (insert (format "  Total Authors: %d\n"
                          (plist-get stats :total-authors)))
          (insert (format "  Total Journals: %d\n"
                          (plist-get stats :total-journals)))
          (insert (format "  Total Concepts: %d\n"
                          (plist-get stats :total-concepts)))))
      
      (insert "\nDocument Statistics:\n")
       (let ((doc-stats (doc-engine-stats)))
         (when doc-stats
           (insert (format "  Total Documents: %d\n"
                           (length doc-stats)))))
      
      (goto-char (point-min))
      (special-mode)))
    (display-buffer buffer)))

;;;; Quick Workflows

;;;###autoload
(defun scientific-mapping-import-paper ()
  "Quick workflow: Import a scientific paper from DOI."
  (interactive)
  (condition-case err
      (let* ((doi (read-string "Enter DOI: "))
             (title (read-string "Enter paper title: "))
             (keywords (split-string (read-string "Enter keywords (comma-separated): ") ",")))
        (unless (and doi title)
          (error "DOI and title are required"))
    (doc-engine-create
     :title title
     :doi doi
     :keywords keywords)
        (message "Paper imported. Add concepts and citations with C-c c and C-c p."))
    (error (message "Error importing paper: %s" (error-message-string err)))))

;;;###autoload
(defun scientific-mapping-create-concept-map ()
  "Quick workflow: Create a new concept map entry."
  (interactive)
  (let* ((concept-name (read-string "Enter concept name: "))
         (definition (read-string "Enter concept definition: "))
         (domain (completing-read "Enter domain: "
                                    '("machine-learning" "nlp" "computer-vision" "statistics"
                                      "theoretical-cs" "software-engineering" "biology"))))
    (let ((concept-id (concept-relationships-create-entry concept-name)))
      (with-current-buffer (find-file-noselect
                           (expand-file-name (concat concept-name ".org")
                                             concept-relationships-path))
        (goto-char (point-max))
        (insert "\n* Definition\n" definition "\n")
        (insert "* Domain\n" domain "\n"))
      (message "Concept created. Visualize with M-x concept-relationships-visualize")))))

;;;###autoload
(defun scientific-mapping-literature-review (topic)
  "Generate literature review for TOPIC using database."
  (interactive "sLiterature review topic: ")
  (let* ((papers (citation-database-search-papers topic 20))
         (buffer (get-buffer-create (format "*Literature Review: %s*" topic))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "= Literature Review: " topic " =\n\n")
      (insert (format-time-string "Generated: %Y-%m-%d %H:%M\n\n" (current-time)))
      
      (dolist (paper papers)
        (let ((title (elt paper 2))    ; title
              (doi (elt paper 3))          ; doi
              (year (elt paper 6))        ; year
              (authors (elt paper 4)))      ; authors
          (insert "\n* " title "\n")
          (when authors
            (insert "  Authors: " authors "\n"))
          (when year
            (insert "  Year: " (number-to-string year) "\n"))
          (when doi
            (insert "  DOI: " doi "\n"))))
      
      (goto-char (point-min))
      (org-mode))
    (display-buffer buffer)))

;;;###autoload
(defun scientific-mapping-citation-network (max-papers)
  "Open citation network visualization for up to MAX-PAPERS."
  (interactive "nMaximum papers to display (500): ")
  (scientific-visualizer-set-mode "citation")
  (scientific-visualizer-open)
  (message "Citation network visualization opened."))

;;;###autoload
(defun scientific-mapping-concept-network (max-concepts)
  "Open concept network visualization for up to MAX-CONCEPTS."
  (interactive "nMaximum concepts to display (100): ")
  (scientific-visualizer-set-mode "concept")
  (scientific-visualizer-open)
  (message "Concept network visualization opened."))

;;;; Integration Utilities

(defun scientific-mapping-sync-all ()
  "Synchronize all databases and caches."
  (interactive)
  (message "Synchronizing scientific-mapping system...")
  (citation-database-sync)
  (concept-relationships-refresh-cache)
  (message "Synchronization complete."))

(defun scientific-mapping-backup ()
  "Create backup of all scientific-mapping data."
  (interactive)
  (let* ((backup-dir (expand-file-name
                      (format "scientific-mapping-backup-%s"
                              (format-time-string "%Y%m%d-%H%M%S"))
                      user-emacs-directory))
         (doc-backup (expand-file-name "documents/" backup-dir))
         (db-backup (expand-file-name "citation-database.sqlite" backup-dir))
         (concept-backup (expand-file-name "concepts/" backup-dir)))
    (make-directory doc-backup t)
    (make-directory db-backup t)
    (make-directory concept-backup t)
    
     ;; Backup documents
     (dolist (file (doc-engine-all-files))
       (copy-file file (expand-file-name (file-name-nondirectory file) doc-backup)))

     ;; Backup database
     (copy-file citation-database-location db-backup)

     ;; Backup concepts
     (dolist (file (concept-relationships-files))
       (copy-file file (expand-file-name (file-name-nondirectory file) concept-backup)))
    
    (message "Backup created: %s" backup-dir)))

;;;; Key Bindings

(defvar scientific-mapping-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'scientific-mapping-start)
    (define-key map "S" 'scientific-mapping-stop)
    (define-key map "i" 'scientific-mapping-import-paper)
    (define-key map "c" 'concept-relationships-create-entry)
    (define-key map "v" 'viz-engine-open)
    (define-key map "n" 'viz-engine-set-mode)
    (define-key map "r" 'scientific-mapping-literature-review)
    (define-key map "b" 'scientific-mapping-backup)
    (define-key map "t" 'timeline-engine-open)
    (define-key map "e" 'concept-tree-toggle-section)
    (define-key map "?" 'scientific-mapping-help)
    map)
  "Prefix keymap for scientific-mapping commands.")

(defvar-keymap scientific-mapping-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s") scientific-mapping-prefix-map)
    map)
  "Keymap for scientific-mapping-mode.")

(provide 'scientific-mapping)

;;; scientific-mapping.el ends here

;;; Usage:
;; 
;; To enable the system:
;; (scientific-mapping-mode 1)
;;
;; Or use the interactive command:
;; M-x scientific-mapping-start
;;
;; Key bindings (when in scientific-mapping-mode):
;; C-c s s - Start system
;; C-c s S - Stop system
;; C-c s i - Import paper
;; C-c s c - Create concept
;; C-c s v - Open visualizer
;; C-c s n - Set visualization mode
;; C-c s r - Generate literature review
;; C-c s b - Create backup
;; C-c s ? - Show help