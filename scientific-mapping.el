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
;; 3. viz-engine - 3D visualization of knowledge networks
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

;; Load org-agenda for research task management
(require 'org-agenda)

;; Load AI integration (optional)
(require 'ai-integration nil t)

;; Load academic APIs (optional)
(require 'academic-apis nil t)

;; Load reference managers (optional)
(require 'reference-managers nil t)

;; Load centralized key bindings
(require 'keybindings)

;; Load workflow integration
(require 'workflow-integration)

;; Load data synchronization
(require 'data-synchronization)

;; Load context awareness
(require 'context-awareness)

;; Load UX enhancements
(require 'ux-enhancements)

;; Load tutorial system
(require 'tutorial-system)

;; Load startup automation
(require 'startup-automation)

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
   '(doc-engine citation-database viz-engine concept-relationships timeline-engine concept-tree yas agenda)
   "List of components to load and enable."
   :group 'scientific-mapping
   :type '(set (const :tag "Document Engine" doc-engine)
                    (const :tag "Citation Database" citation-database)
                    (const :tag "Visualization Engine" viz-engine)
                    (const :tag "Concept Relationships" concept-relationships)
                    (const :tag "Timeline Engine" timeline-engine)
                    (const :tag "Concept Tree" concept-tree)
                    (const :tag "YASnippet Templates" yas)
                    (const :tag "Org Agenda Integration" agenda)))

(defcustom scientific-mapping-agenda-files
   (list (expand-file-name "research-agenda.org" user-emacs-directory))
   "List of Org files to include in scientific-mapping agenda."
   :group 'scientific-mapping
   :type '(repeat file))

(defcustom scientific-mapping-agenda-tags
   '("research" "paper" "review" "experiment" "analysis" "writing" "conference" "deadline")
   "Tags to use for scientific research tasks."
   :group 'scientific-mapping
   :type '(repeat string))

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

     (when (memq 'agenda scientific-mapping-components)
       (scientific-mapping-setup-agenda)
       (message "Setting up org-agenda integration..."))

     (message "Scientific Knowledge Mapping System enabled. Use M-x scientific-mapping-help for commands."))
   (t
    ;; Disable all components
    (citation-database-autosync-mode -1)
    (viz-engine-mode -1)
    (message "Scientific Knowledge Mapping System disabled."))))

;;;; Org Agenda Integration

(defun scientific-mapping-setup-agenda ()
  "Setup org-agenda integration for scientific research."
  (interactive)
  ;; Add scientific-mapping agenda files to org-agenda-files
  (dolist (file scientific-mapping-agenda-files)
    (unless (member file org-agenda-files)
      (add-to-list 'org-agenda-files file)))

  ;; Create default agenda file if it doesn't exist
  (let ((agenda-file (car scientific-mapping-agenda-files)))
    (unless (file-exists-p agenda-file)
      (with-temp-file agenda-file
        (insert "#+TITLE: Scientific Research Agenda
#+AUTHOR: Research Team
#+EMAIL: research@example.com

* Research Projects
** TODO Literature Review: Machine Learning in Scientific Discovery
   SCHEDULED: <2025-01-15 Tue>
   :PROPERTIES:
   :EFFORT: 2d
   :END:
   - Review recent papers on ML applications in scientific research
   - Identify key methodologies and gaps
   - Create summary document

** TODO Paper Writing: Knowledge Mapping Framework
   DEADLINE: <2025-02-28 Fri>
   :PROPERTIES:
   :EFFORT: 5d
   :END:
   - Write introduction section
   - Develop methodology
   - Implement evaluation framework

* Conference Deadlines
** TODO Submit to ICML 2025
   DEADLINE: <2025-01-20 Mon>
   :PROPERTIES:
   :CATEGORY: Conference
   :END:

* Regular Tasks
** TODO Weekly Literature Review
   SCHEDULED: <2025-01-06 Mon +1w>
   :PROPERTIES:
   :STYLE: habit
   :END:

** TODO Database Maintenance
   SCHEDULED: <2025-01-10 Fri +2w>
   :PROPERTIES:
   :EFFORT: 4h
   :END:
"))))

  ;; Setup custom agenda commands for scientific research
  (scientific-mapping-setup-agenda-commands))

(defun scientific-mapping-setup-agenda-commands ()
  "Setup custom org-agenda commands for scientific research."
  (setq org-agenda-custom-commands
        (append org-agenda-custom-commands
                '(("r" "Research Agenda"
                   ((agenda "" ((org-agenda-span 'week)
                               (org-agenda-start-on-weekday 1)))
                    (todo "TODO" ((org-agenda-files scientific-mapping-agenda-files)
                                 (org-agenda-prefix-format " %i %-12:c")
                                 (org-agenda-sorting-strategy '(priority-down effort-up))))
                    (todo "WAITING" ((org-agenda-files scientific-mapping-agenda-files)))
                    (todo "DONE" ((org-agenda-files scientific-mapping-agenda-files)))))
                  ("p" "Paper Deadlines"
                   ((agenda "" ((org-agenda-span 'month)))
                    (todo "TODO" ((org-agenda-files scientific-mapping-agenda-files)
                                 (org-agenda-filter-preset '("+paper+deadline"))))))
                  ("c" "Conference Deadlines"
                   ((agenda "" ((org-agenda-span 'month)))
                    (todo "TODO" ((org-agenda-files scientific-mapping-agenda-files)
                                 (org-agenda-filter-preset '("+conference"))))))
                  ("w" "Weekly Review"
                   ((agenda "" ((org-agenda-span 'week)))
                    (todo "DONE" ((org-agenda-span 'week)
                                 (org-agenda-files scientific-mapping-agenda-files)))))))))

;;;###autoload
(defun scientific-mapping-agenda ()
  "Open the scientific research agenda."
  (interactive)
  (scientific-mapping-setup-agenda)
  (org-agenda nil "r"))

;;;###autoload
(defun scientific-mapping-schedule-paper-review (doi)
  "Schedule a paper for review in the agenda."
  (interactive
   (list (read-string "DOI of paper to review: "
                      (when (and (eq major-mode 'org-mode)
                               (org-entry-get nil "DOI"))
                        (org-entry-get nil "DOI")))))
  (let* ((paper-info (citation-database-get-paper-by-doi doi))
         (title (or (elt paper-info 2) "Unknown Paper"))
         (agenda-file (car scientific-mapping-agenda-files)))

    ;; Ensure agenda file exists
    (scientific-mapping-setup-agenda)

    ;; Add to agenda file
    (with-current-buffer (find-file-noselect agenda-file)
      (goto-char (point-max))
      (insert (format "\n** TODO Review: %s
   SCHEDULED: %s
   :PROPERTIES:
   :DOI: %s
   :EFFORT: 2h
   :END:
   - Read abstract and introduction
   - Identify key contributions
   - Note relevant citations
   - Add to knowledge map if significant\n"
                     title
                     (format-time-string "<%Y-%m-%d %a>" (time-add (current-time) (* 7 24 3600)))
                     doi)))
    (message "Paper review scheduled for %s" title)))

;;;###autoload
(defun scientific-mapping-schedule-deadline (task deadline)
  "Schedule a research task with a deadline."
  (interactive
   (list (read-string "Task description: ")
         (org-read-date nil t nil "Deadline: ")))
  (let ((agenda-file (car scientific-mapping-agenda-files)))

    ;; Ensure agenda file exists
    (scientific-mapping-setup-agenda)

    ;; Add to agenda file
    (with-current-buffer (find-file-noselect agenda-file)
      (goto-char (point-max))
      (insert (format "\n** TODO %s
   DEADLINE: %s
   :PROPERTIES:
   :CATEGORY: Research
   :END:\n"
                     task
                     (format-time-string "<%Y-%m-%d %a>" deadline))))
    (message "Task scheduled: %s (deadline: %s)"
             task
             (format-time-string "%Y-%m-%d" deadline))))

;;;###autoload
(defun scientific-mapping-create-research-project (name description)
  "Create a new research project in the agenda."
  (interactive
   (list (read-string "Project name: ")
         (read-string "Project description: ")))
  (let ((agenda-file (car scientific-mapping-agenda-files)))

    ;; Ensure agenda file exists
    (scientific-mapping-setup-agenda)

    ;; Add project to agenda file
    (with-current-buffer (find-file-noselect agenda-file)
      (goto-char (point-max))
      (insert (format "\n* %s
%s

** TODO Project Planning
   SCHEDULED: %s
   :PROPERTIES:
   :EFFORT: 1d
   :END:
   - Define research questions
   - Identify methodology
   - Set milestones and deliverables

** TODO Literature Review
   SCHEDULED: %s
   :PROPERTIES:
   :EFFORT: 3d
   :END:
   - Conduct comprehensive literature search
   - Analyze existing work
   - Identify research gaps

** TODO Implementation
   SCHEDULED: %s
   :PROPERTIES:
   :EFFORT: 10d
   :END:
   - Develop research methodology
   - Implement solution
   - Collect and analyze data

** TODO Writing and Publication
   SCHEDULED: %s
   :PROPERTIES:
   :EFFORT: 7d
   :END:
   - Write research paper
   - Create presentations
   - Submit to conferences/journals\n"
                     name
                     (if description (concat description "\n\n") "")
                     (format-time-string "<%Y-%m-%d %a>" (time-add (current-time) (* 1 24 3600)))
                     (format-time-string "<%Y-%m-%d %a>" (time-add (current-time) (* 7 24 3600)))
                     (format-time-string "<%Y-%m-%d %a>" (time-add (current-time) (* 14 24 3600)))
                     (format-time-string "<%Y-%m-%d %a>" (time-add (current-time) (* 60 24 3600))))))
    (message "Research project created: %s" name)))

;;;###autoload
(defun scientific-mapping-weekly-review ()
  "Generate a weekly research progress report."
  (interactive)
  (let ((buffer (get-buffer-create "*Weekly Research Review*"))
        (start-date (time-subtract (current-time) (* 7 24 3600)))
        (end-date (current-time)))

    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "= Weekly Research Review: %s - %s =\n\n"
                      (format-time-string "%Y-%m-%d" start-date)
                      (format-time-string "%Y-%m-%d" end-date)))

      ;; Papers reviewed this week
      (insert "* Papers Reviewed\n")
      (let ((papers-reviewed 0))
        (dolist (file (doc-engine-all-files))
          (let ((file-time (nth 5 (file-attributes file))))
            (when (and file-time
                      (time-less-p start-date file-time)
                      (time-less-p file-time end-date))
              (insert (format "** %s\n" (file-name-base file)))
              (setq papers-reviewed (1+ papers-reviewed)))))
        (insert (format "Total: %d papers\n\n" papers-reviewed)))

      ;; Agenda items completed
      (insert "* Tasks Completed\n")
      (let ((completed-tasks 0))
        (dolist (file scientific-mapping-agenda-files)
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (org-map-entries
               (lambda ()
                 (let ((todo-state (org-get-todo-state))
                       (closed-time (org-entry-get nil "CLOSED")))
                   (when (and (string= todo-state "DONE")
                             closed-time
                             (time-less-p start-date (date-to-time closed-time))
                             (time-less-p (date-to-time closed-time) end-date))
                     (insert (format "** DONE %s\n" (org-get-heading t t)))
                     (setq completed-tasks (1+ completed-tasks)))))
               nil 'file))))
        (insert (format "Total: %d tasks\n\n" completed-tasks)))

      ;; Upcoming deadlines
      (insert "* Upcoming Deadlines\n")
      (let ((upcoming-deadlines 0))
        (dolist (file scientific-mapping-agenda-files)
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (org-map-entries
               (lambda ()
                 (let ((deadline (org-entry-get nil "DEADLINE"))
                       (todo-state (org-get-todo-state)))
                   (when (and deadline
                             (not (string= todo-state "DONE"))
                             (time-less-p (current-time) (date-to-time deadline)))
                     (insert (format "** %s - %s\n" deadline (org-get-heading t t)))
                     (setq upcoming-deadlines (1+ upcoming-deadlines)))))
               nil 'file))))
        (insert (format "Total: %d upcoming deadlines\n\n" upcoming-deadlines)))

      (goto-char (point-min))
      (org-mode))
    (display-buffer buffer)))

;;;###autoload
(defun scientific-mapping-agenda-from-document ()
  "Create agenda items from the current scientific document."
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (let* ((title (or (org-get-title) (file-name-base (buffer-file-name))))
           (doi (org-entry-get nil "DOI"))
           (keywords (org-entry-get nil "SCIENTIFIC_CONCEPTS"))
           (agenda-file (car scientific-mapping-agenda-files)))

      ;; Ensure agenda file exists
      (scientific-mapping-setup-agenda)

      ;; Create agenda entry
      (with-current-buffer (find-file-noselect agenda-file)
        (goto-char (point-max))
        (insert (format "\n** TODO Process Document: %s
   SCHEDULED: %s
   :PROPERTIES:
   :FILE: %s
   :DOI: %s
   :KEYWORDS: %s
   :EFFORT: 1h
   :END:
   - Extract key concepts and relationships
   - Add citations to database
   - Link to existing knowledge map
   - Identify follow-up research questions\n"
                       title
                       (format-time-string "<%Y-%m-%d %a>" (time-add (current-time) (* 1 24 3600)))
                       (buffer-file-name)
                       (or doi "")
                       (or keywords ""))))
      (message "Agenda item created for document: %s" title))))

;;;; Interactive Commands

;;;###autoload
(defun scientific-mapping-start ()
  "Start the scientific knowledge mapping system."
  (interactive)
  (scientific-mapping-mode 1)
  (when (y-or-n-p "Open visualizer interface? ")
    (viz-engine-open))
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

;; Key bindings are now centralized in keybindings.el
;; See keybindings.el for all key binding definitions

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
;; See KEYBINDINGS.md for complete command reference and key bindings.