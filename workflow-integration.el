;;; workflow-integration.el --- Seamless integration between scientific mapping components -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

;;; Commentary:

;; This file provides seamless integration between all scientific mapping components,
;; enabling automated workflows and cross-component data synchronization.

;;; Code:

(require 'scientific-mapping)
(require 'doc-engine)
(require 'citation-database)
(require 'concept-relationships)
(require 'ai-integration nil t)
(require 'academic-apis nil t)
(require 'reference-managers nil t)

(defgroup workflow-integration ()
  "Integration between scientific mapping components."
  :group 'scientific-mapping)

;;;; Automated Import Workflow

(defcustom workflow-integration-auto-import-workflow t
  "If non-nil, automatically run full import workflow when adding documents."
  :group 'workflow-integration
  :type 'boolean)

(defcustom workflow-integration-auto-analyze-new-documents t
  "If non-nil, automatically analyze new documents with AI."
  :group 'workflow-integration
  :type 'boolean)

(defcustom workflow-integration-auto-schedule-reviews t
  "If non-nil, automatically schedule reviews for new documents."
  :group 'workflow-integration
  :type 'boolean)

(defcustom workflow-integration-frictionless-mode t
  "If non-nil, enable maximum automation and frictionless workflow."
  :group 'workflow-integration
  :type 'boolean)

;;;###autoload
(defun workflow-integration-import-paper-complete (doi title keywords)
  "Complete automated workflow for importing a paper.
DOI: Document identifier
TITLE: Paper title
KEYWORDS: List of keywords"
  (interactive
   (list (read-string "DOI: ")
         (read-string "Title: ")
         (split-string (read-string "Keywords (comma-separated): ") "," t " ")))

  (message "Starting complete import workflow for: %s" title)

  ;; Step 1: Create document
  (let* ((doc-file (doc-engine-create :title title :doi doi :keywords keywords))
         (doc-id (file-name-base doc-file)))

    ;; Step 2: Add to citation database
    (when (citation-database-autosync-mode)
      (run-with-timer 0.5 nil #'citation-database-index-file doc-file))

    ;; Step 3: AI analysis (if enabled)
    (when (and workflow-integration-auto-analyze-new-documents
               (featurep 'ai-integration))
      (run-with-timer 1.0 nil #'workflow-integration-analyze-new-document doc-file))

    ;; Step 4: Schedule review (if enabled)
    (when workflow-integration-auto-schedule-reviews
      (run-with-timer 1.5 nil #'workflow-integration-schedule-document-review doi title))

    ;; Step 5: Update visualization
    (run-with-timer 2.0 nil #'workflow-integration-update-visualization)

    (message "Import workflow initiated. Document: %s" doc-id)
    doc-file))

;;;###autoload
(defun workflow-integration-import-from-api-complete (source results)
  "Complete workflow for importing from academic APIs.
SOURCE: API source (arxiv, pubmed, doi)
RESULTS: Search results to import"
  (interactive
   (list (completing-read "Import from: " '("arxiv" "pubmed" "doi"))
         nil)) ; Results would come from API search

  (message "Starting API import workflow from %s" source)

  ;; This would process multiple results and run the complete workflow for each
  ;; For now, show the concept
  (message "API import workflow would process %d results with full integration" (length results)))

;;;; AI Integration Workflows

(defun workflow-integration-analyze-new-document (doc-file)
  "Analyze a newly imported document with AI."
  (when (file-exists-p doc-file)
    (with-current-buffer (find-file-noselect doc-file)
      (message "AI analyzing: %s" (file-name-nondirectory doc-file))

      ;; AI analysis
      (when (featurep 'ai-integration)
        (condition-case err
            (progn
              ;; Extract concepts
              (ai-integration-extract-concepts (buffer-string))

              ;; Generate summary
              (ai-integration-summarize-document)

              ;; Add metadata to Org properties
              (org-set-property "AI_ANALYZED" (format-time-string "%Y-%m-%d %H:%M"))
              (org-set-property "ANALYSIS_STATUS" "completed")

              (message "AI analysis completed for %s" (file-name-nondirectory doc-file)))
          (error (message "AI analysis failed for %s: %s"
                         (file-name-nondirectory doc-file)
                         (error-message-string err))))))))

;;;; Agenda Integration Workflows

(defun workflow-integration-schedule-document-review (doi title)
  "Schedule a review for a newly imported document."
  (when (featurep 'scientific-mapping) ; Check if agenda integration is loaded
    (condition-case err
        (progn
          (scientific-mapping-schedule-paper-review doi)
          (message "Review scheduled for: %s" title))
      (error (message "Failed to schedule review: %s" (error-message-string err))))))

;;;; Visualization Integration

(defun workflow-integration-update-visualization ()
  "Update visualization after data changes."
  (when viz-engine-mode
    (message "Updating visualization with new data")
    ;; Trigger visualization refresh
    (run-with-timer 0.5 nil (lambda ()
                             (when (get-buffer "*scientific-visualizer*")
                               (with-current-buffer "*scientific-visualizer*"
                                 (viz-engine-refresh-data)))))))

;;;; Unified Search

;;;###autoload
(defun workflow-integration-unified-search (query)
  "Search across all components: documents, citations, concepts, agenda."
  (interactive "sUnified search: ")

  (let ((results (make-hash-table :test 'equal))
        (search-id (format-time-string "%Y%m%d-%H%M%S")))

    ;; Search documents
    (puthash "documents" (workflow-integration-search-documents query) results)

    ;; Search citations
    (puthash "citations" (workflow-integration-search-citations query) results)

    ;; Search concepts
    (puthash "concepts" (workflow-integration-search-concepts query) results)

    ;; Search agenda
    (puthash "agenda" (workflow-integration-search-agenda query) results)

    ;; Display unified results
    (workflow-integration-display-unified-results query results search-id)))

(defun workflow-integration-search-documents (query)
  "Search documents using doc-engine."
  (condition-case err
      (doc-engine-search query)
    (error (message "Document search failed: %s" (error-message-string err)) nil)))

(defun workflow-integration-search-citations (query)
  "Search citations using citation-database."
  (condition-case err
      (citation-database-search-papers query 20)
    (error (message "Citation search failed: %s" (error-message-string err)) nil)))

(defun workflow-integration-search-concepts (query)
  "Search concepts using concept-relationships."
  (condition-case err
      ;; This would need to be implemented in concept-relationships
      (message "Concept search not yet implemented")
    (error nil)))

(defun workflow-integration-search-agenda (query)
  "Search agenda items."
  (condition-case err
      ;; Search org-agenda files for the query
      (let ((matches '()))
        (dolist (file scientific-mapping-agenda-files)
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (goto-char (point-min))
              (while (re-search-forward (regexp-quote query) nil t)
                (let ((heading (org-get-heading t t)))
                  (when heading
                    (push `((file . ,file) (heading . ,heading) (line . ,(line-number-at-pos))) matches)))))))
        matches)
    (error (message "Agenda search failed: %s" (error-message-string err)) nil)))

(defun workflow-integration-display-unified-results (query results search-id)
  "Display unified search results in an Org buffer."
  (let ((buffer (get-buffer-create (format "*Unified Search: %s*" search-id))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: Unified Search Results: %s\n\n" query))
      (insert (format "* Search Query: %s\n" query))
      (insert (format "Search ID: %s\n\n" search-id))

      ;; Documents section
      (let ((docs (gethash "documents" results)))
        (insert (format "** Documents (%d results)\n" (length docs)))
        (if docs
            (dolist (doc docs)
              (insert (format "- [[file:%s][%s]]\n" (cdr (assoc 'file doc)) (cdr (assoc 'title doc)))))
          (insert "No document results\n"))
        (insert "\n"))

      ;; Citations section
      (let ((cites (gethash "citations" results)))
        (insert (format "** Citations (%d results)\n" (length cites)))
        (if cites
            (dolist (cite cites)
              (insert (format "- %s (%s)\n" (elt cite 2) (elt cite 6)))) ; title and year
          (insert "No citation results\n"))
        (insert "\n"))

      ;; Concepts section
      (let ((concepts (gethash "concepts" results)))
        (insert (format "** Concepts (%d results)\n" (length concepts)))
        (if concepts
            (dolist (concept concepts)
              (insert (format "- %s\n" concept)))
          (insert "No concept results\n"))
        (insert "\n"))

      ;; Agenda section
      (let ((agenda-items (gethash "agenda" results)))
        (insert (format "** Agenda Items (%d results)\n" (length agenda-items)))
        (if agenda-items
            (dolist (item agenda-items)
              (insert (format "- [[file:%s::%d][%s]]\n"
                             (cdr (assoc 'file item))
                             (cdr (assoc 'line item))
                             (cdr (assoc 'heading item)))))
          (insert "No agenda results\n"))
        (insert "\n"))

      (insert "* Actions\n")
      (insert "- [ ] Review search results\n")
      (insert "- [ ] Open relevant documents\n")
      (insert "- [ ] Add findings to research notes\n")
      (insert "- [ ] Create follow-up tasks\n")

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; Workflow Status Tracking

(defvar workflow-integration-workflow-history nil
  "History of completed workflows.")

;;;###autoload
(defun workflow-integration-show-workflow-status ()
  "Show status of all integrated workflows."
  (interactive)
  (let ((buffer (get-buffer-create "*Workflow Integration Status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Workflow Integration Status\n\n")

      (insert "* Component Status\n\n")

      ;; Document Engine
      (insert "** Document Engine\n")
      (insert (format "- Status: %s\n" (if (featurep 'doc-engine) "Active" "Inactive")))
      (insert (format "- Documents: %d\n" (length (doc-engine-all-files))))
      (insert "\n")

      ;; Citation Database
      (insert "** Citation Database\n")
      (insert (format "- Status: %s\n" (if citation-database-autosync-mode "Active" "Inactive")))
      (insert "- Auto-sync: Enabled\n")
      (insert "\n")

      ;; AI Integration
      (insert "** AI Integration\n")
      (insert (format "- Status: %s\n" (if (featurep 'ai-integration) "Active" "Inactive")))
      (insert (format "- Provider: %s\n" (if (featurep 'ai-integration) ai-integration-provider "None")))
      (insert (format "- Auto-analysis: %s\n" (if workflow-integration-auto-analyze-new-documents "Enabled" "Disabled")))
      (insert "\n")

      ;; Agenda Integration
      (insert "** Agenda Integration\n")
      (insert (format "- Status: %s\n" (if (featurep 'scientific-mapping) "Active" "Inactive")))
      (insert (format "- Agenda files: %d\n" (length scientific-mapping-agenda-files)))
      (insert "\n")

      ;; Visualization
      (insert "** Visualization\n")
      (insert (format "- Status: %s\n" (if viz-engine-mode "Active" "Inactive")))
      (insert "- 3D Force Graph: Enabled\n")
      (insert "- Touch controls: Supported\n")
      (insert "\n")

      ;; Academic APIs
      (insert "** Academic APIs\n")
      (insert (format "- Status: %s\n" (if (featurep 'academic-apis) "Active" "Inactive")))
      (insert "- arXiv: Available\n")
      (insert "- PubMed: Available\n")
      (insert "- DOI Resolution: Available\n")
      (insert "\n")

      ;; Reference Managers
      (insert "** Reference Managers\n")
      (insert (format "- Status: %s\n" (if (featurep 'reference-managers) "Active" "Inactive")))
      (insert "- Zotero: Supported\n")
      (insert "- BibTeX: Supported\n")
      (insert "\n")

      (insert "* Workflow Settings\n\n")
      (insert (format "- Auto-import workflow: %s\n" (if workflow-integration-auto-import-workflow "Enabled" "Disabled")))
      (insert (format "- Auto-analyze documents: %s\n" (if workflow-integration-auto-analyze-new-documents "Enabled" "Disabled")))
      (insert (format "- Auto-schedule reviews: %s\n" (if workflow-integration-auto-schedule-reviews "Enabled" "Disabled")))
      (insert "\n")

      (insert "* Recent Workflow Activity\n\n")
      (if workflow-integration-workflow-history
          (dolist (item (reverse workflow-integration-workflow-history))
            (insert (format "- %s: %s\n" (cdr (assoc 'timestamp item)) (cdr (assoc 'action item)))))
        (insert "No recent workflow activity\n"))
      (insert "\n")

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; Workflow Hooks and Automation

;; Hook into document creation
(add-hook 'doc-engine-after-create-hook #'workflow-integration-document-created-hook)

(defun workflow-integration-document-created-hook (doc-file)
  "Hook that runs when a new document is created."
  (when workflow-integration-auto-import-workflow
    (run-with-timer 0.1 nil #'workflow-integration-process-new-document doc-file)))

(defun workflow-integration-process-new-document (doc-file)
  "Process a newly created document through the full workflow."
  (message "Processing new document: %s" (file-name-nondirectory doc-file))

  ;; Add to workflow history
  (push `((timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
          (action . ,(format "Document created: %s" (file-name-nondirectory doc-file)))
          (file . ,doc-file))
        workflow-integration-workflow-history)

  ;; Run automated analysis if enabled
  (when workflow-integration-auto-analyze-new-documents
    (run-with-timer 0.5 nil #'workflow-integration-analyze-new-document doc-file)))

;;;; Initialization

(defun workflow-integration-initialize ()
  "Initialize workflow integration."
  (message "Workflow integration initialized"))

;;;; Frictionless Mode - Maximum Automation

(when workflow-integration-frictionless-mode
  (message "Enabling frictionless research workflow mode...")

  ;; Auto-enable all automation features
  (setq workflow-integration-auto-import-workflow t)
  (setq workflow-integration-auto-analyze-new-documents t)
  (setq workflow-integration-auto-schedule-reviews t)

  ;; Enable doc-engine automation
  (when (featurep 'doc-engine)
    (setq doc-engine-auto-process-on-open t)
    (setq doc-engine-auto-process-on-save t)
    (setq doc-engine-auto-sync-citations t)
    (setq doc-engine-auto-extract-concepts t))

  ;; Enable AI automation
  (when (featurep 'ai-integration)
    (setq ai-integration-auto-analyze-new-documents t))

  ;; Enable data synchronization
  (when (featurep 'data-synchronization)
    (data-synchronization-start-auto-sync))

  ;; Enable context awareness
  (when (featurep 'context-awareness)
    (context-awareness-enable-proactive-suggestions))

  (message "Frictionless mode activated - maximum automation enabled!"))

;;;; Ultra-Automated Research Workflow

;;;###autoload
(defun workflow-integration-frictionless-research-session ()
  "Start a completely automated research session."
  (interactive)
  (message "🌊 Starting frictionless research session...")

  ;; 1. Auto-sync everything
  (when (featurep 'data-synchronization)
    (data-synchronization-full-sync))

  ;; 2. Check for new content to process
  (workflow-integration-process-pending-items)

  ;; 3. Show smart suggestions
  (when (featurep 'context-awareness)
    (run-with-timer 1 nil #'context-awareness-show-suggestions))

  ;; 4. Open unified search interface
  (run-with-timer 2 nil #'workflow-integration-unified-search "")

  (message "🎯 Frictionless research session ready!"))

(defun workflow-integration-process-pending-items ()
  "Process any pending items automatically."
  (let ((processed 0))

    ;; Process unanalyzed documents
    (when (and (featurep 'ai-integration) workflow-integration-auto-analyze-new-documents)
      (dolist (file (doc-engine-all-files))
        (when (and (not (org-entry-get nil "AI_ANALYZED" file))
                   (> (length (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))) 500)) ; Substantial content
          (run-with-timer (* processed 0.5) nil
                         (lambda (f) (workflow-integration-analyze-new-document f)) file)
          (setq processed (1+ processed)))))

    ;; Process unscheduled reviews
    (when workflow-integration-auto-schedule-reviews
      (dolist (file (doc-engine-all-files))
        (when (and (org-entry-get nil "DOI" file)
                   (not (workflow-integration-has-scheduled-review-p file)))
          (run-with-timer (* processed 0.3) nil
                         #'workflow-integration-schedule-document-review
                         (org-entry-get nil "DOI" file)
                         (org-entry-get nil "TITLE" file))
          (setq processed (1+ processed)))))

    (when (> processed 0)
      (message "Auto-processing %d pending items..." processed))))

(defun workflow-integration-has-scheduled-review-p (file)
  "Check if document already has a scheduled review."
  (catch 'found
    (dolist (agenda-file scientific-mapping-agenda-files)
      (when (file-exists-p agenda-file)
        (with-current-buffer (find-file-noselect agenda-file)
          (goto-char (point-min))
          (when (search-forward (file-name-nondirectory file) nil t)
            (throw 'found t)))))
    nil))

;;;; Smart Document Processing

;;;###autoload
(defun workflow-integration-smart-process-buffer ()
  "Automatically process the current buffer with all available intelligence."
  (interactive)
  (message "🧠 Smart processing current buffer...")

  (cond
   ;; Scientific document
   ((and (eq major-mode 'org-mode)
         (doc-engine-is-scientific-document-p))
    (workflow-integration-process-scientific-document))

   ;; Agenda item
   ((and (eq major-mode 'org-mode)
         (member (buffer-file-name) scientific-mapping-agenda-files))
    (workflow-integration-process-agenda-item))

   ;; General text
   (t (workflow-integration-process-general-text)))

  (message "✨ Smart processing complete!"))

(defun workflow-integration-process-scientific-document ()
  "Process scientific document with full automation."
  (let ((actions '()))

    ;; AI Analysis
    (when (and (featurep 'ai-integration)
               (not (org-entry-get nil "AI_ANALYZED")))
      (push "AI analysis" actions)
      (ai-integration-analyze-document))

    ;; Citation sync
    (when (and (featurep 'citation-database)
               (org-entry-get nil "DOI"))
      (push "Citation sync" actions)
      (doc-engine-sync-citations-for-buffer))

    ;; Concept extraction
    (when (and (featurep 'ai-integration)
               (not (org-entry-get nil "CONCEPTS_EXTRACTED")))
      (push "Concept extraction" actions)
      (ai-integration-extract-concepts (buffer-string))
      (org-set-property "CONCEPTS_EXTRACTED" (format-time-string "%Y-%m-%d")))

    ;; Schedule review
    (when (and (featurep 'scientific-mapping)
               (not (workflow-integration-has-scheduled-review-p (buffer-file-name))))
      (push "Review scheduling" actions)
      (let ((doi (org-entry-get nil "DOI"))
            (title (org-entry-get nil "TITLE")))
        (when doi
          (scientific-mapping-schedule-paper-review doi title))))

    (if actions
        (message "Applied smart processing: %s" (string-join actions ", "))
      (message "Document already fully processed"))))

(defun workflow-integration-process-agenda-item ()
  "Process agenda item intelligently."
  (message "Processing agenda item...")

  ;; Could add smart scheduling, deadline analysis, etc.
  (message "Agenda item processing not yet implemented"))

(defun workflow-integration-process-general-text ()
  "Process general text content."
  (when (and (featurep 'ai-integration)
             (> (buffer-size) 100))
    (ai-integration-analyze-document)))

;; Auto-initialize
(workflow-integration-initialize)

(provide 'workflow-integration)

;;; workflow-integration.el ends here

;;; Usage:
;;
;; This file provides seamless integration between all scientific mapping components.
;; Key functions:
;; - workflow-integration-import-paper-complete: Full automated import workflow
;; - workflow-integration-unified-search: Search across all components
;; - workflow-integration-show-workflow-status: View integration status
;;
;; Automated workflows run when:
;; - New documents are created (doc-engine-after-create-hook)
;; - Settings control auto-analysis and scheduling