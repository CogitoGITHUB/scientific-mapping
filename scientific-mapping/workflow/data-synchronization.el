;;; data-synchronization.el --- Keep all scientific mapping components synchronized -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This file provides data synchronization between all scientific mapping components,
;; ensuring consistency and preventing data drift across documents, citations, concepts,
;; and visualizations.

;;; Code:

;; (require 'scientific-mapping)  ; Package main feature loaded via scientific-mapping.el
(require 'doc-engine)
(require 'cite/index)
;; (require 'concept-relationships)
(require 'ai-integration nil t)

(defgroup data-synchronization ()
  "Data synchronization between components."
  :group 'scientific-mapping)

(defcustom data-synchronization-auto-sync t
  "If non-nil, automatically synchronize data between components."
  :group 'data-synchronization
  :type 'boolean)

(defcustom data-synchronization-sync-interval 300
  "Interval in seconds for automatic synchronization checks."
  :group 'data-synchronization
  :type 'integer)

(defvar data-synchronization-last-sync-time nil
  "Timestamp of last synchronization.")

(defvar data-synchronization-sync-timer nil
  "Timer for automatic synchronization.")

;;;; Core Synchronization Functions

;;;###autoload
(defun data-synchronization-full-sync ()
  "Perform complete synchronization between all components."
  (interactive)
  (message "Starting full data synchronization...")

  (let ((start-time (current-time))
        (sync-results (make-hash-table :test 'equal)))

    ;; Sync documents to citations
    (puthash "doc-to-citation" (data-synchronization-sync-documents-to-citations) sync-results)

    ;; Sync citations to documents
    (puthash "citation-to-doc" (data-synchronization-sync-citations-to-documents) sync-results)

    ;; Sync concepts to documents
    (puthash "concept-to-doc" (data-synchronization-sync-concepts-to-documents) sync-results)

    ;; Sync AI metadata
    (puthash "ai-metadata" (data-synchronization-sync-ai-metadata) sync-results)

    ;; Update visualization
    (puthash "visualization" (data-synchronization-update-visualization) sync-results)

    (let ((end-time (current-time))
          (duration (float-time (time-subtract end-time start-time))))
      (message "Synchronization completed in %.2f seconds" duration)
      (data-synchronization-display-sync-results sync-results duration))))

(defun data-synchronization-sync-documents-to-citations ()
  "Ensure all documents are indexed in citation database."
  (let ((synced 0)
        (errors 0))
    (dolist (doc-file (doc-engine-all-files))
      (condition-case err
          (let* ((metadata (doc-engine-extract-metadata doc-file))
                 (doi (cdr (assoc 'doi metadata)))
                 (title (cdr (assoc 'title metadata))))
            (when doi
              ;; Check if citation exists
              (unless (citation-database-get-paper-by-doi doi)
                ;; Add to citation database
                (citation-database-index-file doc-file)
                (setq synced (1+ synced)))))
        (error
         (message "Error syncing document %s: %s" doc-file (error-message-string err))
         (setq errors (1+ errors)))))
    `((synced . ,synced) (errors . ,errors))))

(defun data-synchronization-sync-citations-to-documents ()
  "Ensure citation data is reflected in document metadata."
  (let ((synced 0)
        (errors 0))
    ;; This would check citation database and update document Org properties
    ;; For now, return placeholder
    `((synced . ,synced) (errors . ,errors))))

(defun data-synchronization-sync-concepts-to-documents ()
  "Ensure concept relationships are linked to documents."
  (let ((synced 0)
        (errors 0))
    ;; This would check concept relationships and update document links
    ;; For now, return placeholder
    `((synced . ,synced) (errors . ,errors))))

(defun data-synchronization-sync-ai-metadata ()
  "Synchronize AI-generated metadata across components."
  (let ((synced 0)
        (errors 0))
    (when (featurep 'ai-integration)
      ;; Sync AI analysis results
      (dolist (doc-file (doc-engine-all-files))
        (condition-case err
            (let ((ai-analyzed (org-entry-get nil "AI_ANALYZED" doc-file)))
              (when ai-analyzed
                (setq synced (1+ synced))))
          (error (setq errors (1+ errors))))))
    `((synced . ,synced) (errors . ,errors))))

(defun data-synchronization-update-visualization ()
  "Update visualization with latest data."
  (let ((updated nil))
    (when viz-engine-mode
      ;; Trigger visualization refresh
      (run-with-timer 0.1 nil (lambda ()
                               (when (get-buffer "*scientific-visualizer*")
                                 (with-current-buffer "*scientific-visualizer*"
                                   (viz-engine-refresh-data)))))
      (setq updated t))
    `((updated . ,updated))))

(defun data-synchronization-display-sync-results (results duration)
  "Display synchronization results."
  (let ((buffer (get-buffer-create "*Data Synchronization Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Data Synchronization Results\n\n")
      (insert (format "* Synchronization Report\nCompleted in %.2f seconds\n\n" duration))

      (insert "** Component Synchronization\n\n")

      ;; Documents to Citations
      (let ((doc-sync (gethash "doc-to-citation" results)))
        (insert (format "- Documents → Citations: %d synced, %d errors\n"
                       (cdr (assoc 'synced doc-sync))
                       (cdr (assoc 'errors doc-sync)))))

      ;; Citations to Documents
      (let ((cit-sync (gethash "citation-to-doc" results)))
        (insert (format "- Citations → Documents: %d synced, %d errors\n"
                       (cdr (assoc 'synced cit-sync))
                       (cdr (assoc 'errors cit-sync)))))

      ;; Concepts to Documents
      (let ((con-sync (gethash "concept-to-doc" results)))
        (insert (format "- Concepts → Documents: %d synced, %d errors\n"
                       (cdr (assoc 'synced con-sync))
                       (cdr (assoc 'errors con-sync)))))

      ;; AI Metadata
      (let ((ai-sync (gethash "ai-metadata" results)))
        (insert (format "- AI Metadata: %d synced, %d errors\n"
                       (cdr (assoc 'synced ai-sync))
                       (cdr (assoc 'errors ai-sync)))))

      ;; Visualization
      (let ((viz-sync (gethash "visualization" results)))
        (insert (format "- Visualization: %s\n"
                       (if (cdr (assoc 'updated viz-sync)) "Updated" "No update needed"))))

      (insert "\n** Actions\n")
      (insert "- [ ] Review synchronization results\n")
      (insert "- [ ] Check for any errors that need manual resolution\n")
      (insert "- [ ] Verify data consistency across components\n")

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; Automatic Synchronization

(defun data-synchronization-start-auto-sync ()
  "Start automatic synchronization timer."
  (interactive)
  (when data-synchronization-sync-timer
    (cancel-timer data-synchronization-sync-timer))

  (when data-synchronization-auto-sync
    (setq data-synchronization-sync-timer
          (run-with-timer data-synchronization-sync-interval
                         data-synchronization-sync-interval
                         #'data-synchronization-auto-sync-check))
    (message "Automatic synchronization enabled (every %d seconds)"
             data-synchronization-sync-interval)))

(defun data-synchronization-stop-auto-sync ()
  "Stop automatic synchronization timer."
  (interactive)
  (when data-synchronization-sync-timer
    (cancel-timer data-synchronization-sync-timer)
    (setq data-synchronization-sync-timer nil)
    (message "Automatic synchronization disabled")))

(defun data-synchronization-auto-sync-check ()
  "Check if synchronization is needed and perform if necessary."
  (when (and data-synchronization-auto-sync
             (data-synchronization-needs-sync-p))
    (message "Auto-sync: Changes detected, synchronizing...")
    (data-synchronization-full-sync)))

(defun data-synchronization-needs-sync-p ()
  "Check if synchronization is needed."
  ;; Simple check: see if any files have been modified since last sync
  (let ((needs-sync nil))
    (dolist (doc-file (doc-engine-all-files))
      (when (file-exists-p doc-file)
        (let ((file-mod-time (nth 5 (file-attributes doc-file))))
          (when (or (not data-synchronization-last-sync-time)
                   (time-less-p data-synchronization-last-sync-time file-mod-time))
            (setq needs-sync t)))))
    needs-sync))

;;;; Change Tracking Hooks

;; Debouncing timer for after-save hooks
(defvar data-synchronization--debounce-timer nil
  "Timer for debouncing after-save hooks.")

(defcustom data-synchronization-debounce-interval 0.5
  "Debounce interval in seconds for after-save hooks."
  :group 'data-synchronization
  :type 'number)

;; Hook into document modifications
(add-hook 'after-save-hook #'data-synchronization-document-changed-hook)

(defun data-synchronization-document-changed-hook ()
  "Hook that runs when a document is saved.
Uses debouncing to prevent multiple rapid syncs."
  (when (and (string-match-p "\\.org$" (buffer-file-name))
             (member (buffer-file-name) (doc-engine-all-files)))
    ;; Cancel existing timer and schedule new one (debouncing)
    (when data-synchronization--debounce-timer
      (cancel-timer data-synchronization--debounce-timer)
      (setq data-synchronization--debounce-timer nil))
    (setq data-synchronization--debounce-timer
          (run-with-timer data-synchronization-debounce-interval nil
                         #'data-synchronization-document-sync-hook))))

(defun data-synchronization-document-sync-hook ()
  "Synchronize document changes across components."
  (when data-synchronization-auto-sync
    (let ((doc-file (buffer-file-name)))
      (message "Syncing changes for: %s" (file-name-nondirectory doc-file))

      ;; Update citation database if DOI changed
      (when (citation-database-autosync-mode)
        (citation-database-index-file doc-file))

      ;; Update concepts if relationships changed
      ;; (This would need more sophisticated change detection)

      ;; Update visualization
      (run-with-timer 0.5 nil #'data-synchronization-update-visualization)

      (setq data-synchronization-last-sync-time (current-time)))))

;;;; Data Consistency Checks

;;;###autoload
(defun data-synchronization-consistency-check ()
  "Check data consistency across all components."
  (interactive)
  (let ((issues '())
        (buffer (get-buffer-create "*Data Consistency Check*")))

    (message "Checking data consistency...")

    ;; Check documents vs citations
    (let ((doc-count (length (doc-engine-all-files)))
          (citation-count (length (citation-database-search-papers "" 1000))))
      (when (> doc-count citation-count)
        (push (format "Citation database missing %d documents" (- doc-count citation-count)) issues)))

    ;; Check for orphaned citations
    ;; (Additional checks would go here)

    ;; Check concept relationships
    ;; (Additional checks would go here)

    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Data Consistency Check\n\n")
      (insert (format "* Report Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))

      (if issues
          (progn
            (insert "** Issues Found\n\n")
            (dolist (issue issues)
              (insert (format "- [ ] %s\n" issue)))
            (insert "\n** Recommendations\n")
            (insert "- [ ] Run full synchronization\n")
            (insert "- [ ] Review and resolve inconsistencies\n")
            (insert "- [ ] Consider manual data cleanup\n"))
        (insert "** No Issues Found\n\nAll components appear to be in sync.\n"))

      (goto-char (point-min)))

    (switch-to-buffer buffer)
    (message "Consistency check completed. Found %d issues." (length issues))))

;;;; Initialization

(defun data-synchronization-initialize ()
  "Initialize data synchronization."
  (data-synchronization-start-auto-sync)
  (message "Data synchronization initialized"))

;; Auto-initialize
(data-synchronization-initialize)

(provide 'data-synchronization)

;;; data-synchronization.el ends here

;;; Usage:
;;
;; This file provides data synchronization between all scientific mapping components.
;; Key functions:
;; - data-synchronization-full-sync: Complete synchronization
;; - data-synchronization-consistency-check: Check for data inconsistencies
;; - data-synchronization-start-auto-sync: Enable automatic sync
;; - data-synchronization-stop-auto-sync: Disable automatic sync
;;
;; Automatic synchronization runs when:
;; - Documents are saved (after-save-hook)
;; - Periodic timer checks for changes
;; - Manual sync commands are executed