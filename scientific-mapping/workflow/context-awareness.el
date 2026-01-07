;;; context-awareness.el --- Smart suggestions and context-aware features -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This file provides context-aware features that intelligently suggest next actions
;; based on current work, detect patterns, and provide smart recommendations.

;;; Code:

;; (require 'scientific-mapping)  ; Package main feature loaded via scientific-mapping.el
(require 'doc-engine)
(require 'cite/index)
;; (require 'concept-relationships)
(require 'ai-integration nil t)

(defgroup context-awareness ()
  "Context-aware features and smart suggestions."
  :group 'scientific-mapping)

(defcustom context-awareness-enable-suggestions t
  "If non-nil, provide smart suggestions based on context."
  :group 'context-awareness
  :type 'boolean)

(defvar context-awareness-current-context nil
  "Current work context (document, concept, research area, etc.)")

(defvar context-awareness-work-patterns nil
  "Learned work patterns for smart suggestions.")

;;;; Context Detection

(defun context-awareness-detect-context ()
  "Detect current work context from buffer and recent activity."
  (let ((context '()))
    ;; Detect document context
    (when (and (eq major-mode 'org-mode) (buffer-file-name))
      (let ((file (buffer-file-name)))
        (when (member file (doc-engine-all-files))
          (push `((type . document) (file . ,file)) context))))

    ;; Detect concept context
    (when (and (eq major-mode 'org-mode)
               (string-match-p "concepts" (buffer-file-name)))
      (push `((type . concept) (file . ,(buffer-file-name))) context))

    ;; Detect research area from recent activity
    (let ((recent-docs (context-awareness-get-recent-documents 5)))
      (when recent-docs
        (let ((common-keywords (context-awareness-extract-common-keywords recent-docs)))
          (when common-keywords
            (push `((type . research-area) (keywords . ,common-keywords)) context)))))

    ;; Detect time-based context (morning reading, afternoon writing, etc.)
    (let ((hour (string-to-number (format-time-string "%H"))))
      (cond
       ((and (>= hour 6) (< hour 12))
        (push `((type . time-context) (period . morning) (activity . reading)) context))
       ((and (>= hour 12) (< hour 17))
        (push `((type . time-context) (period . afternoon) (activity . analysis)) context))
       ((and (>= hour 17) (< hour 22))
        (push `((type . time-context) (period . evening) (activity . writing)) context))))

    context))

(defun context-awareness-get-recent-documents (count)
  "Get COUNT most recently modified documents."
  (let ((docs-with-times '()))
    (dolist (file (doc-engine-all-files))
      (when (file-exists-p file)
        (let ((mod-time (nth 5 (file-attributes file))))
          (push (cons mod-time file) docs-with-times))))
    (mapcar #'cdr
            (sort docs-with-times
                  (lambda (a b) (time-less-p (car b) (car a)))))))

(defun context-awareness-extract-common-keywords (files)
  "Extract common keywords from a list of document files."
  (let ((all-keywords '()))
    (dolist (file files)
      (condition-case err
          (let ((metadata (doc-engine-extract-metadata file)))
            (let ((keywords (cdr (assoc 'keywords metadata))))
              (when keywords
                (setq all-keywords (append all-keywords keywords)))))
        (error nil)))

    ;; Find keywords that appear in multiple documents
    (let ((keyword-counts (make-hash-table :test 'equal)))
      (dolist (keyword all-keywords)
        (puthash keyword (1+ (gethash keyword keyword-counts 0)) keyword-counts))

      (let ((common '()))
        (maphash (lambda (keyword count)
                   (when (>= count 2) ; Appears in at least 2 documents
                     (push keyword common)))
                 keyword-counts)
        common))))

;;;; Smart Suggestions

;;;###autoload
(defun context-awareness-show-suggestions ()
  "Show smart suggestions based on current context."
  (interactive)
  (let ((context (context-awareness-detect-context))
        (buffer (get-buffer-create "*Smart Suggestions*")))

    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Smart Suggestions\n\n")
      (insert (format "* Context Analysis (%d contexts detected)\n\n" (length context)))

      ;; Display detected contexts
      (dolist (ctx context)
        (let ((type (cdr (assoc 'type ctx))))
          (insert (format "** %s Context\n" (capitalize (symbol-name type))))
          (cond
           ((eq type 'document)
            (insert (format "- Current document: %s\n" (file-name-nondirectory (cdr (assoc 'file ctx)))))
            (insert "- Suggestions:\n")
            (insert "  - [ ] Analyze with AI\n")
            (insert "  - [ ] Extract concepts\n")
            (insert "  - [ ] Schedule review\n")
            (insert "  - [ ] Find related papers\n"))

           ((eq type 'concept)
            (insert (format "- Current concept file: %s\n" (file-name-nondirectory (cdr (assoc 'file ctx)))))
            (insert "- Suggestions:\n")
            (insert "  - [ ] Visualize relationships\n")
            (insert "  - [ ] Find related documents\n")
            (insert "  - [ ] Check for updates\n"))

           ((eq type 'research-area)
            (let ((keywords (cdr (assoc 'keywords ctx))))
              (insert (format "- Research area: %s\n" (string-join keywords ", ")))
              (insert "- Suggestions:\n")
              (insert "  - [ ] Search for new papers\n")
              (insert "  - [ ] Generate literature review\n")
              (insert "  - [ ] Check related concepts\n")))

           ((eq type 'time-context)
            (let ((period (cdr (assoc 'period ctx)))
                  (activity (cdr (assoc 'activity ctx))))
              (insert (format "- Time context: %s (%s)\n" period activity))
              (insert "- Suggestions:\n")
              (cond
               ((eq activity 'reading)
                (insert "  - [ ] Import new papers\n")
                (insert "  - [ ] Review recent additions\n")
                (insert "  - [ ] Scan abstracts\n"))
               ((eq activity 'analysis)
                (insert "  - [ ] Run AI analysis\n")
                (insert "  - [ ] Extract concepts\n")
                (insert "  - [ ] Update visualizations\n"))
               ((eq activity 'writing)
                (insert "  - [ ] Generate summaries\n")
                (insert "  - [ ] Write literature review\n")
                (insert "  - [ ] Update progress\n"))))))
          (insert "\n")))

      ;; General suggestions based on system state
      (insert "* System-Wide Suggestions\n\n")

      ;; Check for unanalyzed documents
      (let ((unanalyzed (context-awareness-find-unanalyzed-documents)))
        (when unanalyzed
          (insert (format "** Analyze Documents (%d unanalyzed)\n" (length unanalyzed)))
          (insert "- [ ] Run batch AI analysis\n")
          (insert "- [ ] Review analysis results\n")
          (insert "- [ ] Update concept relationships\n\n")))

      ;; Check for upcoming deadlines
      (let ((deadlines (context-awareness-get-upcoming-deadlines)))
        (when deadlines
          (insert (format "** Upcoming Deadlines (%d items)\n" (length deadlines)))
          (dolist (deadline deadlines)
            (insert (format "- [ ] %s\n" deadline)))
          (insert "\n")))

      ;; Suggest based on work patterns
      (let ((patterns (context-awareness-analyze-work-patterns)))
        (when patterns
          (insert "** Work Pattern Suggestions\n")
          (dolist (pattern patterns)
            (insert (format "- [ ] %s\n" pattern)))
          (insert "\n")))

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun context-awareness-find-unanalyzed-documents ()
  "Find documents that haven't been analyzed with AI."
  (let ((unanalyzed '()))
    (dolist (file (doc-engine-all-files))
      (condition-case err
          (unless (org-entry-get nil "AI_ANALYZED" file)
            (push file unanalyzed))
        (error nil)))
    unanalyzed))

(defun context-awareness-get-upcoming-deadlines ()
  "Get upcoming deadlines from agenda."
  (let ((deadlines '()))
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
                 (push (format "%s: %s" deadline (org-get-heading t t)) deadlines))))
           nil 'file))))
    deadlines))

(defun context-awareness-analyze-work-patterns ()
  "Analyze work patterns to provide suggestions."
  (let ((patterns '()))
    ;; Simple pattern analysis - could be expanded
    (let ((recent-activity (context-awareness-get-recent-activity 7)))
      (when (> (length recent-activity) 3)
        (push "Consider batch processing similar tasks" patterns))

      (let ((doc-count (length (doc-engine-all-files))))
        (when (> doc-count 10)
          (push "Large document collection - consider organizing by research themes" patterns)))

      (let ((concept-count (length (concept-relationships-files))))
        (when (> concept-count 5)
          (push "Growing concept network - review relationships and create visualizations" patterns))))

    patterns))

(defun context-awareness-get-recent-activity (days)
  "Get recent activity for the last DAYS."
  (let ((cutoff-time (time-subtract (current-time) (* days 24 3600)))
        (activity '()))
    (dolist (file (doc-engine-all-files))
      (when (file-exists-p file)
        (let ((mod-time (nth 5 (file-attributes file))))
          (when (time-less-p cutoff-time mod-time)
            (push file activity)))))
    activity))

;;;; Proactive Suggestions

(defvar context-awareness-suggestion-timer nil
  "Timer for periodic suggestion checks.")

(defun context-awareness-enable-proactive-suggestions ()
  "Enable proactive suggestions that appear automatically."
  (interactive)
  (when context-awareness-suggestion-timer
    (cancel-timer context-awareness-suggestion-timer))

  (when context-awareness-enable-suggestions
    (setq context-awareness-suggestion-timer
          (run-with-timer 3600 3600 #'context-awareness-check-for-suggestions)) ; Check hourly
    (message "Proactive suggestions enabled")))

(defun context-awareness-disable-proactive-suggestions ()
  "Disable proactive suggestions."
  (interactive)
  (when context-awareness-suggestion-timer
    (cancel-timer context-awareness-suggestion-timer)
    (setq context-awareness-suggestion-timer nil)
    (message "Proactive suggestions disabled")))

(defun context-awareness-check-for-suggestions ()
  "Check if there are useful suggestions to show."
  (let ((suggestions (context-awareness-generate-proactive-suggestions)))
    (when suggestions
      (message "Scientific Mapping: %d suggestions available (C-c s S to view)"
               (length suggestions)))))

(defun context-awareness-generate-proactive-suggestions ()
  "Generate proactive suggestions based on system state."
  (let ((suggestions '()))

    ;; Check for unanalyzed documents
    (let ((unanalyzed (context-awareness-find-unanalyzed-documents)))
      (when (> (length unanalyzed) 2)
        (push "Multiple documents awaiting AI analysis" suggestions)))

    ;; Check for upcoming deadlines
    (let ((deadlines (context-awareness-get-upcoming-deadlines)))
      (when deadlines
        (push (format "%d upcoming deadlines to review" (length deadlines)) suggestions)))

    ;; Check for disconnected concepts
    (let ((orphaned-concepts (context-awareness-find-orphaned-concepts)))
      (when orphaned-concepts
        (push (format "%d concepts not connected to documents" (length orphaned-concepts)) suggestions)))

    suggestions))

(defun context-awareness-find-orphaned-concepts ()
  "Find concepts that aren't linked to any documents."
  ;; This would need to check concept relationships against document metadata
  ;; For now, return placeholder
  '())

;;;; Quick Actions

;;;###autoload
(defun context-awareness-quick-actions ()
  "Show quick actions based on current context."
  (interactive)
  (let* ((context (context-awareness-detect-context))
         (actions (context-awareness-generate-quick-actions context))
         (selection (completing-read "Quick Action: " actions nil t)))

    (when selection
      (let ((action (cdr (assoc selection actions))))
        (when action
          (funcall action))))))

(defun context-awareness-generate-quick-actions (context)
  "Generate quick actions based on context."
  (let ((actions '()))

    ;; Document-specific actions
    (when (member 'document (mapcar (lambda (c) (cdr (assoc 'type c))) context))
      (push '("Analyze Document" . ai-integration-analyze-document) actions)
      (push '("Extract Concepts" . ai-integration-extract-concepts) actions)
      (push '("Schedule Review" . scientific-mapping-schedule-paper-review) actions))

    ;; Concept-specific actions
    (when (member 'concept (mapcar (lambda (c) (cdr (assoc 'type c))) context))
      (push '("Visualize Relationships" . concept-relationships-visualize) actions)
      (push '("Add Child Concept" . concept-relationships-add-child) actions))

    ;; Research area actions
    (when (member 'research-area (mapcar (lambda (c) (cdr (assoc 'type c))) context))
      (push '("Search arXiv" . academic-apis-search-arxiv) actions)
      (push '("Generate Literature Review" . scientific-mapping-literature-review) actions))

    ;; General actions
    (push '("Unified Search" . workflow-integration-unified-search) actions)
    (push '("Show Suggestions" . context-awareness-show-suggestions) actions)
    (push '("Sync Data" . data-synchronization-full-sync) actions)

    actions))

;;;; Initialization

(defun context-awareness-initialize ()
  "Initialize context awareness features."
  (context-awareness-enable-proactive-suggestions)
  (message "Context awareness initialized"))

;; Auto-initialize
(context-awareness-initialize)

(provide 'context-awareness)

;;; context-awareness.el ends here

;;; Usage:
;;
;; This file provides context-aware features for smart suggestions.
;; Key functions:
;; - context-awareness-show-suggestions: Show context-based suggestions
;; - context-awareness-quick-actions: Quick action menu
;; - context-awareness-enable-proactive-suggestions: Enable hourly checks
;;
;; Context detection includes:
;; - Current document/concept being worked on
;; - Research area based on recent activity
;; - Time of day (morning reading, afternoon analysis, evening writing)
;; - System state (unanalyzed documents, upcoming deadlines)