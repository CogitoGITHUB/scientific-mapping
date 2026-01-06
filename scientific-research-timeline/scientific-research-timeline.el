;;; scientific-research-timeline.el --- Visual timeline for scientific research nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Adapted from org-roam-timeline by Gerardo Cendejas Mendoza
;; Maintainer: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (scientific-document-engine "1.0.0") (json-mode "1.0") (simple-httpd "1.5.1"))

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

;; scientific-research-timeline provides a visual timeline interface for scientific
;; research nodes managed by the Scientific Knowledge Mapping System. It enables
;; researchers to:
;;
;; - View research chronology across papers and notes
;; - Track reading progress over time
;; - Visualize publication dates and research phases
;; - Filter by research tags, domains, and projects
;; - Follow research progress in real-time
;; - Interactively zoom to specific time periods
;;
;; The timeline adapts to the scientific workflow, supporting TIMELINE_START
;; and TIMELINE_END properties for custom date ranges, with automatic
;; date extraction from DOI metadata and publication information.

;;; Code:

(require 'scientific-document-engine)
(require 'citation-database)
(require 'simple-httpd)
(require 'json)
(require 'ox-html)

(defconst scientific-research-timeline-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory where scientific-research-timeline package is installed.")

;; --- SIGNALS ---

(defvar scientific-research-timeline--explicit-focus-id nil
  "Store ID of research node to be explicitly focused.")

(defvar scientific-research-timeline--explicit-hide-id nil
  "Store ID of research node to be explicitly hidden.")

(defvar scientific-research-timeline--explicit-date-focus nil
  "Store date string to zoom towards.")

(defvar scientific-research-timeline--filter-signal nil
  "Signal to filter research tags. A cons cell of (action . tag).")

(defvar scientific-research-timeline--toggle-follow-signal nil
  "Boolean signal to toggle Follow Mode.")

(defvar scientific-research-timeline--toggle-preview-signal nil
  "Boolean signal to toggle auto-preview panel.")

;; --- HELPER: Get All Research Tags ---

(defun scientific-research-timeline--get-all-tags ()
  "Retrieve a list of all unique research tags in citation database."
  (mapcar #'car (emacsql citation-database-db
                   [:select :distinct keyword :from keywords])))

;; --- CONFIGURATION ---

(defgroup scientific-research-timeline nil
  "Settings for Scientific Research Timeline."
  :group 'scientific-mapping)

(defcustom scientific-research-timeline-default-theme 'dark
  "Default theme: 'dark or 'light."
  :group 'scientific-research-timeline
  :type '(choice (const :tag "Dark" dark)
                  (const :tag "Light" light)))

(defcustom scientific-research-timeline-focus-window-years 5
  "Zoom window in years for date focus."
  :group 'scientific-research-timeline
  :type 'integer)

(defcustom scientific-research-timeline-show-links-on-start t
  "Show citation links on timeline start."
  :group 'scientific-research-timeline
  :type 'boolean)

(defcustom scientific-research-timeline-follow-mode-on-start t
  "Follow mode active on timeline start."
  :group 'scientific-research-timeline
  :type 'boolean)

(defcustom scientific-research-timeline-preview-on-start t
  "Auto-open preview panel on timeline start."
  :group 'scientific-research-timeline
  :type 'boolean)

;; --- DATA PROCESSING ---

(defun scientific-research-timeline--process-node (paper-id)
  "Process a research PAPER-ID to extract time and structure metadata."
  (condition-case err
      (let* ((paper (citation-database-get-paper-by-identifier paper-id))
             (id (plist-get paper :id))
             (title (plist-get paper :title))
             (year (plist-get paper :year))
             (journal (plist-get paper :journal))
             (reading-status (plist-get paper :reading-status))
             (priority (plist-get paper :priority))
             (keywords (plist-get paper :keywords))
             (citation-count (plist-get paper :citation-count))
             (impact-factor (plist-get paper :impact-factor))
             (created-at (plist-get paper :created-at)))
        
        (when year
          (let* ((date-start (format "%d-01-01" year))
                 (date-end (format "%d-12-31" year))
                 (tags (or keywords '("Uncategorized")))
                 (citations (citation-database-get-citations id))
                 (cited-by (citation-database-get-cited-by id))
                 (total-connections (+ (length citations) (length cited-by)))
                 (importance-class (cond
                                   ((> citation-count 100) "research-seminal")
                                   ((> citation-count 50) "research-high-impact")
                                   ((> citation-count 20) "research-cited")
                                   ((> citation-count 5) "research-notable")
                                   (t "research-emerging"))))
            
            `((id . ,id)
              (content . ,title)
              (title . "Metadata")
              (start . ,date-start)
              (end . ,date-end)
              (type . "range")
              (className . ,importance-class)
              (journal . ,journal)
              (year . ,year)
              (reading_status . ,reading-status)
              (priority . ,priority)
              (citation_count . ,citation-count)
              (impact_factor . ,impact-factor)
              (all_tags . ,tags)))))
    (error nil)))

(defun scientific-research-timeline--get-nodes ()
  "Retrieve and process all valid research nodes for timeline."
  (let* ((papers (emacsql citation-database-db
                       [:select * :from nodes
                        :order-by [(desc year)]))
         (nodes '()))
    (dolist (paper papers)
      (let* ((paper-id (elt paper 0))
             (item (scientific-research-timeline--process-node paper-id)))
        (when item
          (push item nodes))))
    nodes))

(defun scientific-research-timeline--clean-date (date-str &optional is-end)
  "Clean and format DATE-STR. If IS-END is non-nil, adjust to year end."
  (when (stringp date-str)
    (let ((clean (string-trim date-str)))
      (cond
       ((string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" clean) clean)
       ((string-match "^\\(-?[0-9]\\{4\\}\\)$" clean)
        (if is-end (concat (match-string 1 clean) "-12-31") (concat (match-string 1 clean) "-01-01")))
       ((string-match "\\[<\\(-?[0-9]+\\S-*\\)[\\]>]" clean) (match-string 1 clean))
       (t nil)))))

;; --- SERVLETS ---

(defservlet* scientific-research-timeline-content text/html (id)
  (require 'ox-html)
  (let* ((paper (citation-database-get-paper-by-identifier id))
         (marker (when paper
                   (org-id-find id 'marker)))
         (final-output ""))
    
    (if (not marker)
        (setq final-output (format "<h3>Error: Research ID %s not found.</h3>" id))
      
      (let* ((raw-content (with-current-buffer (marker-buffer marker)
                             (save-excursion
                              (goto-char (marker-position marker))
                              (let ((beg (point))
                                    (end (if (org-at-heading-p)
                                              (save-excursion (org-end-of-subtree) (point))
                                            (point-max))))
                                (buffer-substring-no-properties beg end)))))
             (html (with-temp-buffer
                      (insert raw-content)
                      (let ((org-export-use-babel nil)
                            (org-confirm-babel-evaluate nil)
                            (org-html-with-latex 'mathjax)
                            (org-export-with-section-numbers nil)
                            (org-html-link-org-files-as-html nil))
                        (org-export-string-as 'html t '(:with-toc nil :with-section-numbers nil)))))
             (title (when paper (plist-get paper :title)))
             (keywords (when paper (plist-get paper :keywords)))
             (journal (when paper (plist-get paper :journal)))
             (year (when paper (plist-get paper :year))))
        
        (setq html (concat (format "<h1 class='node-title'>%s</h1>" 
                                      (format "%s <em style='color:#666; font-size:0.9em;'><%s, %d</em>" 
                                             title journal year)) html))
        
        ;; Add metadata section
        (setq html (concat html 
                           "<div class='metadata-section' style='margin-top:30px; border-top:1px solid #eee; padding-top:20px;'><h4 style='color:#888; text-transform:uppercase; font-size:12px; letter-spacing:1px;'>Research Metadata</h4><ul style='list-style:none; padding:0;'>"))
        
        (when keywords
          (setq html (concat html 
                               (mapconcat
                                (lambda (kw)
                                  (format "<li><span class='label'>Keywords:</span> %s</li>" kw))
                                keywords))))
        
        (when reading-status
          (setq html (concat html 
                               (format "<li><span class='label'>Reading Status:</span> <span class='status status-%s'>%s</span></li>" reading-status reading-status))))
        
        (when priority
          (setq html (concat html 
                               (format "<li><span class='label'>Priority:</span> <span class='priority priority-%s'>%s</span></li>" priority priority))))
        
        (setq html (concat html "</ul></div>"))
        
        ;; Add citation network preview
        (setq html (concat html 
                           "<div class='citation-preview' style='margin-top:30px;'><h4 style='color:#888; text-transform:uppercase; font-size:12px; letter-spacing:1px;'>Citation Network</h4><p style='color:#666; font-size:0.9em;'>Links and citations for this research</p></div>"))
        
        (setq final-output (concat "<div class='org-content'>" html "</div>"))))
    
    (insert final-output)))

;; UPDATED CONFIG SERVLET (Sends preview setting)
(defservlet* scientific-research-timeline-config text/json ()
  (insert (json-encode `((theme . ,(symbol-name scientific-research-timeline-default-theme))
                          (showLinks . ,(if scientific-research-timeline-show-links-on-start t :json-false))
                          (followMode . ,(if scientific-research-timeline-follow-mode-on-start t :json-false))
                          (autoPreview . ,(if scientific-research-timeline-preview-on-start t :json-false))
                          (zoomWindow . ,scientific-research-timeline-focus-window-years)))))

(defservlet* scientific-research-timeline-data text/json ()
  (insert (json-encode (scientific-research-timeline--get-nodes))))

(defservlet* scientific-research-timeline-node-data text/json (id)
  (let ((paper (citation-database-get-paper-by-identifier id)))
    (if paper
        (let ((item (scientific-research-timeline--process-node id)))
          (if item (insert (json-encode item)) (insert "{}")))
      (insert "{}"))))

(defservlet* scientific-research-timeline-open text/plain (id)
  (let ((paper (citation-database-get-paper-by-identifier id)))
    (if paper
        (progn
          (let* ((file (plist-get paper :file))
                 (pos (plist-get paper :pos)))
            (with-current-buffer (window-buffer (selected-window))
              (find-file file)
              (goto-char pos)))
          (insert "Opened"))
      (insert "Research not found"))))

;; --- POLLING SERVLET ---

(defservlet* scientific-research-timeline-current-focus text/json ()
  (let ((response '((action . "none"))))
    
    ;; 1. Tags Filter
    (when scientific-research-timeline--filter-signal
      (setq response `((action . ,(car scientific-research-timeline--filter-signal))
                        (tag . ,(cdr scientific-research-timeline--filter-signal))))
      (setq scientific-research-timeline--filter-signal nil))
    
    ;; 2. Date Zoom
    (unless (assoc 'tag response)
      (when scientific-research-timeline--explicit-date-focus
        (setq response `((action . "zoom-date")
                          (date . ,scientific-research-timeline--explicit-date-focus)))
        (setq scientific-research-timeline--explicit-date-focus nil)))
    
    ;; 3. Toggle Follow
    (unless (or (assoc 'tag response) (string-equal (cdr (assoc 'action response)) "zoom-date"))
      (when scientific-research-timeline--toggle-follow-signal
        (setq response `((action . "toggle-follow")))
        (setq scientific-research-timeline--toggle-follow-signal nil)))
    
    ;; 4. Toggle Preview
    (unless (or (assoc 'tag response)
                  (string-equal (cdr (assoc 'action response)) "zoom-date")
                  (string-equal (cdr (assoc 'action response)) "toggle-follow"))
      (when scientific-research-timeline--toggle-preview-signal
        (setq response `((action . "toggle-preview")))
        (setq scientific-research-timeline--toggle-preview-signal nil)))
    
    ;; 5. Hide
    (unless (or (assoc 'tag response)
                  (string-equal (cdr (assoc 'action response)) "zoom-date")
                  (string-equal (cdr (assoc 'action response)) "toggle-follow")
                  (string-equal (cdr (assoc 'action response)) "toggle-preview"))
      (when scientific-research-timeline--explicit-hide-id
        (setq response `((action . "hide")
                          (id . ,scientific-research-timeline--explicit-hide-id)))
        (setq scientific-research-timeline--explicit-hide-id nil)))
    
    ;; 6. Focus / Follow
    (if (string-equal (cdr (assoc 'action response)) "none")
        (if scientific-research-timeline--explicit-focus-id
            (progn
              (setq response `((action . "focus")
                            (id . ,scientific-research-timeline--explicit-focus-id)))
              (setq scientific-research-timeline--explicit-focus-id nil))
          (let* ((user-window (selected-window))
                  (user-buffer (window-buffer user-window))
                  (node-id nil))
            (with-current-buffer user-buffer
              (when (and (derived-mode-p 'org-mode) (buffer-file-name))
                (let ((file (buffer-file-name)))
                  (when (string-match-p scientific-document-directory file)
                    (with-current-buffer (find-file-noselect file)
                      (let ((identifier (org-entry-get (point-min) "IDENTIFIER")))
                        (when identifier
                          (setq node-id identifier))))))))
            (when node-id
              (setq response `((action . "follow")
                            (id . ,node-id)))))))
    
    (insert (json-encode response))))

(defservlet* scientific-research-timeline-remove-date text/plain (id)
  (let ((paper (citation-database-get-paper-by-identifier id)))
    (if paper
        (let* ((file (plist-get paper :file))
               (identifier (plist-get paper :identifier)))
          (with-current-buffer (find-file-noselect file)
            (org-delete-property "TIMELINE_START" identifier)
            (org-delete-property "TIMELINE_END" identifier)
            (save-buffer))
          (insert "Removed"))
      (insert "Research not found"))))

;; --- INTERACTIVE COMMANDS ---

(defun scientific-research-timeline-open ()
  "Start web server and open research timeline in browser."
  (interactive)
  (setq httpd-root (expand-file-name "html" scientific-research-timeline-root))
  (httpd-start)
  (browse-url (format "http://localhost:%d" httpd-port)))

(defun scientific-research-timeline-show-node ()
  "Focus current research node in visual timeline."
  (interactive)
  (let* ((file (buffer-file-name))
         (identifier (when (and file (string-match-p scientific-document-directory file))
                        (with-current-buffer (find-file-noselect file)
                          (org-entry-get (point-min) "IDENTIFIER")))))
    (if identifier
        (progn
          (setq scientific-research-timeline--explicit-focus-id identifier)
          (message "Research Timeline: Focus Node"))
      (user-error "No research node at point"))))

(defun scientific-research-timeline-hide-node ()
  "Hide current research node from visual timeline."
  (interactive)
  (let* ((file (buffer-file-name))
         (identifier (when (and file (string-match-p scientific-document-directory file))
                        (with-current-buffer (find-file-noselect file)
                          (org-entry-get (point-min) "IDENTIFIER")))))
    (if identifier
        (progn
          (setq scientific-research-timeline--explicit-hide-id identifier)
          (message "Research Timeline: Hide Node"))
      (user-error "No research node at point"))))

(defun scientific-research-timeline-toggle-follow ()
  "Toggle Follow Mode in visualization."
  (interactive)
  (setq scientific-research-timeline--toggle-follow-signal t)
  (message "Research Timeline: Toggled Follow Mode"))

(defun scientific-research-timeline-toggle-preview ()
  "Toggle Auto-Preview (sidebar content) in browser."
  (interactive)
  (setq scientific-research-timeline--toggle-preview-signal t)
  (message "Research Timeline: Toggled Auto-Preview"))

(defun scientific-research-timeline-add-date ()
  "Interactively add TIMELINE_START/END properties to current research node."
  (interactive)
  (let* ((start-input (read-string "Start (YYYY or YYYY-MM-DD): "))
          (is-range (y-or-n-p "Is this a time range? "))
          (end-input (if is-range (read-string "End (YYYY or YYYY-MM-DD): ") nil)))
    (unless (string-empty-p start-input)
      (org-set-property "TIMELINE_START" start-input)
      (if end-input
          (org-set-property "TIMELINE_END" end-input)
        (org-delete-property "TIMELINE_END"))
      (save-buffer)
      (citation-database-index-file (buffer-file-name))
      (scientific-research-timeline-show-node))))

(defun scientific-research-timeline-zoom-date ()
  "Zoom timeline to a specific date."
  (interactive)
  (let ((date-str (read-string "Zoom to Date (YYYY[-MM[-DD]]): ")))
    (unless (string-empty-p date-str)
      (setq scientific-research-timeline--explicit-date-focus date-str)
      (message "Research Timeline: Zooming to %s..." date-str))))

(defun scientific-research-timeline-filter-toggle ()
  "Toggle visibility of a specific research tag in timeline."
  (interactive)
  (let ((tag (completing-read "Toggle Research Tag: " (scientific-research-timeline--get-all-tags) nil t)))
    (setq scientific-research-timeline--filter-signal (cons "filter-toggle" tag))
    (message "Research Timeline: Toggling %s" tag)))

(defun scientific-research-timeline-filter-block ()
  "Block (force hide) a specific research tag in timeline."
  (interactive)
  (let ((tag (completing-read "Block Research Tag: " (scientific-research-timeline--get-all-tags) nil t)))
    (setq scientific-research-timeline--filter-signal (cons "filter-block" tag))
    (message "Research Timeline: Blocking %s" tag)))

(defun scientific-research-timeline-filter-reset ()
  "Reset all visualization filters."
  (interactive)
  (setq scientific-research-timeline--filter-signal (cons "filter-reset" "all"))
  (message "Research Timeline: Resetting filters"))

(defun scientific-research-timeline-filter-hide-all ()
  "Hide all research tags (blank slate)."
  (interactive)
  (setq scientific-research-timeline--filter-signal (cons "filter-hide-all" "all"))
  (message "Research Timeline: Hiding all research tags (Blank Slate)."))

(provide 'scientific-research-timeline)

;;; scientific-research-timeline.el ends here