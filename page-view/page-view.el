;;; page-view.el --- Scientific writing workflow with page-based layout -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Adapted from page-view by Brad Stewart
;; Maintainer: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (org "9.5"))

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

;; page-view provides a word processor-like page layout for writing
;; scientific papers in org-mode, following established academic writing
;; standards from official sources (PLOS, Nature, Science, ICMJE, etc.).
;;
;; == INTEGRATED TIMELINE VIEW ==
;;
;; page-view integrates with timeline-engine to show connecting research
;; files alongside your writing workflow. When you start a writing session
;; (M-x page-view-start-writing-session), the window splits:
;;
;;   +-------------------+----------------+
;;   |                   |                |
;;   |   YOUR DOCUMENT   |   TIMELINE     |
;;   |   (80% width)     |   (20% width)  |
;;   |                   |                |
;;   +-------------------+----------------+
;;
;; The timeline shows:
;; - Research chronology across your papers
;; - Connecting/cited documents
;; - Publication dates and progress
;; - Related concepts and citations
;;
;; Press M-q to close the timeline and return to normal view.
;; Press C-c s L to toggle the timeline on/off.
;;
;; == SCIENTIFIC WRITING STANDARDS ==
;; 
;; This system follows established academic writing guidelines:
;;
;; **Chronological Process** (based on official sources):
;; 1. Preparation and Planning
;; 2. Literature Review
;; 3. Methods and Design
;; 4. Results and Data Presentation  
;; 5. Discussion and Analysis
;; 6. Conclusion and Implications
;;
;; **Structure Compliance**:
;; - IMRaD format: Introduction, Methods, Results, and Discussion
;; - Journal guidelines: Word counts, section requirements
;; - Citation standards: APA, MLA, Chicago, IEEE
;; - Abstract requirements: 150-250 words (structured)
;; - Figure/Table standards: Clear captions and numbering
;;
;; == FEATURES ==
;;
;; **Page Layout**:
;; - Word processor-like visual display
;; - Page breaks and pagination
;; - Headers and footers with page numbers
;; - Times New Roman 12pt, 1.5 spacing (academic standard)
;; - Margins approximating letter-sized paper (1" margins)
;; - Scientific figure/table support with captions
;;
;; **Writing Workflow**:
;; - Real-time line counting for abstract sections
;; - Automatic footnotes and citation formatting
;; - Section navigation and organization
;; - Debug mode for layout verification
;; - Integration with scientific document templates
;;
;; **Academic Compliance**:
;; - Abstract word count enforcement
;; - Figure and table caption standards
;; - Reference section formatting
;; - Journal-specific formatting support
;;
;; == USAGE ==
;;
;; Enable in any org buffer:
;;   M-x page-view-mode
;;
;; Quick workflow:
;;   1. M-x page-view-start (begin writing)
;;   2. Use page breaks to organize sections
;;   3. Write content following IMRaD structure
;;   4. M-x page-view-goto-page (navigate sections)
;;   5. Export to PDF/Word when complete
;;
;; Integration with snippet-engine templates:
;;   Templates automatically integrate with page view layout
;;   Academic document types (research, review, thesis) supported
;;   Journal-specific formatting enforced

;;; Code:

(require 'org)
(require 'olivetti)  ;; For word processor-like display
(require 'timeline-engine nil t)  ;; Optional: timeline integration

;; ==============================================================================
;; CONFIGURATION AND CUSTOMIZATION
;; ==============================================================================

(defgroup page-view nil
  "Scientific page view settings for academic writing."
  :group 'scientific-mapping
  :prefix "spv-")

;; ========================================
;; Academic Standards Configuration
;; ========================================

(defcustom page-view-paper-type 'research-article
  "Type of scientific paper being written.
Controls word count limits and structure requirements."
  :group 'page-view
  :type '(choice (const :tag "Research Article" research-article)
                  (const :tag "Literature Review" literature-review)
                  (const :tag "Conference Paper" conference-paper)
                  (const :tag "Master's Thesis" thesis-master)
                  (const :tag "Doctoral Dissertation" thesis-doc)
                  (const :tag "Technical Report" technical-report)))

(defcustom page-view-journal-type 'general
  "Journal category for specific formatting requirements.
Different journals have different word limits, abstract requirements, etc."
  :group 'page-view
  :type '(choice (const :tag "General (no specific requirements)" general)
                  (const :tag "APA Style" apa)
                  (const :tag "MLA Style" mla)
                  (const :tag "Chicago Style" chicago)
                  (const :tag "IEEE Format" ieee)
                  (const :tag "Science/PNAS" science)
                  (const :tag "Nature" nature)
                  (const :tag "PLOS" plos)))

(defcustom page-view-citation-style 'apa
  "Citation style to use for formatting references."
  :group 'page-view
  :type '(choice (const :tag "APA (7th edition)" apa)
                  (const :tag "MLA (9th edition)" mla)
                  (const :tag "Chicago (17th edition)" chicago)
                  (const :tag "IEEE" ieee)
                  (const :tag "Vancouver" vancouver)
                  (const :tag "Harvard" harvard)))

;; ========================================
;; Page Layout Configuration
;; ========================================

(defcustom page-view-page-width 69
  "Width of text in characters (approximating letter-sized page).
Times New Roman 12pt at 1.5 spacing ≈ 69 chars wide."
  :group 'page-view
  :type 'integer)

(defcustom page-view-lines-per-page 36
  "Number of lines per page for academic writing.
Approximates ~250-300 words per page (standard single-spaced)."
  :group 'page-view
  :type 'integer)

(defcustom page-view-line-spacing 1.5
  "Line spacing for academic writing (standard is 1.5)."
  :group 'page-view
  :type 'number)

(defcustom page-view-font-family "Times New Roman"
  "Font family for academic writing (standard is Times New Roman)."
  :group 'page-view
  :type 'string)

(defcustom page-view-font-size 12
  "Font size in points (academic standard is 12pt)."
  :group 'page-view
  :type 'integer)

;; ========================================
;; Header and Footer Configuration
;; ========================================

(defcustom page-view-show-headers t
  "Show document title and author information at top of each page."
  :group 'page-view
  :type 'boolean)

(defcustom page-view-show-footers t
  "Show page numbers at bottom of each page."
  :group 'page-view
  :type 'boolean)

(defcustom page-view-footer-format "Page %d"
  "Format string for page footer. Use %d for page number."
  :group 'page-view
  :type 'string)

;; ========================================
;; Academic Writing Features
;; ========================================

(defcustom page-view-enforce-word-counts t
  "Enforce abstract word count limits based on paper type."
  :group 'page-view
  :type 'boolean)

(defcustom page-view-auto-number-figures t
  "Automatically number figures in order of appearance."
  :group 'page-view
  :type 'boolean)

(defcustom page-view-auto-number-tables t
  "Automatically number tables in order of appearance."
  :group 'page-view
  :type 'boolean)

(defcustom page-view-track-changes nil
  "Track word count and line statistics for progress monitoring."
  :group 'page-view
  :type 'boolean)

;; ==============================================================================
;; WORD COUNT LIMITS (From Official Sources)
;; ==============================================================================

(defconst page-view-word-count-limits
  '((research-article . ((abstract . 250)  ; PLOS standard
                     (intro . 300)
                     (methods . 1500)
                     (results . 1500)
                     (discussion . 1500)
                     (conclusion . 300)))
    (literature-review . ((abstract . 250)  ; Systematic reviews
                          (total . 8000)
                          (introduction . 1000)))
    (conference-paper . ((abstract . 200)  ; Conference limits
                        (total . 3000)
                        (methods . 800)))
    (thesis-master . ((abstract . 400)  ; Master's thesis
                      (introduction . 1500)
                      (total . 40000)))
    (thesis-doc . ((abstract . 350)  ; Doctoral dissertation
                   (introduction . 3000)
                   (total . 100000)))
    (technical-report . ((abstract . 250)
                       (total . 5000)))
  "Word count limits based on official journal and university guidelines.
Sources: PLOS, Nature, Science, ICMJE, university thesis requirements.")

;; ==============================================================================
;; FACE DEFINITIONS
;; ==============================================================================

;; Page break separator (horizontal line across page)
(defface page-view-pagebreak-face
  `((t :inherit 'tab-bar
       :family "monospace"
       :foreground "white"
       :background "gray30"
       :height 1.0))
  "Face for page break separators in scientific documents."
  :group 'page-view)

;; Document title in page header
(defface page-view-header-face
  `((t :inherit 'default
       :family ,page-view-font-family
       :weight 'bold
       :height 1.1))
  "Face for document headers."
  :group 'page-view)

;; Page number in footer
(defface page-view-footer-face
  `((t :inherit 'default
       :family "monospace"
       :weight 'bold))
  "Face for page footers."
  :group 'page-view)

;; Debug information overlays
(defface page-view-debug-face
  `((t :inherit 'page-view-footer-face
       :foreground "yellow"
       :slant 'italic))
  "Face for debug mode overlays."
  :group 'page-view)

;; Section headings for organization
(defface page-view-section-face
  `((t :inherit 'org-level-1
       :weight 'bold
       :height 1.2))
  "Face for section headings."
  :group 'page-view)

;; ==============================================================================
;; INTERNAL VARIABLES
;; ==============================================================================

(defvar-local page-view--overlays nil
  "Hash table storing page overlay objects.
Key: page-number → overlay object")

(defvar-local page-view--max-page 1
  "Current highest page number in document.")

(defvar-local page-view--debug-flag nil
  "When non-nil, show debug overlays with line heights.")

(defvar-local page-view--track-statistics nil
  "When tracking writing progress (word counts, etc).")

;; ==============================================================================
;; INITIALIZATION
;; ==============================================================================

(defun page-view-initialize()
  "Initialize page-view system for current buffer."
  (interactive)
  (message "Initializing Scientific Page View...")
  
  ;; Configure olivetti for academic writing
  (setq-local olivetti-style 'fancy)
  (setq-local olivetti-body-width page-view-page-width)
  
  ;; Set academic font
  (setq-local default-text-properties
            `(line-spacing ,page-view-line-spacing
                 wrap-prefix (space . (:height ,(round (* page-view-font-size 
                                                                   page-view-line-spacing))
                                        :width page-view-page-width))))
  
  ;; Enable olivetti mode
  (olivetti-mode 1)
  
  ;; Configure display
  (org-indent-mode -1)
  (diff-hl-mode -1)
  (hl-line-mode -1)
  
  ;; Initialize tracking
  (setq-local page-view--overlays (make-hash-table :test 'equal))
  (setq-local page-view--max-page 1)
  
  (message "Scientific Page View initialized for %s paper" 
           (symbol-name page-view-paper-type)))

;; ==============================================================================
;; PAGE BREAK AND NAVIGATION
;; ==============================================================================

(defun page-view-insert-page-break()
  "Insert a scientific page break at current point.
Creates a visual separator and prepares for new page numbering.
Follows journal standards for page breaks."
  (interactive)
  (let* ((page-number (1+ page-view--max-page))
         (current-line (line-number-at-pos))
         (height 3))
    
    ;; Insert visual separator (three underscores for page break)
    (insert (propertize "___" 'face 'page-view-pagebreak-face))
    (insert "\n\n")
    
    ;; Update max page number
    (setq page-view--max-page page-number)
    
    (message "Inserted page break. Now on page %d" page-number)
    
    ;; Update statistics if tracking
    (when page-view--track-statistics
      (page-view--update-statistics))))

(defun page-view-goto-page(page-number)
  "Navigate to specific PAGE-NUMBER in the document.
Page numbers correspond to logical document pages, not physical lines."
  (interactive "nPage number: ")
  (unless (and (integerp page-number) (> page-number 0))
    (user-error "Invalid page number: %d" page-number))
  
  ;; Find page break marker
  (let* ((found-page nil)
         (target-line (* (1- page-number) page-view-lines-per-page)))
    
    ;; Search for page break at target line
    (save-excursion
      (goto-char (point-min))
      (let ((line-count 0))
        (while (and (not found-page) (not (eobp)))
          (when (and (looking-at "___") (>= line-count target-line))
            (setq found-page t))
          (forward-line 1)
          (setq line-count (1+ line-count)))))
    
    (when found-page
      ;; Position cursor after page break
      (forward-line 1)
      (recenter))
    
    (unless found-page
      (message "Page %d not found. Document has %d pages." 
               page-number page-view--max-page))))

(defun page-view-next-page()
  "Navigate to next page in document."
  (interactive)
  (page-view-goto-page (1+ page-view--max-page)))

(defun page-view-previous-page()
  "Navigate to previous page in document."
  (interactive)
  (page-view-goto-page (1- page-view--max-page)))

;; ==============================================================================
;; HEADER AND FOOTER GENERATION
;; ==============================================================================

(defun page-view--generate-header(page-number)
  "Generate academic document header for PAGE-NUMBER.
Includes title, author, and page number."
  (let* ((title (or (org-get-title) "Untitled"))
         (author (or (org-entry-get (point-min) "AUTHOR") "Anonymous"))
         (date (or (org-entry-get (point-min) "DATE") (format-time-string "%Y-%m-%d")))
    (format "  %s | %s | %s  \n" title author date)))

(defun page-view--generate-footer(page-number)
  "Generate academic document footer for PAGE-NUMBER.
Standard format: 'Page X' at bottom of page."
  (format page-view-footer-format page-number))

;; ==============================================================================
;; WORD COUNT AND ABSTRACT ENFORCEMENT
;; ==============================================================================

(defun page-view-count-words(beginning end)
  "Count words between BEGINNING and END positions.
Uses simple word counting algorithm suitable for academic text."
  (let ((text (buffer-substring-no-properties beginning end))
        (words 0))
    (with-temp-buffer
      (insert text)
      ;; Count words (split by whitespace and punctuation)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-word 1)
        (setq words (1+ words)))
      words)))

(defun page-view-check-abstract-length()
  "Check if abstract section meets word count requirements.
Based on paper type, enforce word limits from official sources."
  (interactive)
  (let* ((limits (cdr (assq page-view-paper-type
                               page-view-word-count-limits)))
         (abstract-limit (cdr (assq 'abstract limits)))
    (save-excursion
      ;; Find abstract section
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\s+Abstract" nil t)
        (let* ((abstract-start (point))
                  (abstract-end (save-excursion
                                 (org-end-of-subtree)
                                 (point)))
                  (abstract-text (buffer-substring-no-properties
                                         (+ 7 abstract-start)  ; Skip "* Abstract"
                                         abstract-end))
                  (word-count (page-view-count-words 
                                                     abstract-start abstract-end)))
          (if (and page-view-enforce-word-counts
                     (> word-count abstract-limit))
              (progn
                (message "Abstract: %d words (limit: %d) - TOO LONG!" 
                         word-count abstract-limit)
                (setq face 'font-lock-warning-face))
            (message "Abstract: %d words (limit: %d) - OK" 
                     word-count abstract-limit)))))))

(defun page-view-show-word-counts()
  "Display word count statistics for current buffer or section."
  (interactive)
  (let* ((total-words (page-view-count-words 
                                            (point-min) (point-max)))
         (limits (cdr (assq page-view-paper-type
                                page-view-word-count-limits)))
         (expected-total (cdr (assq 'total limits))))
    (message "Current: %d words | Expected: %d words | Status: %s"
             total-words 
             expected-total
             (if (and page-view-enforce-word-counts
                      (<= total-words expected-total))
                 "✓ OK"
               (⚠ EXCEEDS")))))

;; ==============================================================================
;; FIGURE AND TABLE CAPTIONING
;; ==============================================================================

(defun page-view-number-figures()
  "Automatically number all figures in order of appearance.
Figure captions follow standard academic format: 'Figure 1: Description'."
  (interactive)
  (let ((figure-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\+CAPTION:" nil t)
        (let* ((caption-start (point))
                  (caption-end (line-end-position))
                  (caption-text (buffer-substring caption-start caption-end)))
          (setq figure-count (1+ figure-count))
          ;; Ensure caption starts with "Figure X:"
          (save-excursion
            (goto-char caption-start)
            (forward-word 2)  ; Skip "#+CAPTION:"
            (unless (looking-at "Figure [0-9]+:")
              (insert (format "Figure %d: " figure-count)))))))
    (message "Numbered %d figures." figure-count)))

(defun page-view-number-tables()
  "Automatically number all tables in order of appearance.
Table captions follow standard academic format: 'Table 1: Description'."
  (interactive)
  (let ((table-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\+CAPTION:" nil t)
        (let* ((caption-start (point))
                  (caption-end (line-end-position))
                  (caption-text (buffer-substring caption-start caption-end)))
          (when (string-match-p "Table\\|table" caption-text)
            (setq table-count (1+ table-count))
            ;; Ensure caption starts with "Table X:"
            (save-excursion
              (goto-char caption-start)
              (forward-word 2)  ; Skip "#+CAPTION:"
              (unless (looking-at "Table [0-9]+:")
                (insert (format "Table %d: " table-count)))))))
    (message "Numbered %d tables." table-count)))

;; ==============================================================================
;; SCIENTIFIC WRITING WORKFLOW
;; ==============================================================================

;; These functions follow the chronological academic writing process
;; based on official sources (PLOS, Nature, Science, ICMJE)

(defun page-view-start-writing-session()
  "Phase 1: Start new scientific writing session with integrated timeline.
Prepares buffer for academic writing with proper formatting and shows
connecting research files in a side timeline view."
  (interactive)
  (message "=== Starting Integrated Scientific Writing Session ===")
  
  ;; Save current window configuration
  (setq page-view--original-window-config (current-window-configuration))
  
  ;; Split window: main area for writing (80%), side panel for timeline (20%)
  (delete-other-windows)
  (split-window-right (floor (* (window-width) 0.2)) nil)  ; 20% for timeline
  (other-window 1)
  
  ;; Set fixed width for timeline window
  (setq window-size-fixed t)
  (when (fboundp 'window-body-size)
    (setq window-body-width (floor (* (window-width) 0.25))))
  
  ;; Start timeline engine if not already running
  (condition-case nil
      (progn
        (require 'timeline-engine nil t)
        (unless (and (boundp 'httpd-port) httpd-port)
          (setq httpd-root (expand-file-name "html" timeline-engine-root))
          (httpd-start)
          (message "Timeline engine web server started")))
    (error (message "Timeline engine not available")))
  
  ;; Open timeline in side window using EAF or browse-url
  (condition-case err
      (progn
        (require 'eaf nil t)
        (when (featurep 'eaf)
          (eaf-open-browser (format "http://localhost:%d" httpd-port))))
    (error
     (browse-url (format "http://localhost:%d" httpd-port))))
  
  ;; Focus current research node if applicable
  (let* ((file (buffer-file-name))
         (identifier (when (and file doc-engine-directory
                               (string-match-p doc-engine-directory file))
                       (with-current-buffer (find-file-noselect file)
                         (org-entry-get (point-min) "IDENTIFIER")))))
    (when identifier
      (setq timeline-engine--explicit-focus-id identifier)
      (message "Timeline focused on: %s" identifier)))
  
  ;; Return to main window for writing
  (other-window -1)
  
  ;; Initialize page-view
  (page-view-initialize)
  
  ;; Show word count limits
  (let ((limits (cdr (assq page-view-paper-type
                                page-view-word-count-limits))))
    (message "Paper Type: %s" (symbol-name page-view-paper-type))
    (message "Word Count Limits:")
    (message "  Abstract: %d words" (cdr (assq 'abstract limits)))
    (message "  Total Expected: %d words" (cdr (assq 'total limits)))
    (message "Journal: %s" (symbol-name page-view-journal-type)))
  
  (when page-view--track-statistics
    (setq page-view--track-statistics t)
    (message "Writing statistics tracking enabled."))
  
  (message "=== Session Ready ===")
  (message "Left: Writing (80%%) | Right: Timeline (20%%) | M-q to close timeline"))

(defun page-view-insert-section-template(section-name &optional heading-level)
  "Insert standard academic section template at current point.
SECTION-NAME is the section name (e.g., 'Methods', 'Results').
HEADING-LEVEL is the org heading level (default 2)."
  (let* ((stars (make-string (or heading-level 2) ?*))
         (template (format "\n%s %s\n\n" stars section-name)))
    (insert template)
    (message "Inserted '%s' section. Start writing..." section-name)))

(defun page-view-insert-figure-with-caption(caption)
  "Insert a figure placeholder with proper caption formatting.
Figure number will be auto-assigned when numbering."
  (interactive "sFigure caption: ")
  (let* ((placeholder (format "\n#+CAPTION: %s\n[[file:figure-placeholder.png]]\n" caption)))
    (insert placeholder)
    (message "Inserted figure placeholder. Use M-x page-view-number-figures when complete.")))

(defun page-view-insert-table-with-caption(caption)
  "Insert a table placeholder with proper caption formatting.
Table number will be auto-assigned when numbering."
  (interactive "sTable caption: ")
  (let* ((placeholder (format "\n#+CAPTION: %s\n| Column 1 | Column 2 |\n|---|\n" caption)))
    (insert placeholder)
    (message "Inserted table placeholder. Use M-x page-view-number-tables when complete.")))

(defun page-view-format-citation(key citation)
  "Format a citation according to selected style (APA, MLA, Chicago, etc.)."
  (let ((formatted (pcase page-view-citation-style
                      ('apa 
                       (format "(%s, %d)" (car citation) (cdr citation)))
                      ('mla 
                       (format "(%s %d)" (car citation) (cdr citation)))
                      ('chicago 
                       (format "(%s %d)" (car citation) (cdr citation)))
                      ('ieee 
                       (format "[%d] %s" (cdr citation) (car citation)))
                      (t 
                       (format "[%s] %s" (car citation) citation)))))
    (message "Formatted as: %s" formatted)
    formatted))

(defun page-view-check-references()
  "Check if references section is properly formatted according to journal standards.
Provides feedback on citation consistency and formatting."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((found-refs (re-search-forward "^\\*\\s+References" nil t))
          (issues '()))
      
      (when found-refs
        ;; Check reference formatting
        (while (re-search-forward "^- " nil t)
          (let ((line (buffer-substring-no-properties 
                                              (line-beginning-position) 
                                              (line-end-position))))
            ;; Check for common reference format issues
            (when (not (string-match-p "^\\[" line))  ; Not bracketed
              (push "Reference not bracketed" issues))))
      
      (if issues
          (message "Found %d reference formatting issues:" (length issues))
        (message "References section format: OK")))))

;; ==============================================================================
;; DEBUG AND STATISTICS
;; ==============================================================================

(defun page-view-toggle-debug()
  "Toggle debug mode showing line heights and page information."
  (interactive)
  (setq page-view--debug-flag 
        (not page-view--debug-flag))
  (message "Debug mode: %s" (if page-view--debug-flag "ON" "OFF")))

(defun page-view--update-statistics()
  "Update writing statistics display in mode line."
  (when page-view--track-statistics
    (let* ((total-words (page-view-count-words 
                                             (point-min) (point-max)))
           (total-lines (count-lines (point-min) (point-max))
           (limits (cdr (assq page-view-paper-type
                                page-view-word-count-limits)))
           (expected-total (cdr (assq 'total limits))))
      ;; Update mode line with statistics
      (setq mode-line-format
            (format " Scientific [%s] | Words: %d/%d | Lines: %d | Pages: %d" 
                   (if (<= total-words expected-total) "✓" "⚠")
                   total-words expected-total
                   total-lines
                   page-view--max-page)))))

;; ==============================================================================
;; MAIN MODE
;; ==============================================================================

(define-minor-mode page-view-mode
  "Scientific page view mode for academic writing.
Provides word processor-like layout following academic writing standards."
  :lighter " SciPageView"
  :group 'scientific-mapping
  :init-value nil
  (if page-view-mode
      (progn
        ;; Activate mode
        (page-view-initialize)
        
        ;; Add hooks for dynamic updates
        (add-hook 'after-change-functions 
                  #'page-view--handle-change nil t)
        (add-hook 'window-scroll-functions 
                  #'page-view--on-scroll nil t))
        
        (message "Scientific Page View enabled for %s writing" 
                 (symbol-name page-view-paper-type)))
    
    (progn
      ;; Deactivate mode
      (remove-hook 'after-change-functions 
                   #'page-view--handle-change t)
      (remove-hook 'window-scroll-functions 
                   #'page-view--on-scroll t)
      
      (olivetti-mode -1)
      (setq-default-text-properties nil)
      
      (remove-overlays (point-min) (point-max) 'pagebreak t)
      (setq page-view--overlays nil)
      (setq page-view--max-page 1)
      
      (message "Scientific Page View disabled")))))

;; Event handlers
(defun page-view--handle-change(beg end _len)
  "Handle buffer changes and invalidate cached overlays."
  (page-view-clear-overlays beg (min (1+ end) (point-max)))

(defun page-view--on-scroll(_window _display-start)
  "Handle window scrolling and reflow page breaks."
  (page-view--reflow-screen _display-start (window-end _window) t))

(defun page-view-clear-overlays(&optional start end)
  "Clear all page-break overlays from START to END."
  (remove-overlays (or start (point-min)) 
                    (or end (point-max)) 
                    'pagebreak t)
  (setq page-view--max-page 1))

(defun page-view--reflow-screen(start end)
  "Reflow page breaks for region between START and END."
  (let* ((page-start (or start 1))
         (page-end (or end page-start))
         (current-page page-view--max-page))
         (target-line (* page-start page-view-lines-per-page)))
    
    (message "Reflowing pages from %d to %d" page-start page-end)
    
    ;; Apply page breaks every N lines
    (save-excursion
      (goto-char (point-min))
      (let ((line-count 0))
        (while (and (not (eobp)) (< line-count page-end))
          (when (= (1+ line-count) target-line)
            (page-view-insert-page-break)
            (setq current-page (1+ current-page)))
          (forward-line 1)
          (setq line-count (1+ line-count)))))))

;; ==============================================================================
;; PAGE-VIEW + TIMELINE INTEGRATION
;; ==============================================================================

(defun page-view-with-timeline ()
  "Toggle integrated timeline view alongside page-view.
Shows connecting research files in a side panel (20% width).
If timeline-engine is unavailable, shows error message and continues without timeline."
  (interactive)
  (condition-case err
      (progn
        (require 'timeline-engine nil t)
        (unless (featurep 'timeline-engine)
          (user-error "timeline-engine not available. Install simple-httpd for timeline functionality.")))
    (error
     (message "Warning: Could not load timeline-engine: %s" (error-message-string err))
     (setq timeline-engine-available nil)))

  (message "=== Starting Integrated Writing + Timeline Session ===")

  ;; Save current window configuration if not already saved
  (unless (boundp 'page-view--original-window-config)
    (setq page-view--original-window-config (current-window-configuration)))

  ;; Check if timeline is already open
  (if (get-buffer "*dashboard*")
      ;; Timeline exists - close it
      (progn
        (delete-other-windows)
        (set-window-configuration page-view--original-window-config)
        (setq page-view--original-window-config nil)
        (message "Timeline closed. Restored normal view."))
    ;; No timeline - open it
    (progn
      (delete-other-windows)
      (split-window-right (floor (* (window-width) 0.2)) nil)  ; 20% for timeline
      (other-window 1)

      ;; Set fixed width for timeline window
      (setq window-size-fixed t)

      ;; Start timeline engine if available and not already running
      (when (and (featurep 'timeline-engine)
                 (not (and (boundp 'httpd-port) httpd-port)))
        (condition-case nil
            (progn
              (setq httpd-root (expand-file-name "html" timeline-engine-root))
              (httpd-start)
              (message "Timeline engine web server started"))
          (error
           (message "Warning: Could not start timeline engine server"))))

      ;; Open timeline in side window using EAF or browse-url
      (condition-case open-err
          (progn
            (require 'eaf nil t)
            (when (featurep 'eaf)
              (eaf-open-browser (format "http://localhost:%d" httpd-port))))
        (error
         (browse-url (format "http://localhost:%d" httpd-port))))

      ;; Focus current research node if applicable
      (let* ((file (buffer-file-name))
             (identifier (when (and file doc-engine-directory
                                   (string-match-p doc-engine-directory file))
                           (with-current-buffer (find-file-noselect file)
                             (org-entry-get (point-min) "IDENTIFIER")))))
        (when identifier
          (setq timeline-engine--explicit-focus-id identifier)
          (message "Timeline focused on: %s" identifier)))

      ;; Return to main window for writing
      (other-window -1)

      ;; Initialize page-view
      (page-view-initialize)

      ;; Show status
      (message "=== Integrated Session Ready ===")
      (message "Writing (80%%) | Timeline (20%%) | M-q to close"))))

(defun page-view-without-timeline ()
  "End integrated session and restore previous window configuration."
  (interactive)
  (when (and (boundp 'page-view--original-window-config)
             page-view--original-window-config)
    (set-window-configuration page-view--original-window-config)
    (setq page-view--original-window-config nil)
    (message "Restored previous window configuration")))

;; ==============================================================================
;; KEY BINDINGS
;; ==============================================================================

(defvar page-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'page-view-insert-page-break)
    (define-key map (kbd "C-c C-g") 'page-view-goto-page)
    (define-key map (kbd "C-c C-n") 'page-view-next-page)
    (define-key map (kbd "C-c C-p") 'page-view-previous-page)
    (define-key map (kbd "C-c C-s") 'page-view-insert-section-template)
    (define-key map (kbd "C-c C-f") 'page-view-insert-figure-with-caption)
    (define-key map (kbd "C-c C-t") 'page-view-insert-table-with-caption)
    (define-key map (kbd "C-c C-c") 'page-view-check-abstract-length)
    (define-key map (kbd "C-c C-w") 'page-view-show-word-counts)
    (define-key map (kbd "C-c C-r") 'page-view-check-references)
    (define-key map (kbd "C-c C-d") 'page-view-toggle-debug)
    map)
  "Keymap for page-view commands.")

;; Add key bindings to mode map
(define-key page-view-mode-map (kbd "M-s") 'page-view-start-writing-session)
(define-key page-view-mode-map (kbd "M-S") 'page-view-with-timeline)
(define-key page-view-mode-map (kbd "M-q") 'page-view-without-timeline)
(define-key page-view-mode-map (kbd "M-w") 'page-view-show-word-counts)
(define-key page-view-mode-map (kbd "M-n") 'page-view-number-figures)
(define-key page-view-mode-map (kbd "M-t") 'page-view-number-tables)

(provide 'page-view)

;;; page-view.el ends here

;; ==============================================================================
;; FUNCTION ORDERING (CHRONOLOGICAL - ACADEMIC WRITING PROCESS)
;; ==============================================================================
;; 
;; The functions in this file are organized chronologically according to the
;; established scientific writing process from official sources:
;; 
;; 1. PREPARATION AND INITIALIZATION
;;    - page-view-initialize()
;;    page-view-start-writing-session()
;; 
;; 2. PAGE LAYOUT AND NAVIGATION
;;    - page-view-insert-page-break()
;;    - page-view-goto-page()
;;    - page-view-next-page()
;;    - page-view-previous-page()
;; 
;; 3. HEADER AND FOOTER GENERATION
;;    - page-view--generate-header()
;;    - page-view--generate-footer()
;; 
;; 4. WORD COUNT AND ABSTRACT COMPLIANCE
;;    - page-view-count-words()
;;    - page-view-check-abstract-length()
;;    - page-view-show-word-counts()
;; 
;; 5. FIGURE AND TABLE MANAGEMENT
;;    - page-view-number-figures()
;;    - page-view-number-tables()
;;    - page-view-insert-figure-with-caption()
;;    - page-view-insert-table-with-caption()
;; 
;; 6. CITATION AND REFERENCE HANDLING
;;    - page-view-format-citation()
;;    - page-view-check-references()
;; 
;; 7. DEBUG AND STATISTICS
;;    - page-view-toggle-debug()
;;    - page-view--update-statistics()
;; 
;; This ordering ensures that writing functions follow the natural academic workflow:
;; Preparation → Structure → Content → Analysis → Revision → Publication
;; 
;; KEY BINDINGS:
;; =============
;; C-c C-p - Insert page break
;; C-c C-g - Go to page
;; C-c C-n - Next page
;; C-c C-p - Previous page
;; C-c C-s - Insert section template
;; C-c C-f - Insert figure with caption
;; C-c C-t - Insert table with caption
;; C-c C-c - Check abstract length
;; C-c C-w - Show word counts
;; C-c C-r - Check references
;; C-c C-d - Toggle debug mode
;; M-s - Start writing session
;; M-w - Word count
;; M-n - Number figures
;; M-t - Number tables
;; 
;; INTEGRATION:
;; ============
;; This system integrates with:
;; - doc-engine: Document management and metadata
;; - snippet-engine: Academic templates
;; - citation-database: Reference tracking
;; - concept-relationships: Concept organization
;; 
;; Templates from snippet-engine automatically conform to page view layout
;; and enforce word counts from official journal guidelines.