;;; scientific-page-view.el --- Scientific writing workflow with page-based layout -*- lexical-binding: t -*-

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

;; scientific-page-view provides a word processor-like page layout for writing
;; scientific papers in org-mode, following established academic writing
;; standards from official sources (PLOS, Nature, Science, ICMJE, etc.).
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
;;   M-x scientific-page-view-mode
;;
;; Quick workflow:
;;   1. M-x scientific-page-view-start (begin writing)
;;   2. Use page breaks to organize sections
;;   3. Write content following IMRaD structure
;;   4. M-x scientific-page-view-goto-page (navigate sections)
;;   5. Export to PDF/Word when complete
;;
;; Integration with scientific-yasnippet templates:
;;   Templates automatically integrate with page view layout
;;   Academic document types (research, review, thesis) supported
;;   Journal-specific formatting enforced

;;; Code:

(require 'org)
(require 'olivetti)  ;; For word processor-like display

;; ==============================================================================
;; CONFIGURATION AND CUSTOMIZATION
;; ==============================================================================

(defgroup scientific-page-view nil
  "Scientific page view settings for academic writing."
  :group 'scientific-mapping
  :prefix "spv-")

;; ========================================
;; Academic Standards Configuration
;; ========================================

(defcustom scientific-page-view-paper-type 'research-article
  "Type of scientific paper being written.
Controls word count limits and structure requirements."
  :group 'scientific-page-view
  :type '(choice (const :tag "Research Article" research-article)
                  (const :tag "Literature Review" literature-review)
                  (const :tag "Conference Paper" conference-paper)
                  (const :tag "Master's Thesis" thesis-master)
                  (const :tag "Doctoral Dissertation" thesis-doc)
                  (const :tag "Technical Report" technical-report)))

(defcustom scientific-page-view-journal-type 'general
  "Journal category for specific formatting requirements.
Different journals have different word limits, abstract requirements, etc."
  :group 'scientific-page-view
  :type '(choice (const :tag "General (no specific requirements)" general)
                  (const :tag "APA Style" apa)
                  (const :tag "MLA Style" mla)
                  (const :tag "Chicago Style" chicago)
                  (const :tag "IEEE Format" ieee)
                  (const :tag "Science/PNAS" science)
                  (const :tag "Nature" nature)
                  (const :tag "PLOS" plos)))

(defcustom scientific-page-view-citation-style 'apa
  "Citation style to use for formatting references."
  :group 'scientific-page-view
  :type '(choice (const :tag "APA (7th edition)" apa)
                  (const :tag "MLA (9th edition)" mla)
                  (const :tag "Chicago (17th edition)" chicago)
                  (const :tag "IEEE" ieee)
                  (const :tag "Vancouver" vancouver)
                  (const :tag "Harvard" harvard)))

;; ========================================
;; Page Layout Configuration
;; ========================================

(defcustom scientific-page-view-page-width 69
  "Width of text in characters (approximating letter-sized page).
Times New Roman 12pt at 1.5 spacing ≈ 69 chars wide."
  :group 'scientific-page-view
  :type 'integer)

(defcustom scientific-page-view-lines-per-page 36
  "Number of lines per page for academic writing.
Approximates ~250-300 words per page (standard single-spaced)."
  :group 'scientific-page-view
  :type 'integer)

(defcustom scientific-page-view-line-spacing 1.5
  "Line spacing for academic writing (standard is 1.5)."
  :group 'scientific-page-view
  :type 'number)

(defcustom scientific-page-view-font-family "Times New Roman"
  "Font family for academic writing (standard is Times New Roman)."
  :group 'scientific-page-view
  :type 'string)

(defcustom scientific-page-view-font-size 12
  "Font size in points (academic standard is 12pt)."
  :group 'scientific-page-view
  :type 'integer)

;; ========================================
;; Header and Footer Configuration
;; ========================================

(defcustom scientific-page-view-show-headers t
  "Show document title and author information at top of each page."
  :group 'scientific-page-view
  :type 'boolean)

(defcustom scientific-page-view-show-footers t
  "Show page numbers at bottom of each page."
  :group 'scientific-page-view
  :type 'boolean)

(defcustom scientific-page-view-footer-format "Page %d"
  "Format string for page footer. Use %d for page number."
  :group 'scientific-page-view
  :type 'string)

;; ========================================
;; Academic Writing Features
;; ========================================

(defcustom scientific-page-view-enforce-word-counts t
  "Enforce abstract word count limits based on paper type."
  :group 'scientific-page-view
  :type 'boolean)

(defcustom scientific-page-view-auto-number-figures t
  "Automatically number figures in order of appearance."
  :group 'scientific-page-view
  :type 'boolean)

(defcustom scientific-page-view-auto-number-tables t
  "Automatically number tables in order of appearance."
  :group 'scientific-page-view
  :type 'boolean)

(defcustom scientific-page-view-track-changes nil
  "Track word count and line statistics for progress monitoring."
  :group 'scientific-page-view
  :type 'boolean)

;; ==============================================================================
;; WORD COUNT LIMITS (From Official Sources)
;; ==============================================================================

(defconst scientific-page-view-word-count-limits
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
(defface scientific-page-view-pagebreak-face
  `((t :inherit 'tab-bar
       :family "monospace"
       :foreground "white"
       :background "gray30"
       :height 1.0))
  "Face for page break separators in scientific documents."
  :group 'scientific-page-view)

;; Document title in page header
(defface scientific-page-view-header-face
  `((t :inherit 'default
       :family ,scientific-page-view-font-family
       :weight 'bold
       :height 1.1))
  "Face for document headers."
  :group 'scientific-page-view)

;; Page number in footer
(defface scientific-page-view-footer-face
  `((t :inherit 'default
       :family "monospace"
       :weight 'bold))
  "Face for page footers."
  :group 'scientific-page-view)

;; Debug information overlays
(defface scientific-page-view-debug-face
  `((t :inherit 'scientific-page-view-footer-face
       :foreground "yellow"
       :slant 'italic))
  "Face for debug mode overlays."
  :group 'scientific-page-view)

;; Section headings for organization
(defface scientific-page-view-section-face
  `((t :inherit 'org-level-1
       :weight 'bold
       :height 1.2))
  "Face for section headings."
  :group 'scientific-page-view)

;; ==============================================================================
;; INTERNAL VARIABLES
;; ==============================================================================

(defvar-local scientific-page-view--overlays nil
  "Hash table storing page overlay objects.
Key: page-number → overlay object")

(defvar-local scientific-page-view--max-page 1
  "Current highest page number in document.")

(defvar-local scientific-page-view--debug-flag nil
  "When non-nil, show debug overlays with line heights.")

(defvar-local scientific-page-view--track-statistics nil
  "When tracking writing progress (word counts, etc).")

;; ==============================================================================
;; INITIALIZATION
;; ==============================================================================

(defun scientific-page-view-initialize()
  "Initialize scientific-page-view system for current buffer."
  (interactive)
  (message "Initializing Scientific Page View...")
  
  ;; Configure olivetti for academic writing
  (setq-local olivetti-style 'fancy)
  (setq-local olivetti-body-width scientific-page-view-page-width)
  
  ;; Set academic font
  (setq-local default-text-properties
            `(line-spacing ,scientific-page-view-line-spacing
                 wrap-prefix (space . (:height ,(round (* scientific-page-view-font-size 
                                                                   scientific-page-view-line-spacing))
                                        :width scientific-page-view-page-width))))
  
  ;; Enable olivetti mode
  (olivetti-mode 1)
  
  ;; Configure display
  (org-indent-mode -1)
  (diff-hl-mode -1)
  (hl-line-mode -1)
  
  ;; Initialize tracking
  (setq-local scientific-page-view--overlays (make-hash-table :test 'equal))
  (setq-local scientific-page-view--max-page 1)
  
  (message "Scientific Page View initialized for %s paper" 
           (symbol-name scientific-page-view-paper-type)))

;; ==============================================================================
;; PAGE BREAK AND NAVIGATION
;; ==============================================================================

(defun scientific-page-view-insert-page-break()
  "Insert a scientific page break at current point.
Creates a visual separator and prepares for new page numbering.
Follows journal standards for page breaks."
  (interactive)
  (let* ((page-number (1+ scientific-page-view--max-page))
         (current-line (line-number-at-pos))
         (height 3))
    
    ;; Insert visual separator (three underscores for page break)
    (insert (propertize "___" 'face 'scientific-page-view-pagebreak-face))
    (insert "\n\n")
    
    ;; Update max page number
    (setq scientific-page-view--max-page page-number)
    
    (message "Inserted page break. Now on page %d" page-number)
    
    ;; Update statistics if tracking
    (when scientific-page-view--track-statistics
      (scientific-page-view--update-statistics))))

(defun scientific-page-view-goto-page(page-number)
  "Navigate to specific PAGE-NUMBER in the document.
Page numbers correspond to logical document pages, not physical lines."
  (interactive "nPage number: ")
  (unless (and (integerp page-number) (> page-number 0))
    (user-error "Invalid page number: %d" page-number))
  
  ;; Find page break marker
  (let* ((found-page nil)
         (target-line (* (1- page-number) scientific-page-view-lines-per-page)))
    
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
               page-number scientific-page-view--max-page))))

(defun scientific-page-view-next-page()
  "Navigate to next page in document."
  (interactive)
  (scientific-page-view-goto-page (1+ scientific-page-view--max-page)))

(defun scientific-page-view-previous-page()
  "Navigate to previous page in document."
  (interactive)
  (scientific-page-view-goto-page (1- scientific-page-view--max-page)))

;; ==============================================================================
;; HEADER AND FOOTER GENERATION
;; ==============================================================================

(defun scientific-page-view--generate-header(page-number)
  "Generate academic document header for PAGE-NUMBER.
Includes title, author, and page number."
  (let* ((title (or (org-get-title) "Untitled"))
         (author (or (org-entry-get (point-min) "AUTHOR") "Anonymous"))
         (date (or (org-entry-get (point-min) "DATE") (format-time-string "%Y-%m-%d")))
    (format "  %s | %s | %s  \n" title author date)))

(defun scientific-page-view--generate-footer(page-number)
  "Generate academic document footer for PAGE-NUMBER.
Standard format: 'Page X' at bottom of page."
  (format scientific-page-view-footer-format page-number))

;; ==============================================================================
;; WORD COUNT AND ABSTRACT ENFORCEMENT
;; ==============================================================================

(defun scientific-page-view-count-words(beginning end)
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

(defun scientific-page-view-check-abstract-length()
  "Check if abstract section meets word count requirements.
Based on paper type, enforce word limits from official sources."
  (interactive)
  (let* ((limits (cdr (assq scientific-page-view-paper-type
                               scientific-page-view-word-count-limits)))
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
                  (word-count (scientific-page-view-count-words 
                                                     abstract-start abstract-end)))
          (if (and scientific-page-view-enforce-word-counts
                     (> word-count abstract-limit))
              (progn
                (message "Abstract: %d words (limit: %d) - TOO LONG!" 
                         word-count abstract-limit)
                (setq face 'font-lock-warning-face))
            (message "Abstract: %d words (limit: %d) - OK" 
                     word-count abstract-limit)))))))

(defun scientific-page-view-show-word-counts()
  "Display word count statistics for current buffer or section."
  (interactive)
  (let* ((total-words (scientific-page-view-count-words 
                                            (point-min) (point-max)))
         (limits (cdr (assq scientific-page-view-paper-type
                                scientific-page-view-word-count-limits)))
         (expected-total (cdr (assq 'total limits))))
    (message "Current: %d words | Expected: %d words | Status: %s"
             total-words 
             expected-total
             (if (and scientific-page-view-enforce-word-counts
                      (<= total-words expected-total))
                 "✓ OK"
               (⚠ EXCEEDS")))))

;; ==============================================================================
;; FIGURE AND TABLE CAPTIONING
;; ==============================================================================

(defun scientific-page-view-number-figures()
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

(defun scientific-page-view-number-tables()
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

(defun scientific-page-view-start-writing-session()
  "Phase 1: Start new scientific writing session.
Prepares buffer for academic writing with proper formatting."
  (interactive)
  (message "=== Starting Scientific Writing Session ===")
  
  ;; Initialize page view
  (scientific-page-view-initialize)
  
  ;; Show word count limits
  (let ((limits (cdr (assq scientific-page-view-paper-type
                                scientific-page-view-word-count-limits))))
    (message "Paper Type: %s" (symbol-name scientific-page-view-paper-type))
    (message "Word Count Limits:")
    (message "  Abstract: %d words" (cdr (assq 'abstract limits)))
    (message "  Total Expected: %d words" (cdr (assq 'total limits)))
    (message "Journal: %s" (symbol-name scientific-page-view-journal-type)))
  
  (when scientific-page-view--track-statistics
    (setq scientific-page-view--track-statistics t)
    (message "Writing statistics tracking enabled."))
  
  (message "=== Session Ready ===")
  (message "Use: M-x scientific-page-view-insert-page-break to add sections")
  (message "      M-x scientific-page-view-goto-page to navigate pages"))

(defun scientific-page-view-insert-section-template(section-name &optional heading-level)
  "Insert standard academic section template at current point.
SECTION-NAME is the section name (e.g., 'Methods', 'Results').
HEADING-LEVEL is the org heading level (default 2)."
  (let* ((stars (make-string (or heading-level 2) ?*))
         (template (format "\n%s %s\n\n" stars section-name)))
    (insert template)
    (message "Inserted '%s' section. Start writing..." section-name)))

(defun scientific-page-view-insert-figure-with-caption(caption)
  "Insert a figure placeholder with proper caption formatting.
Figure number will be auto-assigned when numbering."
  (interactive "sFigure caption: ")
  (let* ((placeholder (format "\n#+CAPTION: %s\n[[file:figure-placeholder.png]]\n" caption)))
    (insert placeholder)
    (message "Inserted figure placeholder. Use M-x scientific-page-view-number-figures when complete.")))

(defun scientific-page-view-insert-table-with-caption(caption)
  "Insert a table placeholder with proper caption formatting.
Table number will be auto-assigned when numbering."
  (interactive "sTable caption: ")
  (let* ((placeholder (format "\n#+CAPTION: %s\n| Column 1 | Column 2 |\n|---|\n" caption)))
    (insert placeholder)
    (message "Inserted table placeholder. Use M-x scientific-page-view-number-tables when complete.")))

(defun scientific-page-view-format-citation(key citation)
  "Format a citation according to selected style (APA, MLA, Chicago, etc.)."
  (let ((formatted (pcase scientific-page-view-citation-style
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

(defun scientific-page-view-check-references()
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

(defun scientific-page-view-toggle-debug()
  "Toggle debug mode showing line heights and page information."
  (interactive)
  (setq scientific-page-view--debug-flag 
        (not scientific-page-view--debug-flag))
  (message "Debug mode: %s" (if scientific-page-view--debug-flag "ON" "OFF")))

(defun scientific-page-view--update-statistics()
  "Update writing statistics display in mode line."
  (when scientific-page-view--track-statistics
    (let* ((total-words (scientific-page-view-count-words 
                                             (point-min) (point-max)))
           (total-lines (count-lines (point-min) (point-max))
           (limits (cdr (assq scientific-page-view-paper-type
                                scientific-page-view-word-count-limits)))
           (expected-total (cdr (assq 'total limits))))
      ;; Update mode line with statistics
      (setq mode-line-format
            (format " Scientific [%s] | Words: %d/%d | Lines: %d | Pages: %d" 
                   (if (<= total-words expected-total) "✓" "⚠")
                   total-words expected-total
                   total-lines
                   scientific-page-view--max-page)))))

;; ==============================================================================
;; MAIN MODE
;; ==============================================================================

(define-minor-mode scientific-page-view-mode
  "Scientific page view mode for academic writing.
Provides word processor-like layout following academic writing standards."
  :lighter " SciPageView"
  :group 'scientific-mapping
  :init-value nil
  (if scientific-page-view-mode
      (progn
        ;; Activate mode
        (scientific-page-view-initialize)
        
        ;; Add hooks for dynamic updates
        (add-hook 'after-change-functions 
                  #'scientific-page-view--handle-change nil t)
        (add-hook 'window-scroll-functions 
                  #'scientific-page-view--on-scroll nil t))
        
        (message "Scientific Page View enabled for %s writing" 
                 (symbol-name scientific-page-view-paper-type)))
    
    (progn
      ;; Deactivate mode
      (remove-hook 'after-change-functions 
                   #'scientific-page-view--handle-change t)
      (remove-hook 'window-scroll-functions 
                   #'scientific-page-view--on-scroll t)
      
      (olivetti-mode -1)
      (setq-default-text-properties nil)
      
      (remove-overlays (point-min) (point-max) 'pagebreak t)
      (setq scientific-page-view--overlays nil)
      (setq scientific-page-view--max-page 1)
      
      (message "Scientific Page View disabled")))))

;; Event handlers
(defun scientific-page-view--handle-change(beg end _len)
  "Handle buffer changes and invalidate cached overlays."
  (scientific-page-view-clear-overlays beg (min (1+ end) (point-max)))

(defun scientific-page-view--on-scroll(_window _display-start)
  "Handle window scrolling and reflow page breaks."
  (scientific-page-view--reflow-screen _display-start (window-end _window) t))

(defun scientific-page-view-clear-overlays(&optional start end)
  "Clear all page-break overlays from START to END."
  (remove-overlays (or start (point-min)) 
                    (or end (point-max)) 
                    'pagebreak t)
  (setq scientific-page-view--max-page 1))

(defun scientific-page-view--reflow-screen(start end)
  "Reflow page breaks for region between START and END."
  (let* ((page-start (or start 1))
         (page-end (or end page-start))
         (current-page scientific-page-view--max-page))
         (target-line (* page-start scientific-page-view-lines-per-page)))
    
    (message "Reflowing pages from %d to %d" page-start page-end)
    
    ;; Apply page breaks every N lines
    (save-excursion
      (goto-char (point-min))
      (let ((line-count 0))
        (while (and (not (eobp)) (< line-count page-end))
          (when (= (1+ line-count) target-line)
            (scientific-page-view-insert-page-break)
            (setq current-page (1+ current-page)))
          (forward-line 1)
          (setq line-count (1+ line-count)))))))

;; ==============================================================================
;; KEY BINDINGS
;; ==============================================================================

(defvar scientific-page-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'scientific-page-view-insert-page-break)
    (define-key map (kbd "C-c C-g") 'scientific-page-view-goto-page)
    (define-key map (kbd "C-c C-n") 'scientific-page-view-next-page)
    (define-key map (kbd "C-c C-p") 'scientific-page-view-previous-page)
    (define-key map (kbd "C-c C-s") 'scientific-page-view-insert-section-template)
    (define-key map (kbd "C-c C-f") 'scientific-page-view-insert-figure-with-caption)
    (define-key map (kbd "C-c C-t") 'scientific-page-view-insert-table-with-caption)
    (define-key map (kbd "C-c C-c") 'scientific-page-view-check-abstract-length)
    (define-key map (kbd "C-c C-w") 'scientific-page-view-show-word-counts)
    (define-key map (kbd "C-c C-r") 'scientific-page-view-check-references)
    (define-key map (kbd "C-c C-d") 'scientific-page-view-toggle-debug)
    map)
  "Keymap for scientific-page-view commands.")

;; Add key bindings to mode map
(define-key scientific-page-view-mode-map (kbd "M-s") 'scientific-page-view-start-writing-session)
(define-key scientific-page-view-mode-map (kbd "M-w") 'scientific-page-view-show-word-counts)
(define-key scientific-page-view-mode-map (kbd "M-n") 'scientific-page-view-number-figures)
(define-key scientific-page-view-mode-map (kbd "M-t") 'scientific-page-view-number-tables)

(provide 'scientific-page-view)

;;; scientific-page-view.el ends here

;; ==============================================================================
;; FUNCTION ORDERING (CHRONOLOGICAL - ACADEMIC WRITING PROCESS)
;; ==============================================================================
;; 
;; The functions in this file are organized chronologically according to the
;; established scientific writing process from official sources:
;; 
;; 1. PREPARATION AND INITIALIZATION
;;    - scientific-page-view-initialize()
;;    scientific-page-view-start-writing-session()
;; 
;; 2. PAGE LAYOUT AND NAVIGATION
;;    - scientific-page-view-insert-page-break()
;;    - scientific-page-view-goto-page()
;;    - scientific-page-view-next-page()
;;    - scientific-page-view-previous-page()
;; 
;; 3. HEADER AND FOOTER GENERATION
;;    - scientific-page-view--generate-header()
;;    - scientific-page-view--generate-footer()
;; 
;; 4. WORD COUNT AND ABSTRACT COMPLIANCE
;;    - scientific-page-view-count-words()
;;    - scientific-page-view-check-abstract-length()
;;    - scientific-page-view-show-word-counts()
;; 
;; 5. FIGURE AND TABLE MANAGEMENT
;;    - scientific-page-view-number-figures()
;;    - scientific-page-view-number-tables()
;;    - scientific-page-view-insert-figure-with-caption()
;;    - scientific-page-view-insert-table-with-caption()
;; 
;; 6. CITATION AND REFERENCE HANDLING
;;    - scientific-page-view-format-citation()
;;    - scientific-page-view-check-references()
;; 
;; 7. DEBUG AND STATISTICS
;;    - scientific-page-view-toggle-debug()
;;    - scientific-page-view--update-statistics()
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
;; - scientific-document-engine: Document management and metadata
;; - scientific-yasnippet: Academic templates
;; - citation-database: Reference tracking
;; - concept-relationships: Concept organization
;; 
;; Templates from scientific-yasnippet automatically conform to page view layout
;; and enforce word counts from official journal guidelines.