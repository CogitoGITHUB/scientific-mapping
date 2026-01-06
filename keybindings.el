;;; keybindings.el --- Centralized key bindings for scientific mapping -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

;;; Commentary:

;; This file centralizes all key bindings for the scientific mapping system.
;; All key bindings are defined here to prevent duplication and make maintenance easier.
;; This is the single source of truth for all scientific-mapping key bindings.

;;; Code:

(require 'scientific-mapping)
(require 'ai-integration nil t)
(require 'academic-apis nil t)
(require 'reference-managers nil t)

;;;; Core Key Binding Setup

(defvar scientific-mapping-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Core system commands
    (define-key map (kbd "C-c s s") 'scientific-mapping-start)
    (define-key map (kbd "C-c s S") 'scientific-mapping-stop)
    (define-key map (kbd "C-c s ?") 'scientific-mapping-help)
    (define-key map (kbd "C-c s h") 'scientific-mapping-status)

    ;; Document management
    (define-key map (kbd "C-c s i") 'scientific-mapping-import-paper)
    (define-key map (kbd "C-c s q") 'doc-engine-quick-note)
    (define-key map (kbd "C-c s m") 'doc-engine-smart-import)
    (define-key map (kbd "C-c s c") 'concept-relationships-create-entry)
    (define-key map (kbd "C-c s v") 'viz-engine-open)
    (define-key map (kbd "C-c s n") 'viz-engine-set-mode)

    ;; Research workflows
    (define-key map (kbd "C-c s r") 'scientific-mapping-literature-review)
    (define-key map (kbd "C-c s b") 'scientific-mapping-backup)
    (define-key map (kbd "C-c s t") 'timeline-engine-open)
    (define-key map (kbd "C-c s e") 'concept-tree-toggle-section)

    ;; Org agenda integration
    (define-key map (kbd "C-c s a") 'scientific-mapping-agenda)
    (define-key map (kbd "C-c s p") 'scientific-mapping-schedule-paper-review)
    (define-key map (kbd "C-c s d") 'scientific-mapping-schedule-deadline)
    (define-key map (kbd "C-c s P") 'scientific-mapping-create-research-project)
    (define-key map (kbd "C-c s w") 'scientific-mapping-weekly-review)
    (define-key map (kbd "C-c s A") 'scientific-mapping-agenda-from-document)

    ;; Academic API integrations
    (define-key map (kbd "C-c s X") 'academic-apis-search-arxiv)
    (define-key map (kbd "C-c s M") 'academic-apis-search-pubmed)
    (define-key map (kbd "C-c s D") 'academic-apis-resolve-doi)
    (define-key map (kbd "C-c s I") 'academic-apis-import-to-scientific-mapping)

    ;; Reference manager integration
    (define-key map (kbd "C-c s Z") 'reference-managers-zotero-get-collections)
    (define-key map (kbd "C-c s B") 'reference-managers-import-bibtex)
    (define-key map (kbd "C-c s R") 'reference-managers-import-to-scientific-mapping)

    ;; AI integration (sub-prefix)
    (let ((ai-map (make-sparse-keymap)))
      (define-key ai-map (kbd "a") 'ai-integration-analyze-document)
      (define-key ai-map (kbd "s") 'ai-integration-summarize-document)
      (define-key ai-map (kbd "q") 'ai-integration-ask-question)
      (define-key ai-map (kbd "c") 'ai-integration-extract-concepts)
      (define-key ai-map (kbd "g") 'ai-integration-detect-research-gaps)
      (define-key ai-map (kbd "r") 'ai-integration-research-assistance)
      (define-key ai-map (kbd "w") 'ai-integration-writing-assistance)
      (define-key ai-map (kbd "i") 'ai-integration-analyze-citations)
      (define-key ai-map (kbd "e") 'ai-integration-semantic-search)
      (define-key ai-map (kbd "b") 'ai-integration-batch-analyze)
      (define-key map (kbd "C-c s A") ai-map))

    ;; Workflow integration
    (define-key map (kbd "C-c s W") 'workflow-integration-show-workflow-status)
    (define-key map (kbd "C-c s U") 'workflow-integration-unified-search)
    (define-key map (kbd "C-c s F") 'workflow-integration-import-paper-complete)
    (define-key map (kbd "C-c s R") 'workflow-integration-frictionless-research-session)
    (define-key map (kbd "C-c s P") 'workflow-integration-smart-process-buffer)

    ;; Data synchronization
    (define-key map (kbd "C-c s Y") 'data-synchronization-full-sync)
    (define-key map (kbd "C-c s C") 'data-synchronization-consistency-check)

    ;; Context awareness
    (define-key map (kbd "C-c s S") 'context-awareness-show-suggestions)
    (define-key map (kbd "C-c s Q") 'context-awareness-quick-actions)

    map)
  "Keymap for scientific-mapping-mode with all centralized key bindings.")

;;;; Key Binding Management Functions

(defun scientific-mapping-add-key-binding (key command)
  "Add a key binding to the scientific mapping keymap.
KEY should be a key sequence string like \"C-c s x\"
COMMAND should be a symbol naming the command function."
  (define-key scientific-mapping-mode-map (kbd key) command))

(defun scientific-mapping-remove-key-binding (key)
  "Remove a key binding from the scientific mapping keymap.
KEY should be a key sequence string like \"C-c s x\""
  (define-key scientific-mapping-mode-map (kbd key) nil))

(defun scientific-mapping-list-key-bindings ()
  "List all current key bindings in scientific-mapping-mode."
  (interactive)
  (let ((buffer (get-buffer-create "*Scientific Mapping Key Bindings*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# Scientific Mapping Key Bindings\n\n")
      (insert "All key bindings are available under the `C-c s` prefix.\n\n")

      ;; Core system
      (insert "## Core System\n")
      (insert "- `C-c s s`: Start scientific mapping system\n")
      (insert "- `C-c s S`: Stop scientific mapping system\n")
      (insert "- `C-c s ?`: Show help\n")
      (insert "- `C-c s h`: Show system status\n\n")

      ;; Document management
      (insert "## Document Management\n")
      (insert "- `C-c s i`: Import paper from DOI\n")
      (insert "- `C-c s c`: Create concept entry\n")
      (insert "- `C-c s v`: Open 3D visualization\n")
      (insert "- `C-c s n`: Set visualization mode\n\n")

      ;; Research workflows
      (insert "## Research Workflows\n")
      (insert "- `C-c s r`: Generate literature review\n")
      (insert "- `C-c s b`: Create system backup\n")
      (insert "- `C-c s t`: Open timeline view\n")
      (insert "- `C-c s e`: Toggle concept tree section\n\n")

      ;; Agenda integration
      (insert "## Agenda Integration\n")
      (insert "- `C-c s a`: Open research agenda\n")
      (insert "- `C-c s p`: Schedule paper review\n")
      (insert "- `C-c s d`: Schedule deadline\n")
      (insert "- `C-c s P`: Create research project\n")
      (insert "- `C-c s w`: Weekly progress review\n")
      (insert "- `C-c s A`: Create agenda from document\n\n")

      ;; Academic APIs
      (insert "## Academic APIs\n")
      (insert "- `C-c s X`: Search arXiv\n")
      (insert "- `C-c s M`: Search PubMed\n")
      (insert "- `C-c s D`: Resolve DOI\n")
      (insert "- `C-c s I`: Import from academic APIs\n\n")

      ;; Reference managers
      (insert "## Reference Managers\n")
      (insert "- `C-c s Z`: Sync Zotero collections\n")
      (insert "- `C-c s B`: Import BibTeX file\n")
      (insert "- `C-c s R`: Import from reference managers\n\n")

      ;; AI integration
      (insert "## AI Integration (C-c s A prefix)\n")
      (insert "- `C-c s A a`: Analyze document\n")
      (insert "- `C-c s A s`: Summarize document\n")
      (insert "- `C-c s A q`: Ask AI question\n")
      (insert "- `C-c s A c`: Extract concepts\n")
      (insert "- `C-c s A g`: Detect research gaps\n")
      (insert "- `C-c s A r`: Research assistance\n")
      (insert "- `C-c s A w`: Writing assistance\n")
      (insert "- `C-c s A i`: Analyze citations\n")
      (insert "- `C-c s A e`: Semantic search\n")
      (insert "- `C-c s A b`: Batch analyze directory\n\n")

      (insert "## 3D Visualization Controls\n")
      (insert "See KEYBINDINGS.md for complete 3D control reference.\n\n")

      (insert "## Notes\n")
      (insert "- All bindings require `scientific-mapping-mode` to be active\n")
      (insert "- Some commands work on the current buffer/document\n")
      (insert "- AI commands require configured LLM providers\n")
      (insert "- Academic API commands require internet connection\n")

      (goto-char (point-min))
      (org-mode))
    (display-buffer buffer)))

;;;; Key Binding Customization

(defcustom scientific-mapping-enable-keybindings t
  "If non-nil, enable the centralized key binding system."
  :group 'scientific-mapping
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (when (and value (boundp 'scientific-mapping-mode))
           (scientific-mapping-setup-keybindings))))

(defun scientific-mapping-setup-keybindings ()
  "Setup all key bindings for scientific mapping."
  (when scientific-mapping-enable-keybindings
    ;; The keymap is already defined above, just ensure it's active
    (message "Scientific mapping key bindings activated")))

;;;; Initialization

;; Setup key bindings when this file is loaded
(scientific-mapping-setup-keybindings)

(provide 'keybindings)

;;; keybindings.el ends here

;;; Usage:
;;
;; This file centralizes all key bindings for the scientific mapping system.
;; To modify key bindings, edit this file rather than individual component files.
;; Use the helper functions:
;; - (scientific-mapping-add-key-binding "C-c s x" 'my-command)
;; - (scientific-mapping-remove-key-binding "C-c s x")
;; - (scientific-mapping-list-key-bindings) to see all bindings