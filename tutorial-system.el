;;; tutorial-system.el --- Interactive tutorials for scientific mapping -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

;;; Commentary:

;; This file provides an interactive tutorial system that guides users through
;; the scientific mapping features with step-by-step instructions and examples.

;;; Code:

(require 'scientific-mapping)
(require 'org)
(require 'cl-lib)

(defgroup tutorial-system ()
  "Interactive tutorial system."
  :group 'scientific-mapping)

(defvar tutorial-system-current-tutorial nil
  "Currently active tutorial.")

(defvar tutorial-system-tutorial-step 0
  "Current step in the tutorial.")

(defvar tutorial-system-tutorials
  '(("Getting Started" . tutorial-system-getting-started)
    ("Document Management" . tutorial-system-document-management)
    ("AI Integration" . tutorial-system-ai-integration)
    ("3D Visualization" . tutorial-system-3d-visualization)
    ("Research Workflows" . tutorial-system-research-workflows)
    ("Advanced Features" . tutorial-system-advanced-features))
  "Available tutorials with their functions.")

;;;###autoload
(defun tutorial-system-start-tutorial (tutorial-name)
  "Start the tutorial named TUTORIAL-NAME."
  (interactive
   (list (completing-read "Choose tutorial: "
                         (mapcar #'car tutorial-system-tutorials)
                         nil t)))
  (let ((tutorial-fn (cdr (assoc tutorial-name tutorial-system-tutorials))))
    (when tutorial-fn
      (setq tutorial-system-current-tutorial tutorial-name)
      (setq tutorial-system-tutorial-step 0)
      (funcall tutorial-fn))))

;;;###autoload
(defun tutorial-system-interactive-tutorial ()
  "Start an interactive tutorial selection."
  (interactive)
  (let ((buffer (get-buffer-create "*Scientific Mapping Tutorials*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Scientific Mapping Tutorials
#+OPTIONS: toc:nil

* 🎓 Interactive Tutorials

Choose a tutorial to learn about different aspects of the scientific mapping system:

** 📚 Getting Started
Learn the basics of scientific mapping and create your first research document.
- Duration: 5 minutes
- Difficulty: Beginner
- Covers: Basic setup, first document, core concepts

** 📄 Document Management
Master document creation, organization, and management.
- Duration: 10 minutes
- Difficulty: Beginner
- Covers: Quick notes, imports, file organization, metadata

** 🤖 AI Integration
Discover AI-powered analysis and research assistance.
- Duration: 15 minutes
- Difficulty: Intermediate
- Covers: Document analysis, question answering, concept extraction

** 🎨 3D Visualization
Explore your knowledge in interactive 3D visualizations.
- Duration: 10 minutes
- Difficulty: Intermediate
- Covers: 3D navigation, node interaction, visualization modes

** 🔄 Research Workflows
Learn complete research project management and automation.
- Duration: 15 minutes
- Difficulty: Advanced
- Covers: Agenda integration, project planning, progress tracking

** ⚡ Advanced Features
Master advanced features and customization options.
- Duration: 20 minutes
- Difficulty: Advanced
- Covers: API integrations, reference managers, customization

* 🚀 Quick Start
If you're new to scientific mapping, start with \"Getting Started\" above.

* 🎯 Pro Tips
- Tutorials include hands-on exercises
- You can pause and resume at any time
- All tutorials work with keyboard-only navigation
- Examples use real research scenarios

* 📞 Need Help?
- Press ~C-c s ?~ for general help
- Use ~C-c s K~ for keyboard reference
- Visit ~C-c s S~ for smart suggestions

Ready to start learning? Choose a tutorial from the list above!
")
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; Tutorial Implementations

(defun tutorial-system-getting-started ()
  "Getting Started tutorial."
  (let ((steps
         '(("Welcome to Scientific Mapping!"
            "This tutorial will guide you through your first steps with the scientific mapping system."
            "Press any key to continue..."
            nil)

           ("Step 1: Enable the System"
            "First, let's enable the scientific mapping system."
            "Run: M-x scientific-mapping-start"
            "scientific-mapping-start")

           ("Step 2: Create Your First Document"
            "Now create your first research document using the quick note feature."
            "Run: C-c s q"
            "doc-engine-quick-note")

           ("Step 3: Enter a Title"
            "When prompted, enter a title for your research document."
            "Example: 'Machine Learning in Scientific Research'"
            nil)

           ("Step 4: View Your Document"
            "Your document has been created! Notice the clean filename and structured content."
            "The system automatically creates proper Org structure."
            nil)

           ("Step 5: Smart Processing"
            "Let's process your document with AI assistance."
            "Run: C-c s P (smart process buffer)"
            "workflow-integration-smart-process-buffer")

           ("Congratulations!"
            "You've completed the Getting Started tutorial!"
            "Key takeaways:"
            "- Clean, frictionless document creation"
            "- Automatic AI processing"
            "- Structured research workflows"
            "Ready for more advanced features?"
            nil))))
    (tutorial-system-run-tutorial "Getting Started" steps)))

(defun tutorial-system-document-management ()
  "Document Management tutorial."
  (let ((steps
         '(("Document Management Mastery"
            "Learn to create, organize, and manage your research documents effectively."
            "This tutorial covers advanced document features."
            nil)

           ("Quick Notes vs Full Documents"
            "Quick notes (C-c s q) create minimal documents instantly."
            "Full import (C-c s i) creates complete documents with metadata."
            "Try both approaches for different research needs."
            nil)

           ("Import from Academic Sources"
            "Import papers directly from academic databases."
            "Try: C-c s X (arXiv) or C-c s M (PubMed)"
            "academic-apis-search-arxiv")

           ("Org File Manager"
            "View all your documents in a structured, Org-aware interface."
            "Run: C-c s f"
            "org-file-manager")

           ("Document Status Indicators"
            "Notice the status icons in the file manager:"
            "🧠 Analyzed, 📄 Published, 💡 Conceptualized, 📝 Draft"
            "These show document processing status at a glance."
            nil)

           ("Advanced Organization"
            "Documents are automatically organized with:"
            "- Clean filenames (no .org extensions visible)"
            "- Internal ID mapping for reliability"
            "- Metadata extraction and indexing"
            "- Citation tracking and linking"
            nil))))
    (tutorial-system-run-tutorial "Document Management" steps)))

(defun tutorial-system-ai-integration ()
  "AI Integration tutorial."
  (let ((steps
         '(("AI-Powered Research Assistant"
            "Discover how AI enhances your research workflow."
            "The system includes multiple AI providers and features."
            nil)

           ("Document Analysis"
            "AI can analyze your documents for key insights."
            "Run: C-c s A a (analyze document)"
            "ai-integration-analyze-document")

           ("Automatic Summarization"
            "Generate concise summaries of long documents."
            "Run: C-c s A s (summarize document)"
            "ai-integration-summarize-document")

           ("Question Answering"
            "Ask questions about your document collection."
            "Run: C-c s A q (ask AI question)"
            "ai-integration-ask-question")

           ("Concept Extraction"
            "Automatically identify key concepts and themes."
            "Run: C-c s A c (extract concepts)"
            "ai-integration-extract-concepts")

           ("Research Gap Detection"
            "AI can identify missing connections in your research."
            "Run: C-c s A g (detect research gaps)"
            "ai-integration-detect-research-gaps")

           ("Writing Assistance"
            "Get AI help with academic writing tasks."
            "Run: C-c s A w (writing assistance)"
            "ai-integration-writing-assistance"))))
    (tutorial-system-run-tutorial "AI Integration" steps)))

(defun tutorial-system-3d-visualization ()
  "3D Visualization tutorial."
  (let ((steps
         '(("3D Knowledge Visualization"
            "Explore your research in interactive 3D visualizations."
            "See connections between papers, concepts, and ideas."
            nil)

           ("Open 3D Visualizer"
            "Launch the 3D visualization interface."
            "Run: C-c s v"
            "viz-engine-open")

           ("Camera Controls (Keyboard)"
            "Navigate in 3D using keyboard controls:"
            "WASD: Move camera"
            "IJKL: Rotate camera"
            "UO: Zoom in/out"
            "R: Reset camera"
            nil)

           ("Node Interaction"
            "Interact with research nodes:"
            "↑↓: Select different nodes"
            "Enter: Open selected document"
            "Space: Highlight connections"
            "N: Focus on selected node"
            nil)

           ("Visualization Modes"
            "Switch between different network views:"
            "Citation Network: Paper connections"
            "Concept Map: Idea relationships"
            "Author Network: Collaboration patterns"
            "Change with: C-c s n"
            nil)

           ("Touch Support"
            "On touch devices:"
            "Single drag: Rotate camera"
            "Pinch: Zoom in/out"
            "Tap nodes: Select and open"
            nil))))
    (tutorial-system-run-tutorial "3D Visualization" steps)))

(defun tutorial-system-research-workflows ()
  "Research Workflows tutorial."
  (let ((steps
         '(("Complete Research Workflow Management"
            "Learn to manage entire research projects from start to finish."
            "Includes planning, execution, and progress tracking."
            nil)

           ("Frictionless Research Session"
            "Start a complete research environment instantly."
            "Run: C-c s R (frictionless research session)"
            "workflow-integration-frictionless-research-session")

           ("Project Planning"
            "Create structured research projects."
            "Run: C-c s P (create research project)"
            "scientific-mapping-create-research-project")

           ("Agenda Integration"
            "Manage research tasks and deadlines."
            "Run: C-c s a (open research agenda)"
            "scientific-mapping-agenda")

           ("Paper Review Scheduling"
            "Schedule systematic literature reviews."
            "Run: C-c s p (schedule paper review)"
            "scientific-mapping-schedule-paper-review")

           ("Progress Tracking"
            "Generate weekly research progress reports."
            "Run: C-c s w (weekly review)"
            "scientific-mapping-weekly-review")

           ("Unified Search"
            "Search across all research content simultaneously."
            "Run: C-c s U (unified search)"
            "workflow-integration-unified-search"))))
    (tutorial-system-run-tutorial "Research Workflows" steps)))

(defun tutorial-system-advanced-features ()
  "Advanced Features tutorial."
  (let ((steps
         '(("Advanced Features & Customization"
            "Master the advanced capabilities and customization options."
            "Includes API integrations, reference managers, and automation."
            nil)

           ("Academic API Integration"
            "Import directly from scholarly databases."
            "arXiv: C-c s X, PubMed: C-c s M, DOI: C-c s D"
            "academic-apis-search-arxiv")

           ("Reference Manager Sync"
            "Sync with Zotero and BibTeX libraries."
            "Zotero: C-c s Z, BibTeX: C-c s B"
            "reference-managers-zotero-get-collections")

           ("Data Synchronization"
            "Keep all components in sync automatically."
            "Full sync: C-c s Y, Check consistency: C-c s C"
            "data-synchronization-full-sync")

           ("Smart Suggestions"
            "Get context-aware recommendations."
            "Run: C-c s S (smart suggestions)"
            "context-awareness-show-suggestions")

           ("Workflow Status"
            "Monitor system status and background operations."
            "Run: C-c s W (workflow status)"
            "workflow-integration-show-workflow-status")

           ("Customization"
            "All features are customizable in your init.el"
            "See keybindings.el for complete configuration system"
            nil))))
    (tutorial-system-run-tutorial "Advanced Features" steps)))

;;;; Tutorial Runner

(defun tutorial-system-run-tutorial (title steps)
  "Run a tutorial with TITLE through STEPS."
  (let ((buffer (get-buffer-create (format "*Tutorial: %s*" title))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (tutorial-system-show-step steps 0)
      (local-set-key (kbd "RET") (lambda () (interactive)
                                  (tutorial-system-next-step steps)))
      (local-set-key (kbd "q") 'tutorial-system-quit-tutorial))
    (switch-to-buffer buffer)))

(defun tutorial-system-show-step (steps step-index)
  "Show tutorial step at STEP-INDEX from STEPS."
  (when (< step-index (length steps))
    (let ((step (nth step-index steps)))
      (erase-buffer)
      (insert (format "#+TITLE: Tutorial - %s (Step %d/%d)\n\n"
                      tutorial-system-current-tutorial
                      (1+ step-index) (length steps)))
      (insert (format "* %s\n\n" (nth 0 step)))
      (insert (format "%s\n\n" (nth 1 step)))
      (when (nth 2 step)
        (insert (format "** Action Required:\n%s\n\n" (nth 2 step))))
      (when (nth 3 step)
        (insert (format "** Command to Run:\n~%s~\n\n" (nth 3 step))))
      (insert "** Navigation:\n")
      (insert "- Press RET to continue\n")
      (insert "- Press q to quit tutorial\n")
      (goto-char (point-min)))))

(defun tutorial-system-next-step (steps)
  "Move to the next tutorial step."
  (setq tutorial-system-tutorial-step (1+ tutorial-system-tutorial-step))
  (if (>= tutorial-system-tutorial-step (length steps))
      (tutorial-system-complete-tutorial)
    (tutorial-system-show-step steps tutorial-system-tutorial-step)))

(defun tutorial-system-complete-tutorial ()
  "Complete the current tutorial."
  (erase-buffer)
  (insert (format "#+TITLE: Tutorial Complete - %s\n\n" tutorial-system-current-tutorial))
  (insert "* 🎉 Tutorial Completed!\n\n")
  (insert "You've successfully completed the tutorial!\n\n")
  (insert "** What you learned:\n")
  (insert "- Key concepts and workflows\n")
  (insert "- Practical commands and shortcuts\n")
  (insert "- Integration between system components\n\n")
  (insert "** Next Steps:\n")
  (insert "- Try the features you just learned\n")
  (insert "- Explore other tutorials for more features\n")
  (insert "- Use C-c s S for context-aware suggestions\n\n")
  (insert "** Need Help?\n")
  (insert "- C-c s ? : General help\n")
  (insert "- C-c s K : Complete keyboard reference\n")
  (insert "- C-c s ! : Welcome screen and quick start\n")
  (goto-char (point-min)))

(defun tutorial-system-quit-tutorial ()
  "Quit the current tutorial."
  (interactive)
  (when (y-or-n-p "Quit tutorial? ")
    (kill-buffer)
    (message "Tutorial exited. You can resume anytime with M-x tutorial-system-start-tutorial")))

;;;; Integration with UX Enhancements

(with-eval-after-load 'ux-enhancements
  ;; Add tutorial to welcome screen
  (defun tutorial-system-enhanced-welcome ()
    "Enhanced welcome screen with tutorial integration."
    (ux-enhancements-welcome-screen)
    ;; Add tutorial option after welcome
    (run-with-timer 2 nil (lambda ()
                           (when (y-or-n-p "Would you like to start an interactive tutorial? ")
                             (tutorial-system-interactive-tutorial))))))

(provide 'tutorial-system)

;;; tutorial-system.el ends here

;;; Usage:
;;
;; Interactive tutorial system for learning scientific mapping features.
;; Key functions:
;; - tutorial-system-interactive-tutorial: Choose and start tutorials
;; - tutorial-system-start-tutorial: Start specific tutorial by name
;; - tutorial-system-run-tutorial: Internal tutorial runner
;;
;; Available tutorials:
;; - Getting Started: Basic setup and first document
;; - Document Management: File organization and metadata
;; - AI Integration: AI-powered analysis features
;; - 3D Visualization: Interactive knowledge graphs
;; - Research Workflows: Complete project management
;; - Advanced Features: API integrations and customization