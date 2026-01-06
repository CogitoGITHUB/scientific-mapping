;;; ux-enhancements.el --- User experience enhancements for scientific mapping -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

;;; Commentary:

;; This file provides comprehensive UX enhancements for the scientific mapping system,
;; including onboarding, visual feedback, workflow guidance, and accessibility improvements.

;;; Code:

(require 'scientific-mapping)
(require 'org)
(require 'cl-lib)

(defgroup ux-enhancements ()
  "User experience enhancements."
  :group 'scientific-mapping)

;;;; Onboarding & Welcome Experience

(defcustom ux-enhancements-enable-welcome t
  "Show welcome screen for new users."
  :group 'ux-enhancements
  :type 'boolean)

(defcustom ux-enhancements-show-tips t
  "Show contextual tips and suggestions."
  :group 'ux-enhancements
  :type 'boolean)

(defvar ux-enhancements-first-run nil
  "Non-nil if this is the first run of scientific mapping.")

;;;###autoload
(defun ux-enhancements-welcome-screen ()
  "Show an interactive welcome screen for new users."
  (interactive)
  (let ((buffer (get-buffer-create "*Scientific Mapping - Welcome*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Welcome to Scientific Mapping
#+OPTIONS: toc:nil num:nil

* 🎯 Your Research Workflow, Reimagined

Welcome to the most advanced scientific research platform for Emacs!

** 🚀 Quick Start Guide
1. *Enable the system*: ~M-x scientific-mapping-start~
2. *Create your first document*: ~C-c s q~ (quick note)
3. *Import research papers*: ~C-c s i~ (from DOI) or ~C-c s X~ (from arXiv)
4. *Explore your knowledge*: ~C-c s v~ (3D visualization)
5. *Manage your agenda*: ~C-c s a~ (research planning)

** ✨ What Makes This Special
- *Frictionless workflow*: Everything works together seamlessly
- *AI-powered intelligence*: Automatic analysis and suggestions
- *Beautiful visualizations*: 3D knowledge graphs with touch support
- *Complete automation*: Background processing and smart defaults
- *Cross-platform*: Works on desktop, laptop, tablet, and mobile

** 🎨 Interface Highlights
- Clean filenames without clutter
- Visual status indicators (🧠 analyzed, 📄 published)
- Keyboard-first design with touch support
- Context-aware suggestions
- Seamless integration across all tools

* 📚 Getting Started Tutorials

** Tutorial 1: Your First Research Document
1. Press ~C-c s q~ to create a quick note
2. Type a title like \"Machine Learning Applications\"
3. The system automatically creates a structured Org file
4. Add content and press ~C-c s P~ for smart processing
5. Watch as AI analyzes and enhances your document!

** Tutorial 2: Building Your Knowledge Graph
1. Import papers with ~C-c s X~ (arXiv) or ~C-c s M~ (PubMed)
2. Extract concepts with ~C-c s A c~
3. Visualize relationships with ~C-c s v~
4. Navigate in 3D with ~WASD~ keys or touch gestures

** Tutorial 3: Research Planning & Tracking
1. Schedule reviews with ~C-c s p~
2. Create project plans with ~C-c s P~
3. Track progress with ~C-c s w~ (weekly review)
4. Get AI assistance with ~C-c s A r~

* 🎯 Pro Tips

** Power User Shortcuts
- ~C-c s R~: Frictionless research session (opens everything you need)
- ~C-c s U~: Unified search across all your content
- ~C-c s S~: Smart suggestions based on your current work
- ~C-c s f~: Org File Manager (clean document overview)

** Workflow Optimization
- Documents auto-analyze when opened/saved
- Citations sync automatically
- Visualizations update in real-time
- Everything works together seamlessly

** Customization
- All features can be customized in your init.el
- Keyboard shortcuts are fully configurable
- Themes and colors adapt to your preferences
- AI providers can be switched (Ollama/OpenAI/Anthropic)

* 🚀 Ready to Start Researching?

Press ~C-c s s~ to begin your scientific mapping journey!

* Need Help?
- ~C-c s ?~: System help and commands
- ~H~ in 3D visualization: Control reference
- ~C-c s S~: Context-aware suggestions
- Visit the documentation for detailed guides

Happy researching! 🎓✨
")
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer buffer)
    (message "Welcome to Scientific Mapping! Press C-c s s to get started.")))

;;;; Visual Feedback & Status Indicators

(defvar ux-enhancements-mode-line-status ""
  "Current status for mode line display.")

(defvar ux-enhancements-status-timer nil
  "Timer for status updates.")

(defun ux-enhancements-update-mode-line ()
  "Update the mode line with current scientific mapping status."
  (when scientific-mapping-mode
    (let* ((doc-count (length (doc-engine-all-files)))
           (ai-status (if (featurep 'ai-integration) "AI✓" "AI✗"))
           (viz-status (if viz-engine-mode "3D✓" "3D✗"))
           (agenda-count (length scientific-mapping-agenda-files)))
      (setq ux-enhancements-mode-line-status
            (format " SciMap[%d docs|%s|%s|%d agendas] "
                    doc-count ai-status viz-status agenda-count))
      (force-mode-line-update))))

(add-hook 'scientific-mapping-mode-hook #'ux-enhancements-update-mode-line)

;;;; Progress Indicators & Feedback

(defvar ux-enhancements-current-progress nil
  "Current progress operation.")

;;;###autoload
(defun ux-enhancements-show-progress (operation &optional current total)
  "Show progress for OPERATION with CURRENT/TOTAL completion."
  (let ((progress (if total
                     (format "%.1f%%" (* 100.0 (/ current total)))
                   "Working...")))
    (message "%s: %s" operation progress)
    (setq ux-enhancements-current-progress
          (format "%s (%s)" operation progress))
    (ux-enhancements-update-mode-line)))

;;;###autoload
(defun ux-enhancements-progress-complete (operation)
  "Mark OPERATION as completed."
  (message "%s: ✓ Complete!" operation)
  (setq ux-enhancements-current-progress nil)
  (ux-enhancements-update-mode-line))

;;;; Smart Notifications & Alerts

(defvar ux-enhancements-notifications-enabled t
  "Enable smart notifications.")

(defvar ux-enhancements-notification-history '()
  "History of recent notifications.")

;;;###autoload
(defun ux-enhancements-smart-notification (type message &optional action)
  "Show a smart notification of TYPE with MESSAGE and optional ACTION."
  (when ux-enhancements-notifications-enabled
    (let ((notification (format "[%s] %s" type message)))
      (message notification)
      (push (list (current-time) type message action) ux-enhancements-notification-history)

      ;; Limit history to last 50 notifications
      (when (> (length ux-enhancements-notification-history) 50)
        (setq ux-enhancements-notification-history
              (seq-take ux-enhancements-notification-history 50)))

      ;; Special handling for different types
      (pcase type
        ("AI" (ux-enhancements-ai-notification message action))
        ("Import" (ux-enhancements-import-notification message action))
        ("Error" (ux-enhancements-error-notification message action))
        ("Success" (ux-enhancements-success-notification message action))))))

(defun ux-enhancements-ai-notification (message action)
  "Handle AI-related notifications."
  (when (string-match-p "analysis.*complete" message)
    (run-with-timer 1 nil (lambda ()
                           (message "💡 AI analysis complete! Check results in the buffer.")))))

(defun ux-enhancements-import-notification (message action)
  "Handle import-related notifications."
  (when action
    (run-with-timer 2 nil action)))

(defun ux-enhancements-error-notification (message action)
  "Handle error notifications with recovery suggestions."
  (when action
    (run-with-timer 1 nil
                   (lambda ()
                     (let ((choice (read-char-choice
                                   (format "Error: %s\n[r]etry, [s]kip, [h]elp: " message)
                                   '(?r ?s ?h))))
                       (pcase choice
                         (?r (funcall action))
                         (?h (describe-function action))))))))

(defun ux-enhancements-success-notification (message action)
  "Handle success notifications."
  (when (string-match-p "workflow" message)
    (run-with-timer 1 nil (lambda ()
                           (message "🎉 Workflow complete! Ready for next steps.")))))

;;;; Context-Aware Help & Tips

(defvar ux-enhancements-tip-timer nil
  "Timer for showing contextual tips.")

;;;###autoload
(defun ux-enhancements-show-contextual-tip ()
  "Show a contextual tip based on current activity."
  (when ux-enhancements-show-tips
    (let ((tip (ux-enhancements-generate-tip)))
      (when tip
        (message "💡 Tip: %s" tip)))))

(defun ux-enhancements-generate-tip ()
  "Generate a contextual tip based on current state."
  (cond
   ;; First time user
   ((and ux-enhancements-first-run scientific-mapping-mode)
    "Welcome! Try C-c s q to create your first research note.")

   ;; In document buffer
   ((and (eq major-mode 'org-mode)
         (member (buffer-file-name) (doc-engine-all-files)))
    (let ((has-ai-analysis (org-entry-get nil "AI_ANALYZED"))
          (has-doi (org-entry-get nil "DOI")))
      (cond
       ((not has-ai-analysis) "Try C-c s A a to analyze this document with AI.")
       ((not has-doi) "Add a DOI with M-x org-set-property to enable citation tracking.")
       (t "Press C-c s A e for semantic search across your documents."))))

   ;; In agenda
   ((and (eq major-mode 'org-mode)
         (member (buffer-file-name) scientific-mapping-agenda-files))
    "Use C-c s p to schedule paper reviews directly from agenda items.")

   ;; In visualization
   ((string-match-p "scientific-visualizer" (buffer-name))
    "Use WASD to move, IJKL to rotate, UO to zoom. Press H for full controls.")

   ;; General tips
   (scientific-mapping-mode
    (seq-random-elt
     '("C-c s R starts a complete frictionless research session."
       "C-c s f opens the Org File Manager for document overview."
       "C-c s S shows smart suggestions based on your work."
       "C-c s U searches across all documents, citations, and concepts.")))))

;;;; Enhanced Error Handling & Recovery

(defvar ux-enhancements-error-recovery-actions '()
  "List of pending error recovery actions.")

;;;###autoload
(defun ux-enhancements-handle-error (error context &optional recovery-fn)
  "Handle ERROR in CONTEXT with optional RECOVERY-FN."
  (let ((error-msg (error-message-string error))
        (context-desc (symbol-name context)))
    (ux-enhancements-smart-notification
     "Error"
     (format "%s in %s" error-msg context-desc)
     recovery-fn)

    (when recovery-fn
      (push (list error context recovery-fn (current-time))
            ux-enhancements-error-recovery-actions))))

;;;###autoload
(defun ux-enhancements-show-error-recovery ()
  "Show pending error recovery options."
  (interactive)
  (if (not ux-enhancements-error-recovery-actions)
      (message "No pending error recoveries.")
    (let ((choice (completing-read
                   "Recover from error: "
                   (mapcar (lambda (item)
                            (format "%s in %s" (error-message-string (nth 0 item)) (nth 1 item)))
                          ux-enhancements-error-recovery-actions))))
      (when choice
        ;; Find and execute the recovery function
        (let ((recovery-item (seq-find (lambda (item)
                                        (string-match-p choice
                                                       (format "%s in %s"
                                                              (error-message-string (nth 0 item))
                                                              (nth 1 item))))
                                      ux-enhancements-error-recovery-actions)))
          (when recovery-item
            (funcall (nth 2 recovery-item))
            (setq ux-enhancements-error-recovery-actions
                  (delete recovery-item ux-enhancements-error-recovery-actions))))))))

;;;; Performance Indicators & Background Processing

(defvar ux-enhancements-background-operations '()
  "List of current background operations.")

;;;###autoload
(defun ux-enhancements-background-operation (name function &optional callback)
  "Run FUNCTION as NAME in background with optional CALLBACK."
  (let ((operation-id (format "%s-%s" name (random 1000))))
    (push (list operation-id name (current-time) callback)
          ux-enhancements-background-operations)
    (ux-enhancements-smart-notification "Background" (format "Started %s" name))

    (run-with-timer 0.1 nil
                   (lambda ()
                     (condition-case err
                         (progn
                           (funcall function)
                           (when callback (funcall callback))
                           (ux-enhancements-smart-notification
                            "Background" (format "Completed %s" name))
                           (setq ux-enhancements-background-operations
                                 (seq-remove (lambda (op) (equal (car op) operation-id))
                                           ux-enhancements-background-operations)))
                       (error (ux-enhancements-handle-error err 'background-operation)))))))

;;;###autoload
(defun ux-enhancements-show-background-status ()
  "Show status of background operations."
  (interactive)
  (if (not ux-enhancements-background-operations)
      (message "No background operations running.")
    (message "Background operations: %s"
             (mapconcat (lambda (op) (format "%s (%.1fs)" (nth 1 op)
                                           (float-time (time-subtract (current-time) (nth 2 op)))))
                       ux-enhancements-background-operations ", "))))

;;;; Accessibility & Keyboard Navigation

(defcustom ux-enhancements-keyboard-hints t
  "Show keyboard hints in status messages."
  :group 'ux-enhancements
  :type 'boolean)

;;;###autoload
(defun ux-enhancements-keyboard-help ()
  "Show comprehensive keyboard help."
  (interactive)
  (let ((buffer (get-buffer-create "*Scientific Mapping - Keyboard Help*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Scientific Mapping - Keyboard Reference
#+OPTIONS: toc:nil

* 🎹 Complete Keyboard Reference

** Core System
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s s~ | ~scientific-mapping-start~ | Start system |
| ~C-c s S~ | ~scientific-mapping-stop~ | Stop system |
| ~C-c s ?~ | ~scientific-mapping-help~ | Show help |
| ~C-c s h~ | ~scientific-mapping-status~ | System status |

** Document Management
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s i~ | ~scientific-mapping-import-paper~ | Import paper |
| ~C-c s q~ | ~doc-engine-quick-note~ | Quick note |
| ~C-c s m~ | ~doc-engine-smart-import~ | Smart import |
| ~C-c s c~ | ~concept-relationships-create-entry~ | Create concept |
| ~C-c s v~ | ~viz-engine-open~ | Open 3D visualization |
| ~C-c s n~ | ~viz-engine-set-mode~ | Set visualization mode |

** Research Workflows
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s r~ | ~scientific-mapping-literature-review~ | Literature review |
| ~C-c s b~ | ~scientific-mapping-backup~ | Create backup |
| ~C-c s t~ | ~timeline-engine-open~ | Open timeline |
| ~C-c s e~ | ~concept-tree-toggle-section~ | Toggle concept tree |

** Agenda Integration
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s a~ | ~scientific-mapping-agenda~ | Open agenda |
| ~C-c s p~ | ~scientific-mapping-schedule-paper-review~ | Schedule review |
| ~C-c s d~ | ~scientific-mapping-schedule-deadline~ | Schedule deadline |
| ~C-c s P~ | ~scientific-mapping-create-research-project~ | Create project |
| ~C-c s w~ | ~scientific-mapping-weekly-review~ | Weekly review |
| ~C-c s A~ | ~scientific-mapping-agenda-from-document~ | Agenda from doc |

** Academic APIs
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s X~ | ~academic-apis-search-arxiv~ | Search arXiv |
| ~C-c s M~ | ~academic-apis-search-pubmed~ | Search PubMed |
| ~C-c s D~ | ~academic-apis-resolve-doi~ | Resolve DOI |
| ~C-c s I~ | ~academic-apis-import-to-scientific-mapping~ | Import from APIs |

** Reference Managers
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s Z~ | ~reference-managers-zotero-get-collections~ | Zotero collections |
| ~C-c s B~ | ~reference-managers-import-bibtex~ | Import BibTeX |
| ~C-c s R~ | ~reference-managers-import-to-scientific-mapping~ | Import from managers |

** AI Integration (C-c s A prefix)
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s A a~ | ~ai-integration-analyze-document~ | Analyze document |
| ~C-c s A s~ | ~ai-integration-summarize-document~ | Summarize document |
| ~C-c s A q~ | ~ai-integration-ask-question~ | Ask AI question |
| ~C-c s A c~ | ~ai-integration-extract-concepts~ | Extract concepts |
| ~C-c s A g~ | ~ai-integration-detect-research-gaps~ | Detect gaps |
| ~C-c s A r~ | ~ai-integration-research-assistance~ | Research assistance |
| ~C-c s A w~ | ~ai-integration-writing-assistance~ | Writing assistance |
| ~C-c s A i~ | ~ai-integration-analyze-citations~ | Analyze citations |
| ~C-c s A e~ | ~ai-integration-semantic-search~ | Semantic search |
| ~C-c s A b~ | ~ai-integration-batch-analyze~ | Batch analyze |

** Workflow Integration
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s W~ | ~workflow-integration-show-workflow-status~ | Workflow status |
| ~C-c s U~ | ~workflow-integration-unified-search~ | Unified search |
| ~C-c s F~ | ~workflow-integration-import-paper-complete~ | Complete import |
| ~C-c s Y~ | ~data-synchronization-full-sync~ | Full sync |
| ~C-c s C~ | ~data-synchronization-consistency-check~ | Consistency check |
| ~C-c s S~ | ~context-awareness-show-suggestions~ | Smart suggestions |
| ~C-c s Q~ | ~context-awareness-quick-actions~ | Quick actions |

** Org File Manager
| Key | Command | Description |
|-----|---------|-------------|
| ~C-c s f~ | ~org-file-manager~ | Open file manager |
| ~RET~ | ~org-file-manager-open-file~ | Open file |
| ~o~ | ~org-file-manager-open-file-other-window~ | Open in other window |
| ~v~ | ~org-file-manager-preview-file~ | Preview file |
| ~g~ | ~org-file-manager-refresh~ | Refresh listing |
| ~s~ | ~org-file-manager-sort~ | Change sort |
| ~c~ | ~org-file-manager-create-file~ | Create file |
| ~d~ | ~org-file-manager-delete-file~ | Delete file |
| ~r~ | ~org-file-manager-rename-file~ | Rename file |
| ~q~ | ~org-file-manager-quit~ | Quit manager |

** 3D Visualization (Keyboard Only)
| Keys | Action |
|------|--------|
| ~WASD~ | Move camera |
| ~QE~ | Up/down |
| ~IJKL~ | Rotate camera |
| ~UO~ | Zoom in/out |
| ~R~ | Reset camera |
| ~F~ | Fit to screen |
| ~P~ | Pause/resume |
| ~Z~ | Toggle labels |
| ~↑↓~ | Select nodes |
| ~←→~ | Navigate connections |
| ~Enter~ | Activate node |
| ~Space~ | Highlight connections |
| ~N~ | Focus on node |
| ~M~ | Zoom to node |
| ~X~ | Clear search |
| ~H~ | Show help |

** Touch Controls (Mobile/Laptop)
| Gesture | Action |
|---------|--------|
| Single drag | Rotate camera |
| Two finger pinch | Zoom |
| Two finger rotate | Camera rotation |
| Tap node | Select & activate |

* 💡 Pro Tips
- Start with ~C-c s R~ for a complete research session
- Use ~C-c s S~ for context-aware suggestions
- ~C-c s f~ gives you a clean document overview
- Everything works without mouse - pure keyboard power!

* 🎨 Customization
All key bindings can be customized in ~~/.emacs.d/init.el~
See ~keybindings.el~ for the complete configuration system.
")
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer buffer)))

;;;; Theme & Visual Enhancements

(defcustom ux-enhancements-theme 'scientific
  "Color theme for scientific mapping."
  :group 'ux-enhancements
  :type '(choice (const :tag "Scientific (Blue/Green)" scientific)
                 (const :tag "Academic (Purple/Gold)" academic)
                 (const :tag "Modern (Dark)" modern)
                 (const :tag "Light" light)))

;;;###autoload
(defun ux-enhancements-apply-theme ()
  "Apply the selected theme."
  (interactive)
  (pcase ux-enhancements-theme
    ('scientific (ux-enhancements-theme-scientific))
    ('academic (ux-enhancements-theme-academic))
    ('modern (ux-enhancements-theme-modern))
    ('light (ux-enhancements-theme-light))))

(defun ux-enhancements-theme-scientific ()
  "Apply scientific theme (blues and greens)."
  (set-face-attribute 'org-level-1 nil :foreground "#4a90e2" :weight 'bold)
  (set-face-attribute 'org-level-2 nil :foreground "#50c878")
  (set-face-attribute 'org-todo nil :foreground "#ff6b6b" :weight 'bold)
  (set-face-attribute 'org-done nil :foreground "#50c878" :weight 'bold))

(defun ux-enhancements-theme-academic ()
  "Apply academic theme (purples and golds)."
  (set-face-attribute 'org-level-1 nil :foreground "#9b59b6" :weight 'bold)
  (set-face-attribute 'org-level-2 nil :foreground "#f39c12")
  (set-face-attribute 'org-todo nil :foreground "#e74c3c" :weight 'bold)
  (set-face-attribute 'org-done nil :foreground "#27ae60" :weight 'bold))

(defun ux-enhancements-theme-modern ()
  "Apply modern dark theme."
  (set-face-attribute 'org-level-1 nil :foreground "#61dafb" :weight 'bold)
  (set-face-attribute 'org-level-2 nil :foreground "#98c379")
  (set-face-attribute 'org-todo nil :foreground "#e06c75" :weight 'bold)
  (set-face-attribute 'org-done nil :foreground "#98c379" :weight 'bold))

(defun ux-enhancements-theme-light ()
  "Apply light theme."
  (set-face-attribute 'org-level-1 nil :foreground "#2c3e50" :weight 'bold)
  (set-face-attribute 'org-level-2 nil :foreground "#3498db")
  (set-face-attribute 'org-todo nil :foreground "#e74c3c" :weight 'bold)
  (set-face-attribute 'org-done nil :foreground "#27ae60" :weight 'bold))

;;;; Initialization

(defun ux-enhancements-initialize ()
  "Initialize UX enhancements."
  (when ux-enhancements-enable-welcome
    (run-with-timer 1 nil #'ux-enhancements-welcome-screen))

  ;; Apply theme
  (ux-enhancements-apply-theme)

  ;; Setup contextual tips
  (when ux-enhancements-show-tips
    (run-with-timer 300 300 #'ux-enhancements-show-contextual-tip)) ; Every 5 minutes

  ;; Setup mode line updates
  (run-with-timer 60 60 #'ux-enhancements-update-mode-line) ; Every minute

  (message "UX enhancements initialized"))

;; Auto-initialize
(ux-enhancements-initialize)

(provide 'ux-enhancements)

;;; ux-enhancements.el ends here

;;; Usage:
;;
;; This file provides comprehensive UX enhancements including:
;; - Welcome screen and onboarding
;; - Visual feedback and status indicators
;; - Smart notifications and error handling
;; - Context-aware tips and suggestions
;; - Keyboard help and accessibility
;; - Theme customization
;; - Progress indicators and background processing
;;
;; Key functions:
;; - ux-enhancements-welcome-screen: Interactive welcome for new users
;; - ux-enhancements-keyboard-help: Complete keyboard reference
;; - ux-enhancements-show-contextual-tip: Smart tips based on context
;; - ux-enhancements-smart-notification: Intelligent notifications
;; - ux-enhancements-apply-theme: Apply visual themes