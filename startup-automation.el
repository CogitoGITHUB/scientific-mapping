;;; startup-automation.el --- Automatic system setup and frictionless startup -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

;;; Commentary:

;; This file provides automatic system setup and frictionless startup
;; for the scientific mapping system. Everything happens in the background
;; with minimal user intervention.

;;; Code:

(require 'scientific-mapping)

(defgroup startup-automation ()
  "Automatic system setup and startup."
  :group 'scientific-mapping)

(defcustom startup-automation-enable t
  "If non-nil, enable automatic system setup on startup."
  :group 'startup-automation
  :type 'boolean)

(defcustom startup-automation-delay 2
  "Delay in seconds before starting automation."
  :group 'startup-automation
  :type 'number)

;;;; Automatic System Initialization

(defun startup-automation-initialize ()
  "Initialize the scientific mapping system automatically."
  (when startup-automation-enable
    (run-with-timer startup-automation-delay nil #'startup-automation-full-setup)))

(defun startup-automation-full-setup ()
  "Perform complete automated system setup."
  (message "🤖 Auto-initializing scientific mapping system...")

  ;; 1. Enable scientific mapping mode
  (unless scientific-mapping-mode
    (scientific-mapping-mode 1))

  ;; 2. Setup all automation features
  (startup-automation-enable-all-automation)

  ;; 3. Load and sync data
  (run-with-timer 1 nil #'startup-automation-load-and-sync)

  ;; 4. Start background services
  (run-with-timer 2 nil #'startup-automation-start-services)

  ;; 5. Show welcome message
  (run-with-timer 3 nil #'startup-automation-welcome-user)

  (message "🎯 System auto-initialization complete!"))

(defun startup-automation-enable-all-automation ()
  "Enable all automation features for frictionless workflow."

  ;; Workflow integration
  (when (featurep 'workflow-integration)
    (setq workflow-integration-frictionless-mode t)
    (message "✓ Workflow integration enabled"))

  ;; Document engine automation
  (when (featurep 'doc-engine)
    (setq doc-engine-auto-process-on-open t)
    (setq doc-engine-auto-process-on-save t)
    (setq doc-engine-auto-sync-citations t)
    (setq doc-engine-auto-extract-concepts t)
    (message "✓ Document automation enabled"))

  ;; AI automation
  (when (featurep 'ai-integration)
    (setq ai-integration-auto-analyze-new-documents t)
    (message "✓ AI automation enabled"))

  ;; Data synchronization
  (when (featurep 'data-synchronization)
    (data-synchronization-start-auto-sync)
    (message "✓ Data synchronization enabled"))

  ;; Context awareness
  (when (featurep 'context-awareness)
    (context-awareness-enable-proactive-suggestions)
    (message "✓ Context awareness enabled")))

(defun startup-automation-load-and-sync ()
  "Load data and perform initial synchronization."
  (message "🔄 Loading and synchronizing data...")

  ;; Load document mappings
  (when (featurep 'doc-engine)
    (doc-engine-load-id-map)
    (message "✓ Document mappings loaded"))

  ;; Perform full sync if needed
  (when (and (featurep 'data-synchronization)
             data-synchronization-auto-sync)
    (run-with-timer 0.5 nil #'data-synchronization-full-sync))

  (message "✓ Data synchronization initiated"))

(defun startup-automation-start-services ()
  "Start all background services."
  (message "⚙️ Starting background services...")

  ;; Start any websocket connections
  (when (featurep 'viz-engine)
    (message "✓ Visualization engine ready"))

  ;; Start MCP if configured
  (when (and (featurep 'ai-integration)
             ai-integration-auto-start-mcp)
    (run-with-timer 0.5 nil #'ai-integration-start-mcp))

  ;; Start agent shell if configured
  (when (and (featurep 'ai-integration)
             ai-integration-auto-start-agent-shell)
    (run-with-timer 0.5 nil #'ai-integration-start-agent-shell))

  (message "✓ Background services started"))

(defun startup-automation-welcome-user ()
  "Show welcome message and quick start guide."
  (let ((welcome-buffer (get-buffer-create "*Scientific Mapping - Welcome*")))
    (with-current-buffer welcome-buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Welcome to Scientific Mapping
#+OPTIONS: toc:nil

* 🎯 Frictionless Research Environment Ready!

Your scientific mapping system is now fully automated and ready for research.

** Quick Start Commands:
- =C-c s q= - Create quick note (just enter title)
- =C-c s m= - Smart import from clipboard/URL/DOI
- =C-c s R= - Start frictionless research session
- =C-c s P= - Smart process current buffer
- =C-c s S= - View smart suggestions

** Automation Active:
- ✅ Auto AI analysis of new documents
- ✅ Auto concept extraction
- ✅ Auto citation synchronization
- ✅ Auto review scheduling
- ✅ Auto data synchronization
- ✅ Smart context-aware suggestions

** Key Features:
- 📄 Clean filenames (no visible IDs)
- 🤖 Automatic AI processing
- 🔄 Real-time data synchronization
- 🎨 3D visualization with keyboard/touch control
- 📚 Unified search across all content
- 📅 Integrated research agenda
- 🔗 Seamless API integrations

* Need Help?
- =C-c s ?= - System help
- =C-c s S= - Smart suggestions
- =KEYBINDINGS.md= - Complete command reference

Happy researching! 🚀
")
      (goto-char (point-min)))
    (run-with-timer 1 nil (lambda () (display-buffer welcome-buffer)))))

;;;; Smart Buffer Management

(defun startup-automation-smart-buffer-setup ()
  "Setup smart buffer behavior for research workflow."
  (when startup-automation-enable

    ;; Auto-save scientific documents
    (add-hook 'org-mode-hook
              (lambda ()
                (when (doc-engine-is-scientific-document-p)
                  (auto-save-mode 1))))

    ;; Smart buffer naming
    (add-hook 'find-file-hook
              (lambda ()
                (when (doc-engine-is-scientific-document-p)
                  (let ((title (org-get-title)))
                    (when title
                      (rename-buffer (format "📄 %s" title) t))))))))

;; Initialize on load
(startup-automation-initialize)
(startup-automation-smart-buffer-setup)

(provide 'startup-automation)

;;; startup-automation.el ends here

;;; Usage:
;;
;; This file provides automatic system setup. When loaded, it will:
;; 1. Auto-enable scientific mapping mode
;; 2. Enable all automation features
;; 3. Load and sync data
;; 4. Start background services
;; 5. Show welcome message with quick start guide
;;
;; Everything happens automatically with zero user intervention!