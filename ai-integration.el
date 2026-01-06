;;; ai-integration.el --- AI integration for scientific knowledge mapping -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

;;; Commentary:

;; This file provides integration between AI tools (org-mcp, agent-shell)
;; and the scientific knowledge mapping system. It enables AI-powered
;; document analysis, question answering, and research assistance.

;;; Code:

(require 'org)
(require 'org-mcp nil t)
(require 'agent-shell nil t)

(defgroup ai-integration ()
  "AI integration for scientific knowledge mapping."
  :group 'scientific-mapping)

(defcustom ai-integration-auto-start-mcp nil
  "If non-nil, automatically start org-mcp server on load."
  :group 'ai-integration
  :type 'boolean)

(defcustom ai-integration-auto-start-agent-shell nil
  "If non-nil, automatically start agent-shell on load."
  :group 'ai-integration
  :type 'boolean)

;;; MCP Integration

(defun ai-integration-start-mcp ()
  "Start the org-mcp server for AI-powered Org interactions."
  (interactive)
  (when (featurep 'org-mcp)
    (org-mcp-start-server)
    (message "org-mcp server started")))

(defun ai-integration-stop-mcp ()
  "Stop the org-mcp server."
  (interactive)
  (when (featurep 'org-mcp)
    (org-mcp-stop-server)
    (message "org-mcp server stopped")))

;;; Agent Shell Integration

(defun ai-integration-start-agent-shell ()
  "Start the agent shell for AI agent interactions."
  (interactive)
  (when (featurep 'agent-shell)
    (agent-shell-start)
    (message "agent-shell started")))

(defun ai-integration-stop-agent-shell ()
  "Stop the agent shell."
  (interactive)
  (when (featurep 'agent-shell)
    (agent-shell-stop)
    (message "agent-shell stopped")))

;;; AI-Powered Document Analysis

(defun ai-integration-analyze-document ()
  "Analyze current document with AI assistance."
  (interactive)
  (if (not (or (featurep 'org-mcp) (featurep 'agent-shell)))
      (message "No AI packages available. Install org-mcp or agent-shell.")
    (let ((buffer (current-buffer))
          (doc-content (buffer-string)))
      (with-current-buffer (get-buffer-create "*AI Document Analysis*")
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: AI Document Analysis\n\n")
        (insert "* Document Overview\n\n")
        (insert (format "** File: %s\n" (buffer-file-name buffer)))
        (insert (format "** Size: %d characters\n\n" (length doc-content)))

        ;; Request AI analysis
        (when (featurep 'org-mcp)
          (insert "* MCP Analysis\n\n")
          (insert "#+BEGIN_SRC org-mcp\n")
          (insert "Analyze this document and provide:\n")
          (insert "1. Main topics and themes\n")
          (insert "2. Key findings or arguments\n")
          (insert "3. Potential research gaps\n")
          (insert "4. Related concepts to explore\n")
          (insert "#+END_SRC\n\n"))

        (switch-to-buffer (current-buffer))))))

;;; AI Question Answering

(defun ai-integration-ask-question (question)
  "Ask an AI question about the current document collection."
  (interactive "sAsk AI about your documents: ")
  (if (not (or (featurep 'org-mcp) (featurep 'agent-shell)))
      (message "No AI packages available. Install org-mcp or agent-shell.")
    (let ((buffer (get-buffer-create "*AI Q&A*")))
      (with-current-buffer buffer
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: AI Question & Answer\n\n")
        (insert (format "* Question: %s\n\n" question))
        (insert "* AI Response\n\n")

        ;; Add context from current document if available
        (when (buffer-file-name)
          (insert "** Context Document\n")
          (insert (format "- File: %s\n" (buffer-file-name)))
          (insert (format "- Last modified: %s\n\n"
                         (format-time-string "%Y-%m-%d %H:%M"
                                           (nth 5 (file-attributes (buffer-file-name)))))))

        (when (featurep 'org-mcp)
          (insert "#+BEGIN_SRC org-mcp\n")
          (insert (format "Question: %s\n\n" question))
          (insert "Please provide a comprehensive answer based on the document context.\n")
          (insert "#+END_SRC\n\n"))

        (switch-to-buffer buffer)))))

;;; Research Assistance

(defun ai-integration-research-assistance ()
  "Get AI assistance for research planning and execution."
  (interactive)
  (if (not (or (featurep 'org-mcp) (featurep 'agent-shell)))
      (message "No AI packages available. Install org-mcp or agent-shell.")
    (let ((buffer (get-buffer-create "*AI Research Assistant*")))
      (with-current-buffer buffer
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: AI Research Assistant\n\n")
        (insert "* Research Planning\n\n")
        (insert "** Current Goals\n- [ ] Define research question\n- [ ] Identify key concepts\n- [ ] Plan methodology\n\n")
        (insert "** AI Assistance\n\n")

        (when (featurep 'org-mcp)
          (insert "#+BEGIN_SRC org-mcp\n")
          (insert "Help me plan my research by:\n")
          (insert "1. Refining my research question\n")
          (insert "2. Suggesting relevant literature\n")
          (insert "3. Identifying potential methodologies\n")
          (insert "4. Planning data collection strategies\n")
          (insert "#+END_SRC\n\n"))

        (switch-to-buffer buffer)))))

;;; Initialization

(defun ai-integration-initialize ()
  "Initialize AI integration components."
  (when (and ai-integration-auto-start-mcp (featurep 'org-mcp))
    (run-with-timer 1 nil #'ai-integration-start-mcp))

  (when (and ai-integration-auto-start-agent-shell (featurep 'agent-shell))
    (run-with-timer 1 nil #'ai-integration-start-agent-shell)))

;; Auto-initialize
(ai-integration-initialize)

(provide 'ai-integration)

;;; ai-integration.el ends here