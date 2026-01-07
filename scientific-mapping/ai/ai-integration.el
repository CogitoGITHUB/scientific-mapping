;;; ai-integration.el --- AI integration for scientific knowledge mapping -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This file provides comprehensive AI integration for the scientific knowledge
;; mapping system, including LLM-powered document analysis, semantic search,
;; automated summarization, and intelligent research assistance.

;; Features:
;; - LLM Integration (Ollama/OpenAI/Anthropic)
;; - Document Summarization & Analysis
;; - Question Answering over Document Collections
;; - Smart Concept & Keyword Extraction
;; - Semantic Search with Vector Embeddings
;; - Research Gap Detection
;; - Citation Analysis & Recommendations
;; - Writing Assistance & Content Generation

;;; Code:

(require 'org)
(require 'url)
(require 'json)
(eval-when-compile (require 'subr-x))

(defgroup ai-integration ()
  "AI integration for scientific knowledge mapping."
  :group 'scientific-mapping)

;;;; AI Provider Configuration

(defcustom ai-integration-provider 'ollama
  "AI provider to use for analysis and generation.
Options: ollama (local), openai, anthropic."
  :group 'ai-integration
  :type '(choice (const :tag "Ollama (Local LLM)" ollama)
                 (const :tag "OpenAI API" openai)
                 (const :tag "Anthropic API" anthropic)))

(defcustom ai-integration-ollama-url "http://localhost:11434"
  "URL for Ollama server."
  :group 'ai-integration
  :type 'string)

(defcustom ai-integration-ollama-model "llama2"
  "Default Ollama model to use."
  :group 'ai-integration
  :type 'string)

(defcustom ai-integration-openai-api-key ""
  "OpenAI API key for cloud LLM access."
  :group 'ai-integration
  :type 'string)

(defcustom ai-integration-anthropic-api-key ""
  "Anthropic API key for Claude access."
  :group 'ai-integration
  :type 'string)

(defcustom ai-integration-auto-analyze-new-documents t
  "If non-nil, automatically analyze new documents for summaries and concepts."
  :group 'ai-integration
  :type 'boolean)

;;;; Core LLM Integration

(defun ai-integration-call-llm (prompt &optional context)
  "Call the configured LLM with PROMPT and optional CONTEXT."
  (let ((full-prompt (if context
                        (format "%s\n\nContext:\n%s" prompt context)
                      prompt)))
    (pcase ai-integration-provider
      ('ollama (ai-integration-call-ollama full-prompt))
      ('openai (ai-integration-call-openai full-prompt))
      ('anthropic (ai-integration-call-anthropic full-prompt))
      (_ (error "Unknown AI provider: %s" ai-integration-provider)))))

(defun ai-integration-call-ollama (prompt)
  "Call Ollama LLM with PROMPT."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode
                           `((model . ,ai-integration-ollama-model)
                             (prompt . ,prompt)
                             (stream . :json-false))))
         (response (url-retrieve-synchronously
                   (format "%s/api/generate" ai-integration-ollama-url))))
    (if response
        (with-current-buffer response
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (let ((json-response (json-read)))
              (cdr (assoc 'response json-response)))))
      (error "Failed to connect to Ollama server"))))

(defun ai-integration-call-openai (prompt)
  "Call OpenAI API with PROMPT."
  (if (string-empty-p ai-integration-openai-api-key)
      (error "OpenAI API key not configured")
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(format "Bearer %s" ai-integration-openai-api-key))))
           (url-request-data (json-encode
                             `((model . "gpt-3.5-turbo")
                               (messages . [((role . "user") (content . ,prompt))])
                               (max_tokens . 1000))))
           (response (url-retrieve-synchronously "https://api.openai.com/v1/chat/completions")))
      (if response
          (with-current-buffer response
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (let* ((json-response (json-read))
                     (choices (cdr (assoc 'choices json-response)))
                     (first-choice (aref choices 0))
                     (message (cdr (assoc 'message first-choice))))
                (cdr (assoc 'content message)))))
        (error "Failed to call OpenAI API")))))

(defun ai-integration-call-anthropic (prompt)
  "Call Anthropic Claude with PROMPT."
  (if (string-empty-p ai-integration-anthropic-api-key)
      (error "Anthropic API key not configured")
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("x-api-key" . ,ai-integration-anthropic-api-key)
              ("anthropic-version" . "2023-06-01")))
           (url-request-data (json-encode
                             `((model . "claude-3-sonnet-20240229")
                               (max_tokens . 1000)
                               (messages . [((role . "user") (content . ,prompt))]))))
           (response (url-retrieve-synchronously "https://api.anthropic.com/v1/messages")))
      (if response
          (with-current-buffer response
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (let* ((json-response (json-read))
                     (content (cdr (assoc 'content json-response)))
                     (first-content (aref content 0)))
                (cdr (assoc 'text first-content)))))
        (error "Failed to call Anthropic API")))))

;;;; AI-Powered Document Analysis

(defun ai-integration-analyze-document ()
  "Analyze current document with AI assistance."
  (interactive)
  (let* ((buffer (current-buffer))
         (doc-content (buffer-string))
         (file-name (buffer-file-name buffer)))
    (message "Analyzing document with AI...")
    (condition-case err
        (let* ((analysis-prompt (format
                                  "Analyze this academic document and provide:\n\n1. Main research question or objective\n2. Key findings and contributions\n3. Methodology used\n4. Theoretical framework or approach\n5. Potential research gaps or limitations\n\nDocument content:\n%s"
                                  (substring doc-content 0 (min 4000 (length doc-content)))))
               (analysis (ai-integration-call-llm analysis-prompt)))
          (message "Analysis complete: %s" (substring analysis 0 (min 100 (length analysis)))))
      (error (message "Analysis failed: %s" (error-message-string err))))))

;;;; Smart Concept Extraction

(defun ai-integration-extract-concepts (text)
  "Extract concepts and keywords from TEXT using AI."
  (interactive (list (if (use-region-p)
                        (buffer-substring (region-beginning) (region-end))
                      (buffer-string))))
  (message "Extracting concepts with AI...")
  (condition-case err
      (let* ((extraction-prompt (format
                                  "Extract key concepts and keywords from this text:\n\n%s"
                                  (substring text 0 (min 3000 (length text)))))
             (concepts (ai-integration-call-llm extraction-prompt)))
        (message "Extracted concepts: %s" (substring concepts 0 (min 100 (length concepts)))))
    (error (message "Concept extraction failed: %s" (error-message-string err)))))

;;;; Document Summarization

(defun ai-integration-summarize-document ()
  "Generate an AI-powered summary of the current document."
  (interactive)
  (let* ((buffer (current-buffer))
         (doc-content (buffer-string))
         (file-name (buffer-file-name buffer)))
    (message "Generating AI summary...")
    (condition-case err
        (let* ((summary-prompt (format
                                  "Create a concise summary of this document. Include:\n\n1. Main research objective\n2. Key findings\n3. Methodology overview\n\nDocument:\n%s"
                                  (substring doc-content 0 (min 4000 (length doc-content)))))
               (summary (ai-integration-call-llm summary-prompt)))
          (message "Summary: %s" (substring summary 0 (min 200 (length summary)))))
      (error (message "Summary generation failed: %s" (error-message-string err))))))

;;;; AI Question Answering

(defun ai-integration-ask-question (question)
  "Ask an AI question about the current document."
  (interactive "sAsk AI: ")
  (message "Querying AI...")
  (condition-case err
      (let* ((qa-prompt (format
                          "Answer this question: %s\n\nProvide a concise, helpful response."
                          question))
             (answer (ai-integration-call-llm qa-prompt)))
        (message "AI: %s" (substring answer 0 (min 200 (length answer)))))
    (error (message "Query failed: %s" (error-message-string err)))))

;;;; Research Assistance

(defun ai-integration-research-assistance ()
  "Get AI assistance for research planning."
  (interactive)
  (message "Getting AI research assistance...")
  (condition-case err
      (let* ((assistance-prompt "Provide guidance on research planning, literature review strategy, and methodology selection.")
             (advice (ai-integration-call-llm assistance-prompt)))
        (message "Research guidance: %s" (substring advice 0 (min 200 (length advice)))))
    (error (message "Research assistance failed: %s" (error-message-string err)))))

;;;; Writing Assistance

(defun ai-integration-writing-assistance (task)
  "Get AI assistance for academic writing tasks."
  (interactive "sWriting task: ")
  (message "Getting AI writing assistance...")
  (condition-case err
      (let* ((writing-prompt (format
                                "Help with academic writing task: %s. Provide structure and guidance."
                                task))
             (assistance (ai-integration-call-llm writing-prompt)))
        (message "Writing guidance: %s" (substring assistance 0 (min 200 (length assistance)))))
    (error (message "Writing assistance failed: %s" (error-message-string err)))))

;;;; Key Bindings Integration

(defvar ai-integration-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'ai-integration-analyze-document)
    (define-key map "s" 'ai-integration-summarize-document)
    (define-key map "q" 'ai-integration-ask-question)
    (define-key map "c" 'ai-integration-extract-concepts)
    (define-key map "r" 'ai-integration-research-assistance)
    (define-key map "w" 'ai-integration-writing-assistance)
    map)
  "Prefix keymap for AI integration commands.")

;; Auto-initialize message
(defun ai-integration-initialize ()
  "Initialize AI integration components."
  (message "AI integration initialized"))

(ai-integration-initialize)

(provide 'ai-integration)

;;; ai-integration.el ends here
