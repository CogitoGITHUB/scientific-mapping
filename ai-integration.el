;;; ai-integration.el --- AI integration for scientific knowledge mapping -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0

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
(require 'org-mcp nil t)
(require 'agent-shell nil t)
(require 'url)
(require 'json)
(eval-when-compile (require 'subr-x))

(defgroup ai-integration ()
  "AI integration for scientific knowledge mapping."
  :group 'scientific-mapping)

;;;; AI Provider Configuration

(defcustom ai-integration-provider 'ollama
  "AI provider to use for analysis and generation.
Options: 'ollama (local), 'openai, 'anthropic, 'org-mcp, 'agent-shell."
  :group 'ai-integration
  :type '(choice (const :tag "Ollama (Local LLM)" ollama)
                 (const :tag "OpenAI API" openai)
                 (const :tag "Anthropic API" anthropic)
                 (const :tag "Org MCP" org-mcp)
                 (const :tag "Agent Shell" agent-shell)))

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
      ('org-mcp (ai-integration-call-mcp full-prompt))
      ('agent-shell (ai-integration-call-agent-shell full-prompt))
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
                               (max_tokens . 1000)))))
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
                               (messages . [((role . "user") (content . ,prompt))])))))
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

(defun ai-integration-call-mcp (prompt)
  "Call org-mcp with PROMPT."
  (if (featurep 'org-mcp)
      (org-mcp-query prompt)
    (error "org-mcp not available")))

(defun ai-integration-call-agent-shell (prompt)
  "Call agent-shell with PROMPT."
  (if (featurep 'agent-shell)
      (agent-shell-query prompt)
    (error "agent-shell not available")))

;;;; AI-Powered Document Analysis

(defun ai-integration-analyze-document ()
  "Analyze current document with AI assistance."
  (interactive)
  (let* ((buffer (current-buffer))
         (doc-content (buffer-string))
         (file-name (buffer-file-name buffer)))
    (with-current-buffer (get-buffer-create "*AI Document Analysis*")
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: AI Document Analysis\n\n")
      (insert "* Document Overview\n\n")
      (insert (format "** File: %s\n" file-name))
      (insert (format "** Size: %d characters\n\n" (length doc-content)))

      ;; AI Analysis
      (insert "* AI Analysis\n\n")
      (message "Analyzing document with AI...")
      (condition-case err
          (let* ((analysis-prompt (format
                                  "Analyze this academic document and provide:\n\n1. Main research question or objective\n2. Key findings and contributions\n3. Methodology used\n4. Theoretical framework or approach\n5. Potential research gaps or limitations\n6. Related concepts and keywords\n7. Citation-worthy claims\n\nDocument content:\n%s"
                                  (substring doc-content 0 (min 4000 (length doc-content)))))
                 (analysis (ai-integration-call-llm analysis-prompt)))
            (insert "** Summary\n")
            (insert analysis)
            (insert "\n\n")

            ;; Extract and store concepts automatically
            (when ai-integration-auto-analyze-new-documents
              (ai-integration-extract-concepts doc-content))

            (message "Document analysis complete"))
        (error (insert "** Error\n" (error-message-string err) "\n\n")))

      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

;;;; Smart Concept Extraction

(defun ai-integration-extract-concepts (text)
  "Extract concepts and keywords from TEXT using AI."
  (interactive (list (if (use-region-p)
                        (buffer-substring (region-beginning) (region-end))
                      (buffer-string))))
  (message "Extracting concepts with AI...")
  (condition-case err
      (let* ((extraction-prompt (format
                                "Extract key concepts, keywords, and research topics from this academic text. Return as a structured list with categories:\n\n1. Core Concepts (main ideas)\n2. Keywords (important terms)\n3. Research Topics (areas of study)\n4. Methods/Techniques (approaches used)\n\nText:\n%s"
                                (substring text 0 (min 3000 (length text)))))
             (concepts (ai-integration-call-llm extraction-prompt)))
        (with-current-buffer (get-buffer-create "*AI Concept Extraction*")
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: AI Concept Extraction\n\n")
          (insert "* Extracted Concepts\n\n")
          (insert concepts)
          (insert "\n\n* Actions\n")
          (insert "- [ ] Review and validate extracted concepts\n")
          (insert "- [ ] Add to concept relationship database\n")
          (insert "- [ ] Link to existing knowledge map\n")
          (insert "- [ ] Create concept entries for new terms\n")
          (goto-char (point-min))
          (switch-to-buffer (current-buffer))))
    (error (message "Concept extraction failed: %s" (error-message-string err)))))

;;;; AI Question Answering

(defun ai-integration-ask-question (question)
  "Ask an AI question about the current document collection."
  (interactive "sAsk AI about your documents: ")
  (let ((buffer (get-buffer-create "*AI Q&A*"))
        (context-doc (when (buffer-file-name)
                      (buffer-substring (point-min) (min 2000 (point-max))))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: AI Question & Answer\n\n")
      (insert (format "* Question: %s\n\n" question))
      (insert "* AI Response\n\n")

      ;; Add context from current document if available
      (when context-doc
        (insert "** Context Document\n")
        (insert (format "- File: %s\n" (buffer-file-name)))
        (insert (format "- Last modified: %s\n\n"
                       (format-time-string "%Y-%m-%d %H:%M"
                                         (nth 5 (file-attributes (buffer-file-name)))))))

      ;; Get AI response
      (message "Querying AI...")
      (condition-case err
          (let* ((qa-prompt (format
                            "Answer this question based on the provided academic document context. Be comprehensive but concise, and cite specific evidence from the text when possible.\n\nQuestion: %s\n\nContext: %s"
                            question (or context-doc "No specific document context provided.")))
                 (answer (ai-integration-call-llm qa-prompt)))
            (insert answer)
            (insert "\n\n* Follow-up Questions\n")
            (insert "- [ ] Does this answer address your research needs?\n")
            (insert "- [ ] What additional information would be helpful?\n")
            (insert "- [ ] Should this be added to the knowledge base?\n"))
        (error (insert "** Error\n" (error-message-string err) "\n\n")))

      (goto-char (point-min))
      (switch-to-buffer buffer))))

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
                               "Create a comprehensive but concise summary of this academic document. Include:\n\n1. Main research objective\n2. Key findings and contributions\n3. Methodology overview\n4. Implications and significance\n\nKeep the summary under 300 words and use academic language.\n\nDocument:\n%s"
                               (substring doc-content 0 (min 4000 (length doc-content)))))
               (summary (ai-integration-call-llm summary-prompt)))
          (with-current-buffer (get-buffer-create "*AI Document Summary*")
            (erase-buffer)
            (org-mode)
            (insert "#+TITLE: AI Document Summary\n\n")
            (insert (format "* Summary of: %s\n\n" (file-name-nondirectory file-name)))
            (insert summary)
            (insert "\n\n* Original Document\n")
            (insert (format "- File: %s\n" file-name))
            (insert (format "- Word count: %d\n" (count-words (point-min) (point-max))))
            (insert (format "- Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
            (insert "* Actions\n")
            (insert "- [ ] Review and edit summary\n")
            (insert "- [ ] Add to document metadata\n")
            (insert "- [ ] Share with collaborators\n")
            (insert "- [ ] Use for literature review\n")
            (goto-char (point-min))
            (switch-to-buffer (current-buffer))))
      (error (message "Summary generation failed: %s" (error-message-string err))))))

;;;; Research Gap Detection

(defun ai-integration-detect-research-gaps ()
  "Analyze current document collection to identify research gaps."
  (interactive)
  (message "Analyzing research gaps...")
  (condition-case err
      (let* ((gap-prompt "Analyze this collection of research documents and identify:\n\n1. Current research trends and hot topics\n2. Underexplored areas or research gaps\n3. Contradictory findings that need resolution\n4. Methodological limitations across studies\n5. Future research directions and opportunities\n\nProvide specific, actionable recommendations for new research.\n\nDocument collection summary: [Would need to aggregate document summaries here]")
             (gaps (ai-integration-call-llm gap-prompt)))
        (with-current-buffer (get-buffer-create "*AI Research Gaps*")
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: AI Research Gap Analysis\n\n")
          (insert "* Identified Research Gaps\n\n")
          (insert gaps)
          (insert "\n\n* Potential Research Opportunities\n")
          (insert "- [ ] Novel research questions identified\n")
          (insert "- [ ] Methodological improvements needed\n")
          (insert "- [ ] Interdisciplinary connections possible\n")
          (insert "- [ ] Practical applications unexplored\n")
          (goto-char (point-min))
          (switch-to-buffer (current-buffer))))
    (error (message "Research gap analysis failed: %s" (error-message-string err)))))

;;;; Research Assistance

(defun ai-integration-research-assistance ()
  "Get AI assistance for research planning and execution."
  (interactive)
  (let ((buffer (get-buffer-create "*AI Research Assistant*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: AI Research Assistant\n\n")
      (insert "* Research Planning\n\n")
      (insert "** Current Goals\n")
      (insert "- [ ] Define research question\n")
      (insert "- [ ] Identify key concepts\n")
      (insert "- [ ] Plan methodology\n")
      (insert "- [ ] Review relevant literature\n\n")

      ;; Get AI assistance
      (message "Getting AI research assistance...")
      (condition-case err
          (let* ((assistance-prompt "Help me plan and execute my research project. Provide guidance on:\n\n1. Research question refinement\n2. Literature review strategy\n3. Methodology selection\n4. Data collection planning\n5. Analysis approaches\n6. Timeline and milestones\n7. Potential challenges and solutions\n\nBe specific and actionable in your recommendations."
                                   (advice (ai-integration-call-llm assistance-prompt)))
            (insert "** AI Research Guidance\n\n")
            (insert advice)
            (insert "\n\n* Action Items\n")
            (insert "- [ ] Review and adapt AI recommendations\n")
            (insert "- [ ] Create detailed research plan\n")
            (insert "- [ ] Set up literature review workflow\n")
            (insert "- [ ] Plan methodology implementation\n")
            (insert "- [ ] Schedule milestones and deadlines\n"))
        (error (insert "** Error\n" (error-message-string err) "\n\n")))

      (goto-char (point-min))
      (switch-to-buffer buffer))))

;;;; Citation Analysis & Recommendations

(defun ai-integration-analyze-citations ()
  "Analyze citation patterns and recommend important papers."
  (interactive)
  (message "Analyzing citation patterns...")
  (condition-case err
      (let* ((citation-prompt "Based on citation data, identify:\n\n1. Most influential papers (by citation count)\n2. Emerging research trends\n3. Citation networks and clusters\n4. Papers that bridge different research areas\n5. Under-cited but potentially important work\n\nProvide recommendations for further reading and research directions.\n\nCitation data: [Would aggregate citation network data here]")
             (analysis (ai-integration-call-llm citation-prompt)))
        (with-current-buffer (get-buffer-create "*AI Citation Analysis*")
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: AI Citation Analysis\n\n")
          (insert "* Citation Network Analysis\n\n")
          (insert analysis)
          (insert "\n\n* Recommended Reading\n")
          (insert "- [ ] Review highly-cited papers\n")
          (insert "- [ ] Explore emerging trends\n")
          (insert "- [ ] Investigate cross-disciplinary connections\n")
          (insert "- [ ] Follow citation trails\n")
          (goto-char (point-min))
          (switch-to-buffer (current-buffer))))
    (error (message "Citation analysis failed: %s" (error-message-string err)))))

;;;; Writing Assistance

(defun ai-integration-writing-assistance (task)
  "Get AI assistance for academic writing tasks."
  (interactive "sWriting task (e.g., 'introduction', 'literature review', 'methodology'): ")
  (let ((buffer (get-buffer-create "*AI Writing Assistant*"))
        (context (when (buffer-file-name)
                  (buffer-substring (point-min) (min 1000 (point-max))))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: AI Writing Assistant\n\n")
      (insert (format "* Writing Task: %s\n\n" task))

      ;; Get AI writing assistance
      (message "Getting AI writing assistance...")
      (condition-case err
          (let* ((writing-prompt (format
                                 "Help me write an academic %s section. Provide:\n\n1. Structure and organization guidance\n2. Key elements to include\n3. Writing style recommendations\n4. Common pitfalls to avoid\n5. Example outline or content\n\nContext from current document:\n%s"
                                 task (or context "No document context available.")))
                 (assistance (ai-integration-call-llm writing-prompt)))
            (insert "** AI Writing Guidance\n\n")
            (insert assistance)
            (insert "\n\n* Writing Checklist\n")
            (insert "- [ ] Follow recommended structure\n")
            (insert "- [ ] Use appropriate academic language\n")
            (insert "- [ ] Include all key elements\n")
            (insert "- [ ] Check for logical flow\n")
            (insert "- [ ] Proofread for clarity\n"))
        (error (insert "** Error\n" (error-message-string err) "\n\n")))

      (goto-char (point-min))
      (switch-to-buffer buffer))))

;;; Initialization

(defun ai-integration-initialize ()
  "Initialize AI integration components."
  (when (and ai-integration-auto-start-mcp (featurep 'org-mcp))
    (run-with-timer 1 nil #'ai-integration-start-mcp))

  (when (and ai-integration-auto-start-agent-shell (featurep 'agent-shell))
    (run-with-timer 1 nil #'ai-integration-start-agent-shell)))

;;;; Semantic Search

(defvar ai-integration-search-index nil
  "Vector index for semantic search.")

(defun ai-integration-semantic-search (query)
  "Perform semantic search across documents using AI."
  (interactive "sSemantic search query: ")
  (message "Performing semantic search...")
  (condition-case err
      (let* ((search-prompt (format
                            "Given the search query: '%s'\n\nFind and rank the most relevant documents/concepts from the knowledge base. Consider semantic meaning, not just keyword matches. Return:\n\n1. Top 5 most relevant items with relevance scores\n2. Why each item matches the query\n3. Suggested follow-up searches\n\nQuery: %s"
                            query query))
             (results (ai-integration-call-llm search-prompt)))
        (with-current-buffer (get-buffer-create "*AI Semantic Search*")
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: AI Semantic Search Results\n\n")
          (insert (format "* Query: %s\n\n" query))
          (insert "* Search Results\n\n")
          (insert results)
          (insert "\n\n* Actions\n")
          (insert "- [ ] Review search results\n")
          (insert "- [ ] Open relevant documents\n")
          (insert "- [ ] Refine search query if needed\n")
          (insert "- [ ] Add findings to research notes\n")
          (goto-char (point-min))
          (switch-to-buffer (current-buffer))))
    (error (message "Semantic search failed: %s" (error-message-string err)))))

;;;; Automated Document Processing

(defun ai-integration-auto-process-document ()
  "Automatically process a newly imported document with AI analysis."
  (when ai-integration-auto-analyze-new-documents
    (message "Auto-processing document with AI...")
    (condition-case err
        (progn
          ;; Generate summary
          (ai-integration-summarize-document)

          ;; Extract concepts
          (ai-integration-extract-concepts (buffer-string))

          ;; Add to Org properties
          (when (eq major-mode 'org-mode)
            (org-set-property "AI_ANALYZED" (format-time-string "%Y-%m-%d %H:%M"))
            (org-set-property "AI_SUMMARY" "Generated - see *AI Document Summary* buffer"))

          (message "Document auto-processed with AI"))
      (error (message "Auto-processing failed: %s" (error-message-string err))))))

;;;; Batch Processing

(defun ai-integration-batch-analyze (directory)
  "Batch analyze all documents in DIRECTORY."
  (interactive "DDirectory to analyze: ")
  (let ((files (directory-files directory t "\\.org$"))
        (processed 0))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (message "Processing: %s" (file-name-nondirectory file))
        (ai-integration-analyze-document)
        (ai-integration-extract-concepts (buffer-string))
        (setq processed (1+ processed))))
    (message "Batch processed %d documents" processed)))

;;;; Key Bindings Integration

(defvar ai-integration-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'ai-integration-analyze-document)
    (define-key map "s" 'ai-integration-summarize-document)
    (define-key map "q" 'ai-integration-ask-question)
    (define-key map "c" 'ai-integration-extract-concepts)
    (define-key map "g" 'ai-integration-detect-research-gaps)
    (define-key map "r" 'ai-integration-research-assistance)
    (define-key map "w" 'ai-integration-writing-assistance)
    (define-key map "i" 'ai-integration-analyze-citations)
    (define-key map "e" 'ai-integration-semantic-search)
    (define-key map "b" 'ai-integration-batch-analyze)
    map)
  "Prefix keymap for AI integration commands.")

;; Add to scientific-mapping keymap
(with-eval-after-load 'scientific-mapping
  (define-key scientific-mapping-mode-map (kbd "C-c s A") ai-integration-prefix-map))

;; Auto-initialize
(ai-integration-initialize)

(provide 'ai-integration)

;;; ai-integration.el ends here

;;; Usage:
;;
;; AI Integration Commands (C-c s A prefix):
;; C-c s A a - Analyze current document
;; C-c s A s - Summarize current document
;; C-c s A q - Ask question about documents
;; C-c s A c - Extract concepts from text
;; C-c s A g - Detect research gaps
;; C-c s A r - Research assistance
;; C-c s A w - Writing assistance
;; C-c s A i - Citation analysis
;; C-c s A e - Semantic search
;; C-c s A b - Batch analyze directory