;;; academic-apis.el --- Integration with academic APIs -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This file provides integration with academic APIs including:
;; - arXiv API for physics, math, computer science papers
;; - PubMed API for biomedical literature
;; - Semantic Scholar API for citation data
;; - CrossRef API for DOI resolution

;;; Code:

(require 'url)
(require 'json)
(require 'org)
(eval-when-compile (require 'subr-x))

(defgroup academic-apis ()
  "Integration with academic APIs."
  :group 'scientific-mapping)

;;;; arXiv Integration

(defcustom academic-apis-arxiv-max-results 50
  "Maximum number of results to fetch from arXiv."
  :group 'academic-apis
  :type 'integer)

(defun academic-apis-search-arxiv (query)
  "Search arXiv for papers matching QUERY."
  (interactive "sSearch arXiv: ")
  (message "Searching arXiv for: %s" query)
  (let* ((encoded-query (url-hexify-string query))
         (url (format "http://export.arxiv.org/api/query?search_query=all:%s&start=0&max_results=%d"
                     encoded-query academic-apis-arxiv-max-results))
         (response (url-retrieve-synchronously url)))
    (if response
        (with-current-buffer response
          (goto-char (point-min))
          (when (re-search-forward "<entry>" nil t)
            (let ((results (academic-apis-parse-arxiv-xml (buffer-string))))
              (academic-apis-display-arxiv-results results query))))
      (message "Failed to connect to arXiv API"))))

(defun academic-apis-parse-arxiv-xml (xml)
  "Parse arXiv XML response into structured data."
  (let ((results '()))
    (with-temp-buffer
      (insert xml)
      (goto-char (point-min))
      (while (re-search-forward "<entry>" nil t)
        (let ((entry-start (match-beginning 0)))
          (when (re-search-forward "</entry>" nil t)
            (let ((entry-xml (buffer-substring entry-start (match-end 0))))
              (push (academic-apis-parse-arxiv-entry entry-xml) results)))))
      (reverse results))))

(defun academic-apis-parse-arxiv-entry (xml)
  "Parse individual arXiv entry XML."
  (let ((title "") (authors '()) (abstract "") (doi "") (url "") (date ""))
    ;; Extract title
    (when (string-match "<title>\\([^<]+\\)</title>" xml)
      (setq title (match-string 1 xml)))
    ;; Extract authors
    (let ((author-matches ()))
      (with-temp-buffer
        (insert xml)
        (goto-char (point-min))
        (while (re-search-forward "<name>\\([^<]+\\)</name>" nil t)
          (push (match-string 1) authors)))
      (setq authors (reverse authors)))
    ;; Extract abstract
    (when (string-match "<summary>\\([^<]+\\)</summary>" xml)
      (setq abstract (replace-regexp-in-string "\n" " " (match-string 1 xml))))
    ;; Extract DOI
    (when (string-match "<arxiv:doi>\\([^<]+\\)</arxiv:doi>" xml)
      (setq doi (match-string 1 xml)))
    ;; Extract URL
    (when (string-match "<id>\\([^<]+\\)</id>" xml)
      (setq url (match-string 1 xml)))
    ;; Extract date
    (when (string-match "<published>\\([^<]+\\)</published>" xml)
      (setq date (match-string 1 xml)))

    `((title . ,title)
      (authors . ,authors)
      (abstract . ,abstract)
      (doi . ,doi)
      (url . ,url)
      (date . ,date)
      (source . "arXiv"))))

(defun academic-apis-display-arxiv-results (results query)
  "Display arXiv search results in an Org buffer."
  (let ((buffer (get-buffer-create "*arXiv Search Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: arXiv Search Results: %s\n\n" query))
      (insert (format "* Search Results (%d papers)\n\n" (length results)))

      (dolist (paper results)
        (let ((title (cdr (assoc 'title paper)))
              (authors (cdr (assoc 'authors paper)))
              (abstract (cdr (assoc 'abstract paper)))
              (doi (cdr (assoc 'doi paper)))
              (url (cdr (assoc 'url paper)))
              (date (cdr (assoc 'date paper))))
          (insert "** " title "\n")
          (insert ":PROPERTIES:\n")
          (insert (format ":DOI: %s\n" (or doi "")))
          (insert (format ":URL: %s\n" (or url "")))
          (insert (format ":DATE: %s\n" (or date "")))
          (insert ":SOURCE: arXiv\n")
          (insert ":END:\n")
          (insert "*** Authors\n")
          (dolist (author authors)
            (insert (format "- %s\n" author)))
          (insert "\n*** Abstract\n")
          (insert (or abstract "No abstract available") "\n\n")
          (insert "*** Actions\n")
          (insert "- [ ] Download PDF\n")
          (insert "- [ ] Add to citation database\n")
          (insert "- [ ] Import to scientific mapping\n")
          (insert "- [ ] Schedule for review\n\n")))

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; PubMed Integration

(defcustom academic-apis-pubmed-api-key ""
  "PubMed API key for higher rate limits."
  :group 'academic-apis
  :type 'string)

(defcustom academic-apis-pubmed-max-results 20
  "Maximum number of results to fetch from PubMed."
  :group 'academic-apis
  :type 'integer)

(defun academic-apis-search-pubmed (query)
  "Search PubMed for papers matching QUERY."
  (interactive "sSearch PubMed: ")
  (message "Searching PubMed for: %s" query)
  (let* ((encoded-query (url-hexify-string query))
         (url (format "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=%s&retmax=%d&retmode=json%s"
                     encoded-query
                     academic-apis-pubmed-max-results
                     (if (not (string-empty-p academic-apis-pubmed-api-key))
                         (format "&api_key=%s" academic-apis-pubmed-api-key)
                       "")))
         (response (url-retrieve-synchronously url)))
    (if response
        (with-current-buffer response
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (let* ((json-response (json-read))
                   (ids (cdr (assoc 'idlist (cdr (assoc 'esearchresult json-response))))))
              (if ids
                  (academic-apis-fetch-pubmed-details ids)
                (message "No PubMed results found")))))
      (message "Failed to connect to PubMed API"))))

(defun academic-apis-fetch-pubmed-details (ids)
  "Fetch detailed information for PubMed IDS."
  (let* ((id-string (string-join ids ","))
         (url (format "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=%s&retmode=json%s"
                     id-string
                     (if (not (string-empty-p academic-apis-pubmed-api-key))
                         (format "&api_key=%s" academic-apis-pubmed-api-key)
                       "")))
         (response (url-retrieve-synchronously url)))
    (if response
        (with-current-buffer response
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (let* ((json-response (json-read))
                   (results (academic-apis-parse-pubmed-json json-response)))
              (academic-apis-display-pubmed-results results))))
      (message "Failed to fetch PubMed details"))))

(defun academic-apis-parse-pubmed-json (json)
  "Parse PubMed JSON response."
  (let ((results '())
        (result-data (cdr (assoc 'result json))))
    (maphash
     (lambda (key value)
       (when (not (string= key "uids"))
         (let ((title (cdr (assoc 'title value)))
               (authors (mapcar (lambda (author)
                                (cdr (assoc 'name (cdr (assoc 'name author)))))
                              (cdr (assoc 'authors value))))
               (abstract (cdr (assoc 'abstract value)))
               (doi (cdr (assoc 'elocationid value)))
               (pmid key)
               (journal (cdr (assoc 'fulljournalname (cdr (assoc 'source value)))))
               (pubdate (cdr (assoc 'pubdate (cdr (assoc 'source value))))))
           (push `((title . ,title)
                   (authors . ,authors)
                   (abstract . ,abstract)
                   (doi . ,doi)
                   (pmid . ,pmid)
                   (journal . ,journal)
                   (date . ,pubdate)
                   (source . "PubMed"))
                 results))))
     result-data)
    results))

(defun academic-apis-display-pubmed-results (results)
  "Display PubMed search results in an Org buffer."
  (let ((buffer (get-buffer-create "*PubMed Search Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: PubMed Search Results\n\n")
      (insert (format "* Search Results (%d papers)\n\n" (length results)))

      (dolist (paper results)
        (let ((title (cdr (assoc 'title paper)))
              (authors (cdr (assoc 'authors paper)))
              (abstract (cdr (assoc 'abstract paper)))
              (doi (cdr (assoc 'doi paper)))
              (pmid (cdr (assoc 'pmid paper)))
              (journal (cdr (assoc 'journal paper)))
              (date (cdr (assoc 'date paper))))
          (insert "** " (or title "Untitled") "\n")
          (insert ":PROPERTIES:\n")
          (insert (format ":DOI: %s\n" (or doi "")))
          (insert (format ":PMID: %s\n" (or pmid "")))
          (insert (format ":JOURNAL: %s\n" (or journal "")))
          (insert (format ":DATE: %s\n" (or date "")))
          (insert ":SOURCE: PubMed\n")
          (insert ":END:\n")
          (when authors
            (insert "*** Authors\n")
            (dolist (author authors)
              (insert (format "- %s\n" author)))
            (insert "\n"))
          (when abstract
            (insert "*** Abstract\n")
            (insert abstract "\n\n"))
          (insert "*** Actions\n")
          (insert "- [ ] Read full paper\n")
          (insert "- [ ] Add to citation database\n")
          (insert "- [ ] Import to scientific mapping\n")
          (insert "- [ ] Schedule for review\n\n")))

      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; CrossRef DOI Resolution

(defun academic-apis-resolve-doi (doi)
  "Resolve DOI using CrossRef API."
  (interactive "sEnter DOI: ")
  (message "Resolving DOI: %s" doi)
  (let* ((encoded-doi (url-hexify-string doi))
         (url (format "https://api.crossref.org/works/%s" encoded-doi))
         (response (url-retrieve-synchronously url)))
    (if response
        (with-current-buffer response
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (let* ((json-response (json-read))
                   (message (cdr (assoc 'message json-response))))
              (academic-apis-display-doi-info message doi))))
      (message "Failed to resolve DOI"))))

(defun academic-apis-display-doi-info (info doi)
  "Display resolved DOI information."
  (let ((buffer (get-buffer-create "*DOI Resolution*"))
        (title (cdr (assoc 'title info)))
        (authors (cdr (assoc 'author info)))
        (journal (cdr (assoc 'container-title info)))
        (year (cdr (assoc 'published-print info)))
        (url (cdr (assoc 'URL info))))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: DOI Resolution\n\n")
      (insert (format "* %s\n\n" (if (listp title) (car title) title)))
      (insert ":PROPERTIES:\n")
      (insert (format ":DOI: %s\n" doi))
      (insert (format ":URL: %s\n" (or url "")))
      (insert ":SOURCE: CrossRef\n")
      (insert ":END:\n")
      (when authors
        (insert "** Authors\n")
        (dolist (author authors)
          (let ((given (cdr (assoc 'given author)))
                (family (cdr (assoc 'family author))))
            (insert (format "- %s %s\n" (or given "") (or family "")))))
        (insert "\n"))
      (when journal
        (insert (format "** Journal: %s\n" (if (listp journal) (car journal) journal))))
      (when year
        (insert (format "** Year: %s\n" (if (listp year) (car year) year))))
      (insert "\n** Actions\n")
      (insert "- [ ] Import to scientific mapping\n")
      (insert "- [ ] Add to citation database\n")
      (insert "- [ ] Download PDF\n")
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;; Integration with Scientific Mapping

(defun academic-apis-import-to-scientific-mapping (source)
  "Import search results from SOURCE into scientific mapping."
  (interactive
   (list (completing-read "Import from: " '("arXiv" "PubMed" "DOI") nil t)))
  (cond
   ((string= source "arXiv")
    (call-interactively 'academic-apis-search-arxiv))
   ((string= source "PubMed")
    (call-interactively 'academic-apis-search-pubmed))
   ((string= source "DOI")
    (call-interactively 'academic-apis-resolve-doi))))

(provide 'academic-apis)

;;; academic-apis.el ends here