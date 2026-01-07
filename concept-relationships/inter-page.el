;;; inter-page.el --- Inter-page (inter-document) relationship extraction -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Commentary:

;; This module extracts and manages relationships between different documents.
;; Handles citation networks, document similarity, and cross-reference links.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

(defgroup inter-page-relationships ()
  "Inter-page relationship management."
  :group 'concept-relationships)

;;; Configuration

(defcustom inter-page-directory
  (expand-file-name "documents/" user-emacs-directory)
  "Directory containing scientific documents."
  :group 'inter-page-relationships
  :type 'directory)

(defcustom inter-page-concept-directory
  (expand-file-name "test-concepts/" user-emacs-directory)
  "Directory containing concept org files."
  :group 'inter-page-relationships
  :type 'directory)

(defcustom inter-page-relationship-types
  '(("cites" "This document cites another")
    ("cited-by" "Document is cited by another")
    ("extends" "Extends/continues previous work")
    ("contradicts" "Contradicts another document")
    ("related-to" "Topically related")
    ("supplements" "Adds additional content")
    ("response-to" "Responds to another document"))
  "Valid inter-page relationship types."
  :group 'inter-page-relationships
  :type '(repeat (list (string :tag "Type")
                       (string :tag "Description"))))

;;; Document Indexing

(defun inter-page-index-documents (&optional directory)
  "Index all org documents in DIRECTORY."
  (let* ((dir (or directory inter-page-concept-directory))
         (files (and (file-directory-p dir)
                     (directory-files dir t "\\.org$"))))
    (mapcar #'inter-page-extract-document-metadata files)))

(defun inter-page-extract-document-metadata (file)
  "Extract metadata from FILE for relationship building."
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (let* ((title (org-entry-get (point-min) "TITLE"))
             (id (or (org-id-get) (org-id-get-create)))
             (keywords (org-entry-get (point-min) "KEYWORDS"))
             (doi (org-entry-get (point-min) "DOI"))
             (citations (org-entry-get (point-min) "CITES"))
             (related (org-entry-get (point-min) "RELATED_TO"))
             (extends (org-entry-get (point-min) "EXTENDS"))
             (abstract (org-entry-get (point-min) "ABSTRACT")))
        (list :file file
              :id id
              :title (or title (file-name-base file))
              :keywords (when keywords (split-string keywords))
              :doi doi
              :citations (when citations (split-string citations))
              :related-to (when related (split-string related))
              :extends (when extends (split-string extends))
              :abstract abstract
              :last-modified (nth 5 (file-attributes file)))))))

;;; Relationship Extraction

(defun inter-page-extract-citations (file)
  "Extract citation relationships from FILE."
  (let ((rels '()))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (let ((doc-id (or (org-id-get) (org-id-get-create))))
          (goto-char (point-min))
          (while (re-search-forward "\\[\\[cite:@\\([^\\]]+\\]\]" nil t)
            (push (list :from doc-id
                        :to (match-string 1)
                        :type "cites"
                        :source 'citation
                        :file file)
                  rels))
          ;; Also check property-based citations
          (let ((cites (org-entry-get (point-min) "CITES")))
            (dolist (target (when cites (split-string cites)))
              (push (list :from doc-id
                          :to target
                          :type "cites"
                          :source 'property
                          :file file)
                    rels))))))
    (nreverse rels)))

(defun inter-page-extract-extensions (file)
  "Extract extends/continues relationships from FILE."
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (let ((doc-id (or (org-id-get) (org-id-get-create)))
            (extends (org-entry-get (point-min) "EXTENDS")))
        (when extends
          (mapcar (lambda (target)
                    (list :from doc-id
                          :to target
                          :type "extends"
                          :source 'property
                          :file file))
                  (split-string extends)))))))

(defun inter-page-extract-related (file)
  "Extract related-to relationships from FILE."
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (let ((doc-id (or (org-id-get) (org-id-get-create)))
            (related (org-entry-get (point-min) "RELATED_TO")))
        (when related
          (mapcar (lambda (target)
                    (list :from doc-id
                          :to target
                          :type "related-to"
                          :source 'property
                          :file file))
                  (split-string related)))))))

;;; Cross-File Link Extraction

(defun inter-page-extract-cross-links (file)
  "Extract links to other documents in FILE."
  (let ((rels '()))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (let ((doc-id (or (org-id-get) (org-id-get-create))))
          (goto-char (point-min))
          (while (re-search-forward org-link-bracket-re nil t)
            (let* ((link-obj (org-element-context))
                   (type (org-element-property :type link-obj))
                   (path (org-element-property :path link-obj)))
              (when (string= type "id")
                (push (list :from doc-id
                            :to path
                            :type "cross-reference"
                            :source 'link
                            :file file)
                      rels))))))
    (nreverse rels)))

;;; Graph Building

(defun inter-page-build-graph (&optional directory)
  "Build inter-document relationship graph for DIRECTORY."
  (let* ((docs (inter-page-index-documents directory))
         (all-rels '()))
    ;; Extract relationships from each document
    (dolist (doc docs)
      (let ((file (plist-get doc :file)))
        (setq all-rels (append all-rels
                               (inter-page-extract-citations file)
                               (inter-page-extract-extensions file)
                               (inter-page-extract-related file)
                               (inter-page-extract-cross-links file)))))
    (list :documents docs
          :relationships all-rels
          :doc-count (length docs)
          :edge-count (length all-rels))))

;;; Similarity Calculation

(defun inter-page-calculate-similarity (doc1 doc2)
  "Calculate similarity between DOC1 and DOC2 based on keywords."
  (let* ((kw1 (plist-get doc1 :keywords))
         (kw2 (plist-get doc2 :keywords))
         (common (cl-intersection kw1 kw2 :test #'string=))
         (union (cl-union kw1 kw2 :test #'string=)))
    (if (null union)
        0.0
      (/ (float (length common)) (length union)))))

(defun inter-page-find-similar (&optional threshold directory)
  "Find similar documents. THRESHOLD defaults to 0.3."
  (let* ((threshold (or threshold 0.3))
         (docs (inter-page-index-documents directory))
         (similarities '()))
    (cl-loop for i from 0 to (- (length docs) 2)
             do (cl-loop for j from (+ i 1) to (- (length docs) 1)
                        do (let ((sim (inter-page-calculate-similarity
                                      (nth i docs) (nth j docs))))
                         (when (>= sim threshold)
                           (push (list (nth i docs) (nth j docs) sim)
                                 similarities))))
    (nreverse similarities)))

;;; Visualization Helpers with Strength

(defun inter-page-annotate-edge (edge)
  "Annotate EDGE with strength and visualization properties."
  (require 'strength-calculator nil t)
  (let* ((type (plist-get edge :type))
         (source (plist-get edge :source))
         (strength (if (featurep 'strength-calculator)
                       (relationship-strength-calculate edge)
                     0.5))
         (strength-category (if (featurep 'strength-calculator)
                                (relationship-strength-classify strength)
                              'moderate))
         (color (if (featurep 'strength-calculator)
                    (relationship-strength-color strength)
                  "#64748b")))
    (plist-put edge :strength strength)
    (plist-put edge :strength-category strength-category)
    (plist-put edge :color color)
    edge))

(defun inter-page-get-nodes (&optional directory)
  "Get nodes for 3D visualization from DIRECTORY."
  (mapcar (lambda (doc)
            (list :id (plist-get doc :id)
                  :label (plist-get doc :title)
                  :type 'document
                  :keywords (plist-get doc :keywords)
                  :layer 'inter-page
                  :group "concepts"))
          (plist-get (inter-page-build-graph directory) :documents)))

(defun inter-page-get-edges (&optional directory)
  "Get edges for 3D visualization from DIRECTORY."
  (let* ((graph (inter-page-build-graph directory))
         (rels (plist-get graph :relationships)))
    (mapcar #'inter-page-annotate-edge
            (mapcar (lambda (r)
                      (list :from (plist-get r :from)
                            :to (plist-get r :to)
                            :type (plist-get r :type)
                            :source 'inter-page
                            :label (plist-get r :type)))
                    rels))))

(defun inter-page-get-stats (&optional directory)
  "Get statistics for inter-page relationships in DIRECTORY."
  (let* ((graph (inter-page-build-graph directory))
         (edges (inter-page-get-edges directory)))
    (list :nodes (plist-get graph :doc-count)
          :edges (length edges)
          :layer 'inter-page)))

(defun inter-page-merge-with-inner (inner-nodes inner-edges &optional directory)
  "Merge INNER-NODES and INNER-EDGES with inter-page data from DIRECTORY."
  (let* ((inter-nodes (inter-page-get-nodes directory))
         (inter-edges (inter-page-get-edges directory)))
    (list :nodes (append inner-nodes inter-nodes)
          :edges (append inner-edges inter-edges)
          :layer 'combined)))

(provide 'inter-page)
;;; inter-page.el ends here
