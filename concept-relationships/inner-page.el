;;; inner-page.el --- Inner-page (intra-document) relationship extraction -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Commentary:

;; This module extracts and manages relationships within a single document.
;; Handles org-mode property-based relationships and internal heading links.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)

(defgroup inner-page-relationships ()
  "Inner-page relationship management."
  :group 'concept-relationships)

;;; Relationship Type Definitions

(defvar inner-page-relationship-types
  '(("parent" "Higher-level concept this belongs to")
    ("child" "Sub-concepts contained within")
    ("related" "General association")
    ("supports" "Evidence/citation supporting this concept")
    ("contradicts" "Opposing viewpoint")
    ("builds-on" "Prior work this extends")
    ("leads-to" "Future work/consequences"))
  "Valid inner-page relationship types.")

;;; Property Parsing

(defun inner-page-extract-properties (file)
  "Extract all properties from FILE that define relationships."
  (let ((relationships '()))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (headline)
            (let* ((id (org-element-property :ID headline))
                   (title (org-element-property :raw-value headline))
                   (props (org-element-property :PROPERTIES headline)))
              (dolist (prop-name props)
                (when (string-match-p "^CONCEPT_" (symbol-name (car prop-name)))
                  (let* ((prop-key (symbol-name (car prop-name)))
                    (dolist (related-id (split-string (cdr prop-name)))
                      (push (list :from id
                                  :from-title title
                                  :to related-id
                                  :type (downcase (replace-regexp-in-string "^CONCEPT_" "" prop-key))
                                  :source 'property
                                  :file file)
                            relationships)))))))
          nil nil)))
    (nreverse relationships)))

(defun inner-page-extract-headings (file)
  "Extract all headings with IDs from FILE."
  (let ((headings '()))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (headline)
            (let ((id (org-element-property :ID headline)))
              (when id
                (push (list :id id
                            :title (org-element-property :raw-value headline)
                            :level (org-element-property :level headline)
                            :file file)
                      headings)))))
          nil nil)))
    (nreverse headings)))

;;; Link Parsing

(defun inner-page-extract-links (file)
  "Extract all internal org links from FILE."
  (let ((links '()))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (re-search-forward org-link-bracket-re nil t)
          (let* ((link-start (match-beginning 0))
                 (path (org-element-property :path (org-element-context)))
                 (type (org-element-property :type (org-element-context))))
            (when (and (string= type "id")
                       (not (string= path (buffer-file-name))))
              (push (list :from (org-id-get)
                          :to path
                          :type "internal-link"
                          :source 'link
                          :file file)
                    links))))))
    links))

;;; Relationship Graph Building

(defun inner-page-build-graph (&optional file)
  "Build relationship graph for FILE or current buffer."
  (let* ((target-file (or file (buffer-file-name)))
         (headings (inner-page-extract-headings target-file))
         (property-rels (inner-page-extract-properties target-file))
         (link-rels (inner-page-extract-links target-file)))
    (list :file target-file
          :headings headings
          :relationships (append property-rels link-rels)
          :node-count (length headings)
          :edge-count (+ (length property-rels) (length link-rels)))))

;;; Relationship Display

(defun inner-page-list-relationships (&optional file)
  "List all inner-page relationships for FILE."
  (interactive)
  (let* ((graph (inner-page-build-graph file))
         (rels (plist-get graph :relationships)))
    (if (null rels)
        (message "No inner-page relationships found.")
      (message "Found %d relationships in %s"
               (length rels)
               (plist-get graph :file)))))

;;; Visualization Helpers with Strength

(defun inner-page-annotate-edge (edge)
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

(defun inner-page-get-nodes (&optional file)
  "Get nodes for 3D visualization from FILE."
  (mapcar (lambda (h)
            (list :id (plist-get h :id)
                  :label (plist-get h :title)
                  :type 'heading
                  :level (plist-get h :level)
                  :layer 'inner-page
                  :group (plist-get h :file)))
          (plist-get (inner-page-build-graph file) :headings)))

(defun inner-page-get-edges (&optional file)
  "Get edges for 3D visualization from FILE."
  (let* ((graph (inner-page-build-graph file))
         (rels (plist-get graph :relationships)))
    (mapcar #'inner-page-annotate-edge
            (mapcar (lambda (r)
                      (list :from (plist-get r :from)
                            :to (plist-get r :to)
                            :type (plist-get r :type)
                            :source 'inner-page
                            :label (plist-get r :type)))
                    rels))))

(defun inner-page-get-stats (&optional file)
  "Get statistics for inner-page relationships in FILE."
  (let* ((graph (inner-page-build-graph file))
         (edges (inner-page-get-edges file)))
    (list :nodes (plist-get graph :node-count)
          :edges (length edges)
          :layer 'inner-page)))

(provide 'inner-page)
;;; inner-page.el ends here
