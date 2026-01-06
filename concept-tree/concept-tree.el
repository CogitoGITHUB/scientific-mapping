;;; concept-tree.el --- Tree-style concept display for scientific research  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Adapted from org-roam-tree by Brad Stewart
;; Maintainer: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (citation-database "1.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of GNU General Public License as published by
;; Free Software Foundation, either version 3 of License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; concept-tree creates a tree-like structure display for concept
;; relationships in scientific research. It organizes concept connections
;; by their source, creating hierarchical views of theoretical frameworks
;; and research concepts.
;;
;; You can:
;; - Display concept relationships as file-based trees
;; - View citation networks in tree structure
;; - Navigate through concept hierarchies
;; - Expand/collapse concept branches
;; - Reuse display logic for different groupings

;;; Code:

(require 'citation-database)

(defgroup concept-tree nil
  "Tree-style display extensions for scientific concepts."
  :group 'scientific-mapping)

(defcustom concept-tree-default-visible t
  "Whether to collapse all file-level branches after rendering."
  :group 'concept-tree
  :type 'boolean)

;; Enable by adding to concept-relationships sections
;; (add-to-list 'concept-relationships-mode-sections
;;              #'concept-tree-section t)

(defvar concept-tree-visible-state (make-hash-table :test 'equal)
  "Stores fold states for concepts per node.")

(defun concept-tree--visible-state (concept source)
  "Return t if SOURCE under CONCEPT should be folded."
  (gethash (cons concept source) concept-tree-visible-state
          concept-tree-default-visible))

(defun concept-tree--set-visible-state (concept source hidden)
  "Store fold state for SOURCE under CONCEPT."
  (puthash (cons concept source) hidden concept-tree-visible-state))

;; Concept Tree Section

(cl-defun concept-tree-concepts-section (concept &key (section-heading "Concept Tree:"))
  "A tree-style section showing CONCEPT's related concepts grouped by source."
  (concept-tree-section concept :section-heading section-heading
                                 :data-getter #'concept-tree-concepts
                                 :section-id 'concept-tree))

;; Citation Tree Section

(cl-defun concept-tree-citations-section (concept &key (section-heading "Citation Tree:"))
  "A tree-style section showing CONCEPT's citation relationships grouped by source."
  (concept-tree-section concept :section-heading section-heading
                                 :data-getter #'concept-tree-citations
                                 :section-id 'citation-tree))

;; Evidence Tree Section

(cl-defun concept-tree-evidence-section (concept &key (section-heading "Evidence Tree:"))
  "A tree-style section showing CONCEPT's evidence relationships grouped by source."
  (concept-tree-section concept :section-heading section-heading
                                 :data-getter #'concept-tree-evidence
                                 :section-id 'evidence-tree))

;; Generalized Tree Logic

(cl-defun concept-tree-section (node &key (section-heading "Tree Section:") (data-getter #'concept-tree-default) (section-id 'concept-tree))
  "Generalized logic for a tree in concept-relationships buffer.

DATA-GETTER is a function that returns a tree in format:
((parent . (backlink backlink ...)) ...) where parent is a string and
backlinks are relationship objects.

SECTION-ID is a symbol to tag this tree's section in the buffer."
  (with-concept-tree-layout
    (when-let ((tree (funcall data-getter node)))
      (magit-insert-section section-id
        ;; Top-level heading
        (magit-insert-heading section-heading)
        ;; Iterate over sources
        (dolist (source-entry tree)
          (let* ((source (car source-entry))
                 (relationships (cdr source-entry))
                 (visible (concept-tree--visible-state node source)))
            (if visible
                ;; Show source heading and relationships
                (progn
                  (magit-insert-heading (format "  %s" source))
                  (dolist (rel relationships)
                    (magit-insert-heading (format "    %s"
                                                   (concept-tree--format-relationship rel)))
                    (insert "\n")))
              ;; Collapsed: just show indicator
              (magit-insert-heading (format "  %s [collapsed]" source))
              (when (and visible (not (eq visible 'collapsed)))
                (insert "\n")))))))))

(defun concept-tree-concepts (concept-id)
  "Return concept relationships tree for CONCEPT-ID grouped by source file."
  (let* ((concepts (citation-database--get-concept-relations concept-id))
         (tree '()))
    ;; Group by source (file or domain)
    (dolist (concept concepts)
      (let* ((source (plist-get concept :source))
             (concept-name (plist-get concept :name))
             (existing-source (assoc source tree)))
        (if existing-source
            (push concept (cdr existing-source))
          (push (cons source (list concept)) tree))))
    tree))

(defun concept-tree-citations (paper-id)
  "Return citation tree for PAPER-ID grouped by source."
  (let* ((citations (citation-database-get-citations paper-id))
         (tree '()))
    ;; Group by journal or domain
    (dolist (citation citations)
      (let* ((target-id (elt citation 0))
             (citation-type (elt citation 1))
             (context (elt citation 2))
             (target-paper (citation-database-get-paper-by-identifier target-id))
             (source (if target-paper
                          (plist-get target-paper :journal)
                        "Unknown")))
        (when target-paper
          (let* ((existing-source (assoc source tree))
                 (citation-info (list :target-id target-id
                                         :target-title (plist-get target-paper :title)
                                         :type citation-type
                                         :context context)))
            (if existing-source
                (push citation-info (cdr existing-source))
              (push (cons source (list citation-info)) tree)))))
    tree))

(defun concept-tree-evidence (concept-id)
  "Return evidence tree for CONCEPT-ID grouped by source."
  (let* ((evidence (citation-database--get-evidence-relations concept-id))
         (tree '()))
    ;; Group by paper or study
    (dolist (ev evidence)
      (let* ((source (plist-get ev :source))
             (existing-source (assoc source tree)))
        (if existing-source
            (push ev (cdr existing-source))
          (push (cons source (list ev)) tree))))
    tree))

(defun concept-tree-default (node-id)
  "Return default tree with all relationship types for NODE-ID."
  (let* ((concepts (concept-tree-concepts node-id))
         (citations (concept-tree-citations node-id))
         (evidence (concept-tree-evidence node-id))
         (combined-tree `(("Concepts" . ,concepts)
                           ("Citations" . ,citations)
                           ("Evidence" . ,evidence))))
    combined-tree))

(defun concept-tree--format-relationship (rel)
  "Format RELATION object for display."
  (let* ((type (plist-get rel :type))
         (target (plist-get rel :target))
         (context (plist-get rel :context)))
    (concat target
            (when context
              (concat " (" context ") ")")))))

(defun concept-tree-toggle-section (section-id)
  "Toggle visibility of tree SECTION-ID."
  (interactive)
  (let* ((section (completing-read "Toggle section: "
                                    '("concept-tree" "citation-tree" "evidence-tree")))
         (concept-id (org-entry-get (point-min) "IDENTIFIER"))
         (source (read-string "Source to toggle: ")))
    (when (and concept-id source)
      (let ((current-state (concept-tree--visible-state concept-id source)))
        (concept-tree--set-visible-state concept-id source (not current-state))
        (concept-relationships--update-visualization)))))

(provide 'concept-tree)

;;; concept-tree.el ends here

;;; Usage:

;; Add tree sections to concept-relationships visualization:
;; (add-to-list 'concept-relationships-mode-sections
;;              #'concept-tree-concepts-section)
;;
;; (add-to-list 'concept-relationships-mode-sections
;;              #'concept-tree-citations-section)
;;
;; (add-to-list 'concept-relationships-mode-sections
;;              #'concept-tree-evidence-section)