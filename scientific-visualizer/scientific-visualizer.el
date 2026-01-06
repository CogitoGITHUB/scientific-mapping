;;; scientific-visualizer.el --- Emacs-based visualization for scientific knowledge networks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/scientific-visualizer
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
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

;; scientific-visualizer provides an Emacs-based visualization interface for
;; exploring scientific knowledge networks. It integrates with the citation-database
;; to provide text-based graphs of papers, concepts, and their relationships.
;;
;; Features include:
;; - Text-based graph layouts of citation networks
;; - Concept mapping and visualization
;; - Multiple visualization modes (citation, concept, author collaboration)
;; - Interactive navigation in Emacs buffers
;; - Filtering and searching capabilities

;;; Code:

(eval-when-compile (require 'subr-x))

;; Load citation-database
(add-to-list 'load-path (expand-file-name "../citation-database" (file-name-directory load-file-name)))
(require 'citation-database)

(defgroup scientific-visualizer ()
  "Emacs-based visualization for scientific knowledge networks."
  :group 'files
  :link '(info-link "(scientific-visualizer) Top")
  :link '(url-link :tag "Homepage" "https://scientific-mapping.org"))

;;;; Configuration

(defcustom scientific-visualizer-visualization-mode 'citation
  "Default visualization mode: `citation', `concept', `author', `journal'."
  :group 'scientific-visualizer
  :type '(choice (const :tag "Citation Network" citation)
                 (const :tag "Concept Map" concept)
                 (const :tag "Author Collaboration" author)
                 (const :tag "Journal Landscape" journal)))

(defcustom scientific-visualizer-max-nodes 50
  "Maximum number of nodes to display in visualizations."
  :group 'scientific-visualizer
  :type 'integer)

;;;; Internal Variables

(defvar scientific-visualizer-buffer-name "*Scientific Visualizer*"
  "Name of the buffer used for visualization.")

;;;; Main Mode

;;;###autoload
(define-minor-mode scientific-visualizer-mode
  "Enable scientific-visualizer for Emacs-based visualization."
  :lighter " SciViz"
  :global t
  :group 'scientific-visualizer
  :init-value nil)

;;;; Visualization Functions

(defun scientific-visualizer--display-graph (nodes edges title)
  "Display a text-based graph with NODES, EDGES, and TITLE."
  (let ((buffer (get-buffer-create scientific-visualizer-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "=== %s ===\n\n" title))
      (insert "Nodes:\n")
      (dolist (node nodes)
        (insert (format "  - %s\n" (scientific-visualizer--format-node node))))
      (insert "\nEdges:\n")
      (dolist (edge edges)
        (insert (format "  - %s → %s\n" (car edge) (cadr edge))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

(defun scientific-visualizer--format-node (node)
  "Format a NODE for display."
  (if (listp node)
      (format "%s (%s)" (cadr node) (car node))
    (format "%s" node)))

;;;; Data Retrieval

(defun scientific-visualizer--get-citation-network ()
  "Get citation network data for visualization."
  (let* ((network (citation-database-citation-network 2 scientific-visualizer-max-nodes))
         (nodes (plist-get network :nodes))
         (edges (plist-get network :edges)))
    (list :nodes nodes :edges edges :title "Citation Network")))

(defun scientific-visualizer--get-concept-network ()
  "Get concept network data for visualization."
  (let* ((concepts (citation-database-search-concepts "" scientific-visualizer-max-nodes))
         (nodes (mapcar (lambda (c)
                          (list (elt c 0) (elt c 1) (elt c 4)))
                        concepts))
         (edges '())) ; Concept relationships would be added here
    (list :nodes nodes :edges edges :title "Concept Network")))

(defun scientific-visualizer--get-author-network ()
  "Get author collaboration network data for visualization."
  (let* ((authors (emacsql citation-database-db
                           [:select id name :from authors :limit $s1]
                           scientific-visualizer-max-nodes))
         (nodes (mapcar (lambda (a)
                          (list (elt a 0) (elt a 1) 1))
                        authors))
         (edges '())) ; Author collaborations from co-authorships
    (list :nodes nodes :edges edges :title "Author Network")))

(defun scientific-visualizer--get-journal-network ()
  "Get journal network data for visualization."
  (let* ((journals (emacsql citation-database-db
                            [:select id name impact-factor :from journals :limit $s1]
                            scientific-visualizer-max-nodes))
         (nodes (mapcar (lambda (j)
                          (list (elt j 0) (elt j 1) (or (elt j 2) 0.0)))
                        journals))
         (edges '())) ; Journal similarity edges
    (list :nodes nodes :edges edges :title "Journal Network")))

;;;; Interactive Commands

;;;###autoload
(defun scientific-visualizer-open ()
  "Open the scientific visualizer in an Emacs buffer."
  (interactive)
  (unless scientific-visualizer-mode (scientific-visualizer-mode))
  (let* ((data (pcase scientific-visualizer-visualization-mode
                 ('citation (scientific-visualizer--get-citation-network))
                 ('concept (scientific-visualizer--get-concept-network))
                 ('author (scientific-visualizer--get-author-network))
                 ('journal (scientific-visualizer--get-journal-network))))
         (nodes (plist-get data :nodes))
         (edges (plist-get data :edges))
         (title (plist-get data :title)))
    (scientific-visualizer--display-graph nodes edges title)))

;;;###autoload
(defun scientific-visualizer-set-mode (mode)
  "Set visualization mode to MODE (`citation', `concept', `author', `journal')."
  (interactive
   (list (completing-read "Visualization mode: "
                          '("citation" "concept" "author" "journal"))))
  (setq scientific-visualizer-visualization-mode (intern mode))
  (message "Visualization mode set to: %s" mode))

(provide 'scientific-visualizer)

;;; scientific-visualizer.el ends here