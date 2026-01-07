;;; semantic-zoom.el --- Semantic zoom for 3D visualization -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System

;;; Commentary:

;; This module provides semantic zoom functionality for switching between
;; inner-page, inter-page, and combined views in the 3D visualization.

;;; Code:

(require 'cl-lib)

(defgroup semantic-zoom ()
  "Semantic zoom for visualization layers."
  :group 'concept-relationships)

(defcustom semantic-zoom-thresholds
  '((macro . 100.0)
    (meso . 50.0)
    (micro . 10.0))
  "Zoom distance thresholds for different detail levels."
  :group 'semantic-zoom
  :type '(alist :key-type symbol :value-type float))

(defcustom semantic-zoom-layer-colors
  '((inner-page . "#3b82f6")
    (inter-page . "#22c55e")
    (combined . "#8b5cf6"))
  "Colors for different visualization layers."
  :group 'semantic-zoom
  :type '(alist :key-type symbol :value-type string))

(defvar semantic-zoom-current-layer 'combined
  "Current active visualization layer.")

(defvar semantic-zoom-current-zoom 'meso
  "Current zoom level.")

(defun semantic-zoom-set-layer (layer)
  "Set the active visualization LAYER.
LAYER can be 'inner-page, 'inter-page, or 'combined."
  (setq semantic-zoom-current-layer
        (pcase layer
          ('inner-page 'inner-page)
          ('inter-page 'inter-page)
          ('combined 'combined)
          (_ (error "Invalid layer: %s" layer))))
  (message "Zoom layer set to: %s" semantic-zoom-current-layer))

(defun semantic-zoom-get-layer ()
  "Return the current active layer."
  semantic-zoom-current-layer)

(defun semantic-zoom-set-zoom-level (level)
  "Set zoom LEVEL based on distance."
  (setq semantic-zoom-current-zoom
        (pcase level
          ((pred (lambda (d) (>= d 100.0))) 'macro)
          ((pred (lambda (d) (and (>= d 50.0) (< d 100.0)))) 'meso)
          ((pred (lambda (d) (< d 50.0))) 'micro)
          (_ 'meso))))

(defun semantic-zoom-get-detail-level ()
  "Get detail level based on current zoom."
  (pcase semantic-zoom-current-zoom
    ('macro 'low)
    ('meso 'medium)
    ('micro 'high)))

(defun semantic-zoom-filter-nodes (nodes layer zoom-level)
  "Filter NODES based on LAYER and ZOOM-LEVEL."
  (let ((detail (pcase zoom-level
                  ('macro 'low)
                  ('meso 'medium)
                  ('micro 'high))))
    (cl-case detail
      (low
       (cl-remove-if (lambda (n) (> (plist-get n :level) 1)) nodes))
      (medium
       nodes)
      (high
       nodes))))

(defun semantic-zoom-filter-edges (edges layer zoom-level)
  "Filter EDGES based on LAYER and ZOOM-LEVEL."
  (let ((detail (pcase zoom-level
                  ('macro 'low)
                  ('meso 'medium)
                  ('micro 'high))))
    (cl-case detail
      (low
       (cl-remove-if (lambda (e)
                       (memq (intern (plist-get e :type))
                             '(related internal-link cross-reference)))
                     edges))
      (medium
       edges)
      (high
       edges))))

(defun semantic-zoom-get-config (layer zoom-level)
  "Get visualization configuration for LAYER and ZOOM-LEVEL."
  (let* ((color (cdr (assoc layer semantic-zoom-layer-colors)))
         (threshold (cdr (assoc zoom-level semantic-zoom-thresholds))))
    (list :layer layer
          :zoom-level zoom-level
          :color color
          :threshold threshold
          :node-filter (pcase zoom-level
                         ('macro 'high-level)
                         ('meso 'all)
                         ('micro 'detailed))
          :edge-filter (pcase zoom-level
                         ('macro 'major-only)
                         ('meso 'all)
                         ('micro 'detailed)))))

(defun semantic-zoom-toggle-layer ()
  "Toggle between combined and current layer."
  (interactive)
  (semantic-zoom-set-layer
   (pcase semantic-zoom-current-layer
     ('combined 'inner-page)
     (_ 'combined))))

(defun semantic-zoom-cycle-layers ()
  "Cycle through visualization layers."
  (interactive)
  (semantic-zoom-set-layer
   (pcase semantic-zoom-current-layer
     ('inner-page 'inter-page)
     ('inter-page 'combined)
     ('combined 'inner-page))))

(defun semantic-zoom-description (layer zoom-level)
  "Get description string for LAYER and ZOOM-LEVEL."
  (format "Layer: %s | Zoom: %s"
          (pcase layer
            ('inner-page "Inner-Page")
            ('inter-page "Inter-Page")
            ('combined "Combined"))
          (pcase zoom-level
            ('macro "Overview")
            ('meso "Standard")
            ('micro "Detailed"))))

(provide 'semantic-zoom)
;;; semantic-zoom.el ends here
