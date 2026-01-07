;;; strength-calculator.el --- Relationship strength calculation -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System

;;; Commentary:

;; This module calculates relationship strength/confidence scores
;; based on relationship type, source, and metadata.

;;; Code:

(require 'cl-lib)

(defgroup relationship-strength ()
  "Relationship strength calculation."
  :group 'concept-relationships)

(defcustom relationship-strength-default 0.5
  "Default strength for relationships without explicit strength."
  :group 'relationship-strength
  :type 'float)

(defcustom relationship-strength-weights
  '((parent . 0.9)
    (child . 0.9)
    (sibling . 0.6)
    (related . 0.5)
    (friend . 0.4)
    (evidence . 0.8)
    (method . 0.7)
    (evolution . 0.6)
    (supports . 0.8)
    (contradicts . 0.7)
    (builds-on . 0.85)
    (leads-to . 0.65)
    (cites . 0.8)
    (cited-by . 0.8)
    (extends . 0.85)
    (related-to . 0.5)
    (supplements . 0.7)
    (response-to . 0.75)
    (cross-reference . 0.6)
    (internal-link . 0.7))
  "Base strength weights for different relationship types."
  :group 'relationship-strength
  :type '(alist :key-type symbol :value-type float))

(defcustom relationship-source-weights
  '((property . 1.0)
    (citation . 0.95)
    (link . 0.8)
    (manual . 0.9)
    (ai . 0.7))
  "Strength multipliers based on relationship source."
  :group 'relationship-strength
  :type '(alist :key-type symbol :value-type float))

(defun relationship-strength-calculate (relationship)
  "Calculate strength for RELATIONSHIP.
RELATIONSHIP is a plist with :type, :source, :strength, and :metadata keys."
  (let* ((type (plist-get relationship :type))
         (source (plist-get relationship :source))
         (explicit-strength (plist-get relationship :strength))
         (metadata (plist-get relationship :metadata))
         (base-strength (or explicit-strength
                           (cdr (assoc (intern type) relationship-strength-weights))
                           relationship-strength-default))
         (source-multiplier (or (cdr (assoc source relationship-source-weights)) 0.8)))
    (* base-strength source-multiplier)))

(defun relationship-strength-normalize (strength)
  "Normalize STRENGTH to 0.0-1.0 range."
  (max 0.0 (min 1.0 strength)))

(defun relationship-strength-classify (strength)
  "Classify STRENGTH into category: strong, moderate, weak."
  (cond
   ((>= strength 0.8) 'strong)
   ((>= strength 0.5) 'moderate)
   (t 'weak)))

(defun relationship-strength-color (strength)
  "Get color for STRENGTH level (hex string)."
  (cond
   ((>= strength 0.8) "#22c55e")
   ((>= strength 0.5) "#eab308")
   (t "#ef4444")))

(defun relationship-strength-description (type)
  "Get description for relationship TYPE."
  (pcase (intern type)
    ('parent "is a specialization of")
    ('child "is a generalization of")
    ('sibling "is related to")
    ('related "is associated with")
    ('friend "connects different disciplines")
    ('evidence "provides support for")
    ('method "is used by")
    ('evolution "evolved from")
    ('supports "supports")
    ('contradicts "contrasts with")
    ('builds-on "extends")
    ('leads-to "influences")
    ('cites "references")
    ('cited-by "is referenced by")
    ('extends "continues")
    ('related-to "is thematically related to")
    ('supplements "adds context to")
    ('response-to "responds to")
    (t "connects")))

(defun relationship-strength-annotate (relationship)
  "Annotate RELATIONSHIP with calculated strength metadata."
  (let* ((strength (relationship-strength-normalize
                    (relationship-strength-calculate relationship)))
         (category (relationship-strength-classify strength))
         (color (relationship-strength-color strength))
         (type (plist-get relationship :type))
         (description (relationship-strength-description type)))
    (plist-put relationship :strength strength)
    (plist-put relationship :strength-category category)
    (plist-put relationship :strength-color color)
    (plist-put relationship :strength-description description)
    relationship))

(defun relationship-strength-annotate-list (relationships)
  "Annotate all RELATIONSHIPS with strength metadata."
  (mapcar #'relationship-strength-annotate relationships))

(defun relationship-strength-summarize (relationships)
  "Summarize strength distribution in RELATIONSHIPS."
  (let* ((annotated (relationship-strength-annotate-list relationships))
         (strong (cl-count-if (lambda (r) (eq (plist-get r :strength-category) 'strong)) annotated))
         (moderate (cl-count-if (lambda (r) (eq (plist-get r :strength-category) 'moderate)) annotated))
         (weak (cl-count-if (lambda (r) (eq (plist-get r :strength-category) 'weak)) annotated)))
    (list :total (length relationships)
          :strong strong
          :moderate moderate
          :weak weak
          :average-strength (/ (float (cl-reduce #'+ annotated :key (lambda (r) (plist-get r :strength)))) (max 1 (length annotated))))))

(provide 'strength-calculator)
;;; strength-calculator.el ends here
