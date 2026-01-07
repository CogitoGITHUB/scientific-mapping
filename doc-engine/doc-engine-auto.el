;;; doc-engine-auto.el --- Frictionless automation system -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/doc-engine
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

;; Frictionless automation system for document processing.

;;; Code:

(require 'doc-engine-core)

;;;; Frictionless Automation System

(defcustom doc-engine-auto-process-on-open t
  "Automatically process documents when opened."
  :group 'doc-engine
  :type 'boolean)

(defcustom doc-engine-auto-process-on-save t
  "Automatically process documents when saved."
  :group 'doc-engine
  :type 'boolean)

(defcustom doc-engine-auto-sync-citations t
  "Automatically sync citations when document changes."
  :group 'doc-engine
  :type 'boolean)

(defcustom doc-engine-auto-extract-concepts t
  "Automatically extract concepts from document content."
  :group 'doc-engine
  :type 'boolean)

;; Debouncing timer for after-save hooks
(defvar doc-engine-auto--debounce-timer nil
  "Timer for debouncing after-save hooks.")

(defcustom doc-engine-auto-debounce-interval 0.3
  "Debounce interval in seconds for after-save hooks."
  :group 'doc-engine
  :type 'number)

;; Hooks for automatic processing
(add-hook 'find-file-hook #'doc-engine-auto-process-on-open)
(add-hook 'after-save-hook #'doc-engine-auto-process-on-save)

(defun doc-engine-auto-process-on-open ()
  "Automatically process document when opened."
  (when (and doc-engine-auto-process-on-open
             (doc-engine-is-scientific-document-p))
    (run-with-timer 0.5 nil #'doc-engine-process-opened-document)))

(defun doc-engine-auto-process-on-save ()
  "Automatically process document when saved.
Uses debouncing to prevent multiple rapid syncs."
  (when (and doc-engine-auto-process-on-save
             (doc-engine-is-scientific-document-p))
    ;; Cancel existing timer and schedule new one (debouncing)
    (when doc-engine-auto--debounce-timer
      (cancel-timer doc-engine-auto--debounce-timer)
      (setq doc-engine-auto--debounce-timer nil))
    (setq doc-engine-auto--debounce-timer
          (run-with-timer doc-engine-auto-debounce-interval nil
                         #'doc-engine-process-saved-document))))

(defun doc-engine-is-scientific-document-p ()
  "Check if current buffer is a scientific document."
  (and (buffer-file-name)
       (member (buffer-file-name) (doc-engine-all-files))))

(defun doc-engine-process-opened-document ()
  "Process document that was just opened."
  (when (doc-engine-is-scientific-document-p)
    ;; Ensure file is registered in ID map
    (let ((id (org-entry-get nil "IDENTIFIER"))
          (filepath (buffer-file-name)))
      (when (and id filepath)
        (doc-engine-register-file id filepath)))

    ;; Auto-sync citations if enabled
    (when doc-engine-auto-sync-citations
      (run-with-timer 1.0 nil #'doc-engine-sync-citations-for-buffer))))

(defun doc-engine-process-saved-document ()
  "Process document that was just saved."
  (when (doc-engine-is-scientific-document-p)
    ;; Update ID mapping
    (let ((id (org-entry-get nil "IDENTIFIER"))
          (filepath (buffer-file-name)))
      (when id
        (doc-engine-register-file id filepath)))

    ;; Auto-sync citations
    (when doc-engine-auto-sync-citations
      (doc-engine-sync-citations-for-buffer))

    ;; Auto-extract concepts
    (when doc-engine-auto-extract-concepts
      (run-with-timer 0.5 nil #'doc-engine-auto-extract-concepts-from-buffer))))

(defun doc-engine-sync-citations-for-buffer ()
  "Sync citations for current buffer."
  (when (and (featurep 'citation-database)
             citation-database-autosync-mode)
    (let ((doi (org-entry-get nil "DOI")))
      (when doi
        (citation-database-index-file (buffer-file-name))))))

(defun doc-engine-auto-extract-concepts-from-buffer ()
  "Automatically extract concepts from current buffer."
  (when (featurep 'ai-integration)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (when (> (length content) 100) ; Only process substantial content
        (condition-case err
            (ai-integration-extract-concepts content)
          (error (message "Auto concept extraction failed: %s" (error-message-string err))))))))

(provide 'doc-engine-auto)

;;; doc-engine-auto.el ends here