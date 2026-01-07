;;; core/ids.el --- ID generation and management  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; ID generation utilities.
;; - DOI-based IDs
;; - Timestamp IDs
;; - UUID-like IDs

;;; Code:

(defgroup scientific-mapping-ids nil
  "ID generation settings for scientific-mapping."
  :group 'scientific-mapping)

(defcustom scientific-mapping-id-format 'timestamp
  "Format for generating document IDs."
  :group 'scientific-mapping-ids
  :type '(choice (const :tag "Timestamp" timestamp)
                 (const :tag "DOI-based" doi)
                 (const :tag "UUID" uuid)))

(defun scientific-mapping--generate-id (&optional type)
  "Generate a document ID. TYPE: 'timestamp, 'doi, or 'uuid."
  (let ((type (or type scientific-mapping-id-format)))
    (pcase type
      ('timestamp (format-time-string "%Y%m%dT%H%M%S"))
      ('uuid (format "%s-%s-%s-%s"
                     (md5 (number-to-string (random)))
                     (md5 (number-to-string (float-time)))
                     (md5 (number-to-string (current-time)))
                     (md5 (user-login-name))))
      (_ (format-time-string "%Y%m%dT%H%M%S")))))

(defun scientific-mapping--doi-to-id (doi)
  "Convert DOI to a filesystem-safe ID."
  (when doi
    (let ((clean-doi (replace-regexp-in-string "/" "_" doi))
          (remove-prefix (replace-regexp-in-string "^[0-9]+\\.[0-9]+/" "" doi)))
      (replace-regexp-in-string "/" "_" remove-prefix))))

(defun scientific-mapping--validate-doi (doi)
  "Validate DOI format."
  (and (stringp doi)
       (string-match-p "10\\.[0-9]\\{4,9\\}/[^[:space:]]" doi)))

(defun scientific-mapping--sluggify (string)
  "Convert STRING to a filesystem-safe slug."
  (let* ((lowercase (downcase string))
         (no-special (replace-regexp-in-string "[^[:alnum:] ]+" "" lowercase))
         (spaces-replaced (replace-regexp-in-string " +" "-" no-special))
         (trimmed (replace-regexp-in-string "-+$" "" spaces-replaced)))
    (replace-regexp-in-string "^-+" "" trimmed)))

(defun scientific-mapping--timestamp-id (&optional time)
  "Generate timestamp-based ID using TIME."
  (format-time-string "%Y%m%dT%H%M%S" (or time (current-time))))

(provide 'core/ids)
;;; core/ids.el ends here
