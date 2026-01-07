;;; core/files.el --- File operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; File operations utilities.
;; - Reading/writing files
;; - Finding files
;; - File metadata

;;; Code:

(defun scientific-mapping--read-file (path)
  "Read and return contents of PATH as string."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun scientific-mapping--write-file (path content &optional append)
  "Write CONTENT to PATH. APPEND if non-nil."
  (make-directory (file-name-directory path) t)
  (with-temp-buffer
    (insert content)
    (write-file path append)))

(defun scientific-mapping--find-files (dir pattern &optional recursive)
  "Find files in DIR matching PATTERN. RECURSIVE if non-nil."
  (let ((files nil))
    (dolist (file (directory-files dir t pattern))
      (when (file-regular-p file)
        (push file files)))
    (when recursive
      (dolist (subdir (directory-files dir t "[^.]"))
        (when (file-directory-p subdir)
          (setq files (append files (scientific-mapping--find-files subdir pattern t))))))
    (nreverse files)))

(defun scientific-mapping--file-identifier (path)
  "Get the identifier from file at PATH."
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+IDENTIFIER: \\(.+\\)$" nil t)
        (match-string 1)))))

(defun scientific-mapping--file-doi (path)
  "Get the DOI from file at PATH."
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+DOI: \\(.+\\)$" nil t)
        (match-string 1)))))

(defun scientific-mapping--file-title (path)
  "Get the title from file at PATH."
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+TITLE: \\(.+\\)$" nil t)
        (match-string 1)))))

(defun scientific-mapping--file-keywords (path)
  "Get keywords from file at PATH as list."
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+KEYWORDS: \\(.+\\)$" nil t)
        (split-string (match-string 1) ", " t)))))

(provide 'core/files)
;;; core/files.el ends here
