;;; core/paths.el --- Path management  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Path and directory management utilities.
;; - Default directories
;; - Path resolution
;; - Directory creation

;;; Code:

(defgroup scientific-mapping-paths nil
  "Path and directory settings for scientific-mapping."
  :group 'scientific-mapping)

(defcustom scientific-mapping-root-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of scientific-mapping installation."
  :group 'scientific-mapping-paths
  :type 'directory)

(defcustom scientific-mapping-documents-dir
  (expand-file-name "documents" user-emacs-directory)
  "Default directory for scientific documents."
  :group 'scientific-mapping-paths
  :type 'directory)

(defun scientific-mapping--ensure-dir (dir)
  "Ensure DIR exists, create if needed."
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun scientific-mapping--resolve-dir (dir)
  "Resolve DIR relative to scientific-mapping root."
  (expand-file-name dir scientific-mapping-root-dir))

(defun scientific-mapping--data-dir ()
  "Get the data directory for scientific-mapping."
  (scientific-mapping--ensure-dir
   (expand-file-name ".scientific-mapping" user-emacs-directory)))

(defun scientific-mapping--cache-dir ()
  "Get the cache directory."
  (scientific-mapping--ensure-dir
   (expand-file-name "cache" (scientific-mapping--data-dir))))

(defun scientific-mapping--db-path ()
  "Get the database file path."
  (expand-file-name "citations.sqlite"
                    (scientific-mapping--data-dir)))

(defun scientific-mapping--add-load-path (subdir)
  "Add SUBDIR relative to scientific-mapping root to load-path."
  (let ((path (scientific-mapping--resolve-dir subdir)))
    (add-to-list 'load-path path)
    path))

(provide 'core/paths)
;;; core/paths.el ends here
