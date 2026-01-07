;;; doc/index.el --- Document management  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Commentary:

;; Index file for doc module. Automatically loads all .el files from this directory.

;;; Code:

(let* ((dir (file-name-directory load-file-name))
       (files (directory-files dir nil "\\.el$")))
  ;; Load doc-engine first as other files may depend on it
  (load (expand-file-name "doc-engine" dir) t t)
  (dolist (file files)
    (unless (string= file "index.el")
      (load (expand-file-name file dir) t t))))

(provide 'doc/index)
;;; doc/index.el ends here
