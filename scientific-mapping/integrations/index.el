;;; integrations/index.el --- External integrations  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team

;;; Commentary:

;; Index file for integrations module. Automatically loads all .el files from this directory.

;;; Code:

(let* ((dir (file-name-directory load-file-name))
       (files (directory-files dir nil "\\.el$")))
  (dolist (file files)
    (unless (string= file "index.el")
      (load (expand-file-name file dir) t t))))

(provide 'integrations/index)
;;; integrations/index.el ends here
