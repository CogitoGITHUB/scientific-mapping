;;; doc/templates.el --- Document templates  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Document template functions.
;; - Template definitions
;; - Template insertion

;;; Code:

(defgroup scientific-mapping-templates nil
  "Document template settings."
  :group 'scientific-mapping)

(defcustom scientific-mapping-template-dir
  (expand-file-name "templates" user-emacs-directory)
  "Directory for document templates."
  :group 'scientific-mapping-templates
  :type 'directory)

(defun scientific-mapping-template-insert (template-name)
  "Insert TEMPLATE-NAME at point."
  (interactive
   (list (completing-read "Template: "
                          (scientific-mapping-template--list))))
  (let ((template (scientific-mapping-template--load template-name)))
    (when template
      (insert template))))

(defun scientific-mapping-template--list ()
  "List available templates."
  (directory-files scientific-mapping-template-dir nil "\\.org$"))

(defun scientific-mapping-template--load (name)
  "Load template NAME."
  (let ((path (expand-file-name (format "%s.org" name)
                                scientific-mapping-template-dir)))
    (when (file-exists-p path)
      (scientific-mapping--read-file path))))

(defun scientific-mapping-template-research-article ()
  "Insert research article template."
  (interactive)
  (insert "* Introduction\n\n"
           "* Methods\n\n"
           "* Results\n\n"
           "* Discussion\n\n"
           "* Conclusion\n\n"
           "* References\n\n"))

(defun scientific-mapping-template-literature-review ()
  "Insert literature review template."
  (interactive)
  (insert "* Abstract\n\n"
           "* Introduction\n\n"
           "* Methodology\n\n"
           "* Findings\n\n"
           "* Discussion\n\n"
           "* Conclusion\n\n"
           "* References\n\n"))

(defun scientific-mapping-template-conference-paper ()
  "Insert conference paper template."
  (interactive)
  (insert "* Abstract\n\n"
           "* Introduction\n\n"
           "* Related Work\n\n"
           "* Methodology\n\n"
           "* Results\n\n"
           "* Conclusion\n\n"
           "* References\n\n"))

(provide 'doc/templates)
;;; doc/templates.el ends here
