;;; install-config.el --- Installation and configuration for scientific-mapping -*- lexical-binding: t; -*-

;; This file provides an example setup for installing and configuring
;; the scientific-mapping package using straight.el and leaf.
;; This file is optional and only needed for initial setup.

;; Only load if straight.el is available
(when (require 'straight nil t)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (when (require 'leaf nil t)
    (straight-use-package 'leaf)

    (leaf scientific-mapping
      :straight t
      :require t
      :config
      (scientific-mapping-mode 1))

    (leaf org
      :straight t
      :config
      (require 'org-faces))))

(provide 'core/install-config)

;;; install-config.el ends here
