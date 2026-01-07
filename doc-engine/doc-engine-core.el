;;; doc-engine-core.el --- Core setup for document management system -*- lexical-binding: t -*-

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

;; Core setup and configuration for the document management system.

;;; Code:

(require 'seq)
(require 'xref)
(require 'org-id)
(eval-when-compile (require 'subr-x))

;; Load citation-database
(add-to-list 'load-path (expand-file-name "../citation-database" (file-name-directory load-file-name)))
(require 'citation-database)
(citation-database-autosync-mode 1)

;; Associate .orgtex with org-mode
(add-to-list 'auto-mode-alist '("\\.orgtex\\'" . org-mode))

(defgroup doc-engine ()
  "Document and paper management system."
  :group 'files
  :link '(info-link "(doc-engine) Top")
  :link '(url-link :tag "Homepage" "https://scientific-mapping.org"))

(provide 'doc-engine-core)

;;; doc-engine-core.el ends here