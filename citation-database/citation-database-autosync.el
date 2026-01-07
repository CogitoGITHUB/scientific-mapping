;;; citation-database-autosync.el --- Autosync mode for citation database -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/citation-database
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (emacsql "3.0.0"))

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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Autosync mode for automatic database synchronization.

;;; Code:

(require 'citation-database-core)

;;;; Autosync Mode

(define-minor-mode citation-database-autosync-mode
  "Auto-sync citation database when files are saved."
  :lighter " CitationDB"
  :global t
  (if citation-database-autosync-mode
      (add-hook 'after-save-hook #'citation-database--autosync-handler)
    (remove-hook 'after-save-hook #'citation-database--autosync-handler)))

(defun citation-database--autosync-handler ()
  "Handle autosync on file save."
  (when (and citation-database-autosync
             (buffer-file-name)
             (member (file-name-extension (buffer-file-name))
                     '("org" "md" "txt")))
    (citation-database-index-file (buffer-file-name))))

(provide 'citation-database-autosync)

;;; citation-database-autosync.el ends here