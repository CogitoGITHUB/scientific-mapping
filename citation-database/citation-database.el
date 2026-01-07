;;; citation-database.el --- Bibliographic database for scientific citations -*- lexical-binding: t -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; citation-database provides an SQLite backend for managing scientific citations,
;; bibliographic metadata, and citation relationships. It serves as the data layer
;; for the scientific knowledge mapping system, enabling efficient storage, indexing,
;; and querying of academic literature and research notes.
;;
;; The database stores:
;; - Papers and documents with DOIs and identifiers
;; - Citation relationships between documents
;; - Authors, journals, and affiliations
;; - Keywords, concepts, and tags
;; - Reading status and priority information
;; - Impact factors and citation counts

;;; Code:

;; Load submodules
(require 'citation-database-core)
(require 'citation-database-queries)
(require 'citation-database-entities)
(require 'citation-database-autosync)

(provide 'citation-database)

;;; citation-database.el ends here