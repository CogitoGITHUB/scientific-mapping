;;; scientific-visualizer.el --- 3D visualization for scientific knowledge networks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/scientific-visualizer
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (websocket "1.13") (simple-httpd "1.5"))

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

;; scientific-visualizer provides a web-based 3D visualization interface for
;; exploring scientific knowledge networks, inspired by org-roam-ui. It integrates
;; with the citation-database to provide interactive 3D graphs of papers, concepts,
;; and their relationships.
;;
;; Features include:
;; - 3D force-directed graph layouts of citation networks
;; - Concept mapping and visualization
;; - Real-time synchronization with Emacs
;; - Multiple visualization modes (citation, concept, author collaboration)
;; - Interactive filtering and searching
;; - Timeline and evolution views

;;; Code:

(require 'json)
(require 'simple-httpd)
(require 'websocket)
(require 'citeproc)
(eval-when-compile (require 'subr-x))

;; Load citation-database
(add-to-list 'load-path (expand-file-name "../citation-database" (file-name-directory load-file-name)))
(require 'citation-database)

(defgroup scientific-visualizer ()
  "3D visualization for scientific knowledge networks."
  :group 'files
  :link '(info-link "(scientific-visualizer) Top")
  :link '(url-link :tag "Homepage" "https://scientific-mapping.org"))

;;;; Configuration

(defvar scientific-visualizer-root-dir
  (concat (file-name-directory
            (expand-file-name (or
                     load-file-name
                     buffer-file-name)))
           ".")
  "Root directory of the scientific-visualizer project.")

(defvar scientific-visualizer-app-build-dir
  (expand-file-name "./out/" scientific-visualizer-root-dir)
  "Directory containing scientific-visualizer's web build.")

(defvar scientific-visualizer-port
  35901
  "Port to serve the scientific-visualizer interface.")

(defcustom scientific-visualizer-sync-theme t
  "If true, sync your current Emacs theme with `scientific-visualizer'."
  :group 'scientific-visualizer
  :type 'boolean)

(defcustom scientific-visualizer-follow t
  "If true, `scientific-visualizer' will follow you around in the graph."
  :group 'scientific-visualizer
  :type 'boolean)

(defcustom scientific-visualizer-update-on-save t
  "If true, `scientific-visualizer' will send new data when you save a document."
  :group 'scientific-visualizer
  :type 'boolean)

(defcustom scientific-visualizer-open-on-start t
  "Whether to open your default browser when `scientific-visualizer' launches."
  :group 'scientific-visualizer
  :type 'boolean)

(defcustom scientific-visualizer-visualization-mode 'citation
  "Default visualization mode: `citation', `concept', `author', `journal'."
  :group 'scientific-visualizer
  :type '(choice (const :tag "Citation Network" citation)
                   (const :tag "Concept Map" concept)
                   (const :tag "Author Collaboration" author)
                   (const :tag "Journal Landscape" journal)))

;;;; Internal Variables

(defvar scientific-visualizer-ws-socket nil
  "The websocket for scientific-visualizer.")

(defvar scientific-visualizer--current-paper nil
  "Var to keep track of which paper you are looking at.")

(defvar scientific-visualizer-ws-server nil
  "The websocket server for scientific-visualizer.")

;;;; Main Mode

;;;###autoload
(define-minor-mode scientific-visualizer-mode
  "Enable scientific-visualizer.
This serves the web-build and API over HTTP."
  :lighter " SciViz"
  :global t
  :group 'scientific-visualizer
  :init-value nil
  (cond
   (scientific-visualizer-mode
    (setq-local httpd-port scientific-visualizer-port)
    (setq httpd-root scientific-visualizer-app-build-dir)
    (httpd-start)
    (setq scientific-visualizer-ws-server
          (websocket-server
           35903
           :host 'local
           :on-open #'scientific-visualizer--ws-on-open
           :on-message #'scientific-visualizer--ws-on-message
           :on-close #'scientific-visualizer--ws-on-close))
    (when scientific-visualizer-open-on-start (scientific-visualizer-open)))
   (t
    (websocket-server-close scientific-visualizer-ws-server)
    (httpd-stop)
    (remove-hook 'after-save-hook #'scientific-visualizer--on-save)
    (scientific-visualizer-follow-mode -1))))

;;;; WebSocket Handlers

(defun scientific-visualizer--ws-on-open (ws)
  "Open the websocket WS to scientific-visualizer and send initial data."
  (progn
    (setq scientific-visualizer-ws-socket ws)
    (scientific-visualizer--send-variables scientific-visualizer-ws-socket)
    (scientific-visualizer--send-graphdata)
    (when scientific-visualizer-update-on-save
      (add-hook 'after-save-hook #'scientific-visualizer--on-save))
    (message "Connection established with scientific-visualizer")
    (when scientific-visualizer-follow
      (scientific-visualizer-follow-mode 1))))

(defun scientific-visualizer--ws-on-message (_ws frame)
  "Functions to run when the scientific-visualizer server receives a message.
Takes _WS and FRAME as arguments."
  (let* ((msg (json-parse-string
                 (websocket-frame-text frame) :object-type 'alist))
           (command (alist-get 'command msg))
           (data (alist-get 'data msg)))
    (cond ((string= command "open")
           (scientific-visualizer--on-msg-open-paper data))
          ((string= command "add-citation")
           (scientific-visualizer--on-msg-add-citation data))
          ((string= command "set-visualization-mode")
           (scientific-visualizer--on-msg-set-visualization-mode data))
          (t
           (message "Unknown command: %s" command)))))

(defun scientific-visualizer--ws-on-close (_websocket)
  "What to do when _WEBSOCKET to scientific-visualizer is closed."
  (remove-hook 'after-save-hook #'scientific-visualizer--on-save)
  (scientific-visualizer-follow-mode -1)
  (message "Connection with scientific-visualizer closed."))

;;;; Message Handlers

(defun scientific-visualizer--on-msg-open-paper (data)
  "Open a paper when receiving DATA from the websocket."
  (let* ((identifier (alist-get 'identifier data))
          (paper (citation-database-get-paper-by-identifier identifier)))
    (when paper
      (let* ((file (alist-get 'file paper)))
        (find-file file)
        (message "Opened paper: %s" (alist-get 'title paper))))))

(defun scientific-visualizer--on-msg-add-citation (data)
  "Add citation when receiving DATA from the websocket."
  (let* ((source-id (alist-get 'source-id data))
          (target-id (alist-get 'target-id data))
          (citation-type (alist-get 'citation-type data)))
    (citation-database-add-citation source-id target-id citation-type)
    (scientific-visualizer--send-graphdata)))

(defun scientific-visualizer--on-msg-set-visualization-mode (data)
  "Set visualization mode when receiving DATA from the websocket."
  (let ((mode (alist-get 'mode data)))
    (setq scientific-visualizer-visualization-mode mode)
    (scientific-visualizer--send-graphdata)))

;;;; Data Transmission

(defun scientific-visualizer--send-graphdata ()
  "Send graph data to the websocket."
  (let* ((visualization-data
           (pcase scientific-visualizer-visualization-mode
             ('citation (scientific-visualizer--get-citation-network))
             ('concept (scientific-visualizer--get-concept-network))
             ('author (scientific-visualizer--get-author-network))
             ('journal (scientific-visualizer--get-journal-network)))))
    (websocket-send-text scientific-visualizer-ws-socket
                         (json-encode
                          `((type . "graphdata")
                            (data . ,visualization-data))))))

(defun scientific-visualizer--get-citation-network ()
  "Get citation network data for visualization."
  (let* ((network (citation-database-citation-network 3 1000))
          (nodes (plist-get network :nodes))
          (edges (plist-get network :edges)))
    (list
     :nodes nodes
     :edges edges
     :mode 'citation
     :layout 'force-directed)))

(defun scientific-visualizer--get-concept-network ()
  "Get concept network data for visualization."
  (let* ((concepts (citation-database-search-concepts "" 500))
          (nodes (mapcar (lambda (c)
                         (list (elt c 0)          ; id
                               (elt c 1)          ; name
                               (elt c 4)))        ; frequency
                       concepts))
          (edges '())) ; Concept relationships would be added here
    (list
     :nodes nodes
     :edges edges
     :mode 'concept
     :layout 'hierarchical)))

(defun scientific-visualizer--get-author-network ()
  "Get author collaboration network data for visualization."
  (let* ((authors (emacsql citation-database-db
                    [:select id name :from authors]))
          (nodes (mapcar (lambda (a)
                         (list (elt a 0) (elt a 1) 1))
                       authors))
          (edges '())) ; Author collaborations from co-authorships
    (list
     :nodes nodes
     :edges edges
     :mode 'author
     :layout 'force-directed)))

(defun scientific-visualizer--get-journal-network ()
  "Get journal network data for visualization."
  (let* ((journals (emacsql citation-database-db
                      [:select id name impact-factor :from journals]))
          (nodes (mapcar (lambda (j)
                         (list (elt j 0) (elt j 1) (or (elt j 2) 0.0)))
                       journals))
          (edges '())) ; Journal similarity edges
    (list
     :nodes nodes
     :edges edges
     :mode 'journal
     :layout 'circular)))

(defun scientific-visualizer--send-variables (ws)
  "Send miscellaneous scientific-visualizer variables through websocket WS."
  (websocket-send-text ws
                       (json-encode
                        `((type . "variables")
                          (data .
                                (("visualizationMode" .
                                  ,scientific-visualizer-visualization-mode)
                                 ("syncTheme" .
                                  ,scientific-visualizer-sync-theme)
                                 ("followMode" .
                                  ,scientific-visualizer-follow)))))))

;;;; Follow Mode

(defun scientific-visualizer--update-current-paper ()
  "Send the current paper data to the websocket."
  (when (and (websocket-openp scientific-visualizer-ws-socket)
              (buffer-file-name))
    (let* ((file (buffer-file-name))
           (identifier (when file
                         (with-current-buffer (find-file-noselect file)
                           (org-id-get-create)))))
      (unless (string= scientific-visualizer--current-paper identifier)
        (setq scientific-visualizer--current-paper identifier)
        (websocket-send-text scientific-visualizer-ws-socket
                             (json-encode `((type . "command")
                                             (data . ((commandName . "focus")
                                                      (identifier . ,identifier))))))))))

;;;###autoload
(define-minor-mode scientific-visualizer-follow-mode
  "Set whether scientific-visualizer should follow your every move in Emacs."
  :lighter " SciViz-Follow"
  :global t
  :group 'scientific-visualizer
  :init-value nil
  (if scientific-visualizer-follow-mode
      (progn
        (add-hook 'post-command-hook #'scientific-visualizer--update-current-paper)
        (message "scientific-visualizer will now follow you around."))
    (remove-hook 'post-command-hook #'scientific-visualizer--update-current-paper)
    (message "scientific-visualizer will now leave you alone.")))

;;;; Save Handler

(defun scientific-visualizer--on-save ()
  "Send graphdata on saving a scientific document."
  (when (member (file-name-extension (buffer-file-name))
                 '("org" "md" "txt"))
    (scientific-visualizer--send-variables scientific-visualizer-ws-socket)
    (scientific-visualizer--send-graphdata)))

;;;; Interactive Commands

;;;###autoload
(defun scientific-visualizer-open ()
  "Ensure `scientific-visualizer' is running, then open the interface."
  (interactive)
  (unless scientific-visualizer-mode (scientific-visualizer-mode))
  (browse-url (format "http://localhost:%d" scientific-visualizer-port)))

;;;###autoload
(defun scientific-visualizer-paper-zoom (&optional identifier speed padding)
  "Move the view of the graph to current paper or optional IDENTIFIER."
  (interactive)
  (let* ((file (buffer-file-name))
          (paper-id (when file
                      (with-current-buffer (find-file-noselect file)
                        (org-id-get-create)))))
    (if (or identifier paper-id)
        (websocket-send-text scientific-visualizer-ws-socket
                             (json-encode `((type . "command")
                                             (data . ((commandName . "zoom")
                                                      (identifier . ,(or identifier paper-id))
                                                      (speed . ,speed)
                                                      (padding . ,padding))))))
      (message "No paper found."))))

;;;###autoload
(defun scientific-visualizer-set-mode (mode)
  "Set visualization mode to MODE (`citation', `concept', `author', `journal')."
  (interactive
   (list (completing-read "Visualization mode: "
                           '("citation" "concept" "author" "journal"))))
  (setq scientific-visualizer-visualization-mode (intern mode))
  (scientific-visualizer--send-graphdata)
  (message "Visualization mode set to: %s" mode)

(provide 'scientific-visualizer)

;;; scientific-visualizer.el ends here