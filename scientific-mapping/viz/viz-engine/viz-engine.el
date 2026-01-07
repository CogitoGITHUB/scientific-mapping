;;; viz-engine.el --- 3D visualization for scientific knowledge networks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; URL: https://github.com/scientific-mapping/viz-engine
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

;; viz-engine provides a web-based 3D visualization interface for
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

;; Load EAF browser for in-Emacs visualization
(when (require 'eaf-browser nil t)
  (require 'eaf-browser))

(defgroup viz-engine ()
  "3D visualization for scientific knowledge networks."
  :group 'files
  :link '(info-link "(viz-engine) Top")
  :link '(url-link :tag "Homepage" "https://scientific-mapping.org"))

;;;; Configuration

(defvar viz-engine-root-dir
  (concat (file-name-directory
            (expand-file-name (or
                     load-file-name
                     buffer-file-name)))
           ".")
  "Root directory of the viz-engine project.")

(defvar viz-engine-app-build-dir
  (expand-file-name "./out/" viz-engine-root-dir)
  "Directory containing viz-engine's web build.")

(defvar viz-engine-port
  35901
  "Port to serve the viz-engine interface.")

(defcustom viz-engine-sync-theme t
  "If true, sync your current Emacs theme with `viz-engine'."
  :group 'viz-engine
  :type 'boolean)

(defcustom viz-engine-show-relationships t
  "If true, show detailed relationship types in visualization."
  :group 'viz-engine
  :type 'boolean)

(defcustom viz-engine-follow t
  "If true, `viz-engine' will follow you around in the graph."
  :group 'viz-engine
  :type 'boolean)

(defcustom viz-engine-update-on-save t
  "If true, `viz-engine' will send new data when you save a document."
  :group 'viz-engine
  :type 'boolean)

(defcustom viz-engine-open-on-start t
  "Whether to open your default browser when `viz-engine' launches."
  :group 'viz-engine
  :type 'boolean)

(defcustom viz-engine-visualization-mode 'citation
  "Default visualization mode: `citation', `concept', `author', `journal'."
  :group 'viz-engine
  :type '(choice (const :tag "Citation Network" citation)
                   (const :tag "Concept Map" concept)
                   (const :tag "Author Collaboration" author)
                   (const :tag "Journal Landscape" journal)))

;;;; Internal Variables

(defvar viz-engine-ws-socket nil
  "The websocket for viz-engine.")

(defvar viz-engine--current-paper nil
  "Var to keep track of which paper you are looking at.")

(defvar viz-engine-ws-server nil
  "The websocket server for viz-engine.")

;;;; Main Mode

;;;###autoload
(define-minor-mode viz-engine-mode
  "Enable viz-engine.
This serves the web-build and API over HTTP."
  :lighter " SciViz"
  :global t
  :group 'viz-engine
  :init-value nil
  (cond
   (viz-engine-mode
    (setq-local httpd-port viz-engine-port)
    (setq httpd-root viz-engine-app-build-dir)
    (httpd-start)
    (setq viz-engine-ws-server
          (websocket-server
           35903
           :host 'local
           :on-open #'viz-engine--ws-on-open
           :on-message #'viz-engine--ws-on-message
           :on-close #'viz-engine--ws-on-close))
    (when viz-engine-open-on-start (viz-engine-open)))
   (t
    (websocket-server-close viz-engine-ws-server)
    (httpd-stop)
    (remove-hook 'after-save-hook #'viz-engine--on-save)
    (viz-engine-follow-mode -1))))

;;;; WebSocket Handlers

(defun viz-engine--ws-on-open (ws)
  "Open the websocket WS to viz-engine and send initial data."
  (progn
    (setq viz-engine-ws-socket ws)
    (viz-engine--send-variables viz-engine-ws-socket)
    (viz-engine--send-graphdata)
    (when viz-engine-update-on-save
      (add-hook 'after-save-hook #'viz-engine--on-save))
    (message "Connection established with viz-engine")
    (when viz-engine-follow
      (viz-engine-follow-mode 1))))

(defun viz-engine--ws-on-message (_ws frame)
  "Functions to run when the viz-engine server receives a message.
Takes _WS and FRAME as arguments."
  (let* ((msg (json-parse-string
                 (websocket-frame-text frame) :object-type 'alist))
           (command (alist-get 'command msg))
           (data (alist-get 'data msg)))
    (cond ((string= command "open")
           (viz-engine--on-msg-open-paper data))
          ((string= command "add-citation")
           (viz-engine--on-msg-add-citation data))
          ((string= command "set-visualization-mode")
           (viz-engine--on-msg-set-visualization-mode data))
          (t
           (message "Unknown command: %s" command)))))

(defun viz-engine--ws-on-close (_websocket)
  "What to do when _WEBSOCKET to viz-engine is closed."
  (remove-hook 'after-save-hook #'viz-engine--on-save)
  (viz-engine-follow-mode -1)
  (message "Connection with viz-engine closed."))

;;;; Message Handlers

(defun viz-engine--on-msg-open-paper (data)
  "Open a paper when receiving DATA from the websocket."
  (let* ((identifier (alist-get 'identifier data))
          (paper (citation-database-get-paper-by-identifier identifier)))
    (when paper
      (let* ((file (alist-get 'file paper)))
        (find-file file)
        (message "Opened paper: %s" (alist-get 'title paper))))))

(defun viz-engine--on-msg-add-citation (data)
  "Add citation when receiving DATA from the websocket."
  (let* ((source-id (alist-get 'source-id data))
          (target-id (alist-get 'target-id data))
          (citation-type (alist-get 'citation-type data)))
    (citation-database-add-citation source-id target-id citation-type)
    (viz-engine--send-graphdata)))

(defun viz-engine--on-msg-set-visualization-mode (data)
  "Set visualization mode when receiving DATA from the websocket."
  (let ((mode (alist-get 'mode data)))
    (setq viz-engine-visualization-mode mode)
    (viz-engine--send-graphdata)))

;;;; Data Transmission

(defun viz-engine--send-graphdata ()
  "Send graph data to the websocket."
  (let* ((visualization-data
           (pcase viz-engine-visualization-mode
             ('citation (viz-engine--get-citation-network))
             ('concept (viz-engine--get-concept-network))
             ('author (viz-engine--get-author-network))
             ('journal (viz-engine--get-journal-network)))))
    (websocket-send-text viz-engine-ws-socket
                         (json-encode
                          `((type . "graphdata")
                            (data . ,visualization-data))))))

(defun viz-engine--get-citation-network ()
  "Get citation network data for visualization."
  (let* ((network (citation-database-citation-network 3 1000))
          (nodes (plist-get network :nodes))
          (edges (mapcar (lambda (edge)
                          (list (elt edge 0) (elt edge 1) "citation" 1))
                        (plist-get network :edges))))
    (list
     :nodes nodes
     :edges edges
     :mode 'citation
     :layout 'force-directed)))

(defun viz-engine--get-concept-network ()
  "Get concept network data for visualization."
  (let* ((concepts (citation-database-search-concepts "" 500))
          (nodes (mapcar (lambda (c)
                         (list (elt c 0)          ; id
                               (elt c 1)          ; name
                               (elt c 4)))        ; frequency
                       concepts))
          (edges (viz-engine--get-concept-relationships)))
    (list
     :nodes nodes
     :edges edges
     :mode 'concept
     :layout 'hierarchical)))

(defun viz-engine--get-concept-relationships ()
  "Get concept relationships for visualization."
  (let ((relationships '()))
    ;; Get relationships from concept-relationships system
    (when (and (boundp 'concept-relationships-files) concept-relationships-files)
      (dolist (file (concept-relationships-files))
        (with-current-buffer (find-file-noselect file)
          (let ((id (org-id-get)))
            (when id
              ;; Get different relationship types
              (let ((parents (org-entry-get (point-min) concept-relationships-parents-property-name))
                    (children (org-entry-get (point-min) concept-relationships-children-property-name))
                    (siblings (org-entry-get (point-min) concept-relationships-siblings-property-name))
                    (friends (org-entry-get (point-min) concept-relationships-friends-property-name))
                    (evidence (org-entry-get (point-min) concept-relationships-evidence-property-name)))
                (dolist (rel-id (split-string (or parents "") " "))
                  (when (and rel-id (> (length rel-id) 0))
                    (push (list id rel-id "parent" 2) relationships)))
                (dolist (rel-id (split-string (or children "") " "))
                  (when (and rel-id (> (length rel-id) 0))
                    (push (list id rel-id "child" 2) relationships)))
                (dolist (rel-id (split-string (or siblings "") " "))
                  (when (and rel-id (> (length rel-id) 0))
                    (push (list id rel-id "sibling" 1) relationships)))
                (dolist (rel-id (split-string (or friends "") " "))
                  (when (and rel-id (> (length rel-id) 0))
                    (push (list id rel-id "friend" 1) relationships)))
                (dolist (rel-id (split-string (or evidence "") " "))
                  (when (and rel-id (> (length rel-id) 0))
                    (push (list id rel-id "evidence" 3) relationships))))))))
    relationships))

(defun viz-engine--get-author-network ()
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

(defun viz-engine--get-journal-network ()
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

(defun viz-engine--get-modus-theme-colors ()
  "Get current Modus theme colors."
  (when (and (boundp 'modus-themes-mode) modus-themes-mode)
    (let ((bg (face-attribute 'default :background))
          (fg (face-attribute 'default :foreground))
          (accent (face-attribute 'modus-themes-accent :foreground))
          (border (face-attribute 'border :background))
          (highlight (face-attribute 'highlight :background)))
      `((background . ,bg)
        (foreground . ,fg)
        (accent . ,accent)
        (border . ,border)
        (highlight . ,highlight)))))

(defun viz-engine--send-variables (ws)
  "Send miscellaneous viz-engine variables through websocket WS."
  (let ((theme-colors (when viz-engine-sync-theme
                        (viz-engine--get-modus-theme-colors))))
    (websocket-send-text ws
                         (json-encode
                          `((type . "variables")
                            (data .
                                  (("visualizationMode" .
                                    ,viz-engine-visualization-mode)
                                   ("syncTheme" .
                                    ,viz-engine-sync-theme)
                                   ("followMode" .
                                    ,viz-engine-follow)
                                   ("showRelationships" .
                                    ,viz-engine-show-relationships)
                                   ("theme" . ,theme-colors))))))))

;;;; Follow Mode

(defun viz-engine--update-current-paper ()
  "Send the current paper data to the websocket."
  (when (and (websocket-openp viz-engine-ws-socket)
              (buffer-file-name))
    (let* ((file (buffer-file-name))
           (identifier (when file
                         (with-current-buffer (find-file-noselect file)
                           (org-id-get-create)))))
      (unless (string= viz-engine--current-paper identifier)
        (setq viz-engine--current-paper identifier)
        (websocket-send-text viz-engine-ws-socket
                             (json-encode `((type . "command")
                                             (data . ((commandName . "focus")
                                                      (identifier . ,identifier))))))))))

;;;###autoload
(define-minor-mode viz-engine-follow-mode
  "Set whether viz-engine should follow your every move in Emacs."
  :lighter " SciViz-Follow"
  :global t
  :group 'viz-engine
  :init-value nil
  (if viz-engine-follow-mode
      (progn
        (add-hook 'post-command-hook #'viz-engine--update-current-paper)
        (message "viz-engine will now follow you around."))
    (remove-hook 'post-command-hook #'viz-engine--update-current-paper)
    (message "viz-engine will now leave you alone.")))

;;;; Save Handler

(defun viz-engine--on-save ()
  "Send graphdata on saving a scientific document."
  (when (member (file-name-extension (buffer-file-name))
                 '("org" "md" "txt"))
    (viz-engine--send-variables viz-engine-ws-socket)
    (viz-engine--send-graphdata)))

;;;; Interactive Commands

;;;###autoload
(defun viz-engine-open ()
  "Ensure `viz-engine' is running, then open the interface."
  (interactive)
  (unless viz-engine-mode (viz-engine-mode))
  (if (featurep 'eaf-browser)
      ;; Use EAF browser for in-Emacs visualization
      (eaf-open-browser (format "http://localhost:%d" viz-engine-port))
    ;; Fallback to external browser
    (browse-url (format "http://localhost:%d" viz-engine-port))))

;;;###autoload
(defun viz-engine-paper-zoom (&optional identifier speed padding)
  "Move the view of the graph to current paper or optional IDENTIFIER."
  (interactive)
  (let* ((file (buffer-file-name))
          (paper-id (when file
                      (with-current-buffer (find-file-noselect file)
                        (org-id-get-create)))))
    (if (or identifier paper-id)
        (websocket-send-text viz-engine-ws-socket
                             (json-encode `((type . "command")
                                             (data . ((commandName . "zoom")
                                                      (identifier . ,(or identifier paper-id))
                                                      (speed . ,speed)
                                                      (padding . ,padding))))))
      (message "No paper found."))))

;;;###autoload
(defun viz-engine-set-mode (mode)
  "Set visualization mode to MODE (`citation', `concept', `author', `journal')."
  (interactive
   (list (completing-read "Visualization mode: "
                           '("citation" "concept" "author" "journal"))))
  (setq viz-engine-visualization-mode (intern mode))
  (viz-engine--send-graphdata)
  (message "Visualization mode set to: %s" mode)

(provide 'viz-engine)

;;; viz-engine.el ends here