;;; scientific-scientific-dashboard-widgets.el --- Widgets for scientific mapping dashboard  -*- lexical-binding: t -*-

;; Copyright (c) 2024-2026 Scientific Mapping System

;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:

;; Widgets for the scientific-mapping dashboard, including sections for
;; recent documents, citations, concepts, research stats, and more.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'mule-util)
(require 'rect)
(require 'subr-x)

;;
;;; Externals

;; Compiler pacifier
(declare-function all-the-icons-icon-for-dir "ext:all-the-icons.el")
(declare-function all-the-icons-icon-for-file "ext:all-the-icons.el")
(declare-function all-the-icons-fileicon "ext:data-fileicons.el")
(declare-function all-the-icons-octicon "ext:data-octicons.el")
(declare-function nerd-icons-icon-for-dir "ext:nerd-icons.el")
(declare-function nerd-icons-icon-for-file "ext:nerd-icons.el")
(declare-function nerd-icons-sucicon "ext:nerd-icons.el")
(declare-function nerd-icons-octicon "ext:nerd-icons.el")
(declare-function nerd-icons-codicon "ext:nerd-icons.el")
(declare-function bookmark-get-filename "ext:bookmark.el")
(declare-function bookmark-all-names "ext:bookmark.el")
(declare-function calendar-date-compare "ext:calendar.el")
(declare-function projectile-cleanup-known-projects "ext:projectile.el")
(declare-function projectile-load-known-projects "ext:projectile.el")
(declare-function projectile-mode "ext:projectile.el")
;;; project.el in Emacs 26 does not contain this function
(declare-function project-known-project-roots "ext:project.el" nil t)
(declare-function project-forget-zombie-projects "ext:project.el" nil t)
(declare-function org-agenda-format-item "ext:org-agenda.el")
(declare-function org-compile-prefix-format "ext:org-agenda.el")
(declare-function org-entry-get "ext:org.el")
(declare-function org-entry-is-done-p "ext:org.el")
(declare-function org-entry-is-todo-p "ext:org.el")
(declare-function org-get-category "ext:org.el")
(declare-function org-get-deadline-time "ext:org.el")
(declare-function org-get-heading "ext:org.el")
(declare-function org-get-priority "ext:org.el")
(declare-function org-get-scheduled-time "ext:org.el")
(declare-function org-get-tags "ext:org.el")
(declare-function org-get-todo-face "ext:org.el")
(declare-function org-get-todo-state "ext:org.el")
(declare-function org-in-archived-heading-p "ext:org.el")
(declare-function org-link-display-format "ext:org.el")
(declare-function org-map-entries "ext:org.el")
(declare-function org-outline-level "ext:org.el")
(declare-function org-release-buffers "ext:org.el")
(declare-function org-time-string-to-time "ext:org.el")
(declare-function org-today "ext:org.el")
(declare-function recentf-cleanup "ext:recentf.el")
(defvar org-level-faces)
(defvar org-agenda-new-buffers)
(defvar org-agenda-prefix-format)
(defvar org-agenda-todo-keyword-format)
(defvar org-todo-keywords-1)
(defvar all-the-icons-dir-icon-alist)
(defvar package-activated-list)
(defvar elpaca-after-init-time)
(declare-function string-pixel-width "subr-x.el")   ; TODO: remove this after 29.1
(declare-function shr-string-pixel-width "shr.el")  ; TODO: remove this after 29.1

(defvar truncate-string-ellipsis)
(declare-function truncate-string-ellipsis "mule-util.el")  ; TODO: remove this after 28.1
(defvar recentf-list nil)
(defvar scientific-dashboard-buffer-name)

;;
;;; Customization

(defcustom scientific-dashboard-page-separator "\n"
  "Separator to use between the different pages."
  :type '(choice
          (const :tag "Default" "\n")
          (const :tag "Use Page indicator (requires page-break-lines)"
                 "\n\f\n")
          (string :tag "Use Custom String"))
  :group 'dashboard)

(defcustom scientific-dashboard-image-banner-max-height 0
  "Maximum height of banner image.

This setting applies only if Emacs supports image transforms or
compiled with Imagemagick support.  When value is non-zero the image
banner will be resized to the specified height in pixels, with aspect
ratio preserved."
  :type 'integer
  :group 'dashboard)

(defcustom scientific-dashboard-image-banner-max-width 0
  "Maximum width of banner image.

This setting applies if Emacs supports image transforms or compiled
with Imagemagick support.  When value is non-zero the image banner
will be resized to the specified width in pixels, with aspect ratio
preserved."
  :type 'integer
  :group 'dashboard)

(defcustom scientific-dashboard-image-extra-props nil
  "Additional image attributes to assign to the image.
This could be useful for displaying images with transparency,
for example, by setting the `:mask' property to `heuristic'.
See `create-image' and Info node `(elisp)Image Descriptors'."
  :type 'plist
  :group 'dashboard)

(defcustom scientific-dashboard-set-heading-icons nil
  "When non nil, heading sections will have icons."
  :type 'boolean
  :group 'dashboard)

(defcustom scientific-dashboard-set-file-icons nil
  "When non nil, file lists will have icons."
  :type 'boolean
  :group 'dashboard)

(defcustom scientific-dashboard-set-navigator nil
  "When non nil, a navigator will be displayed under the banner."
  :type 'boolean
  :group 'dashboard)
(make-obsolete-variable 'scientific-dashboard-set-navigator
                        'scientific-dashboard-startupify-list "1.9.0")

(defcustom scientific-dashboard-set-footer t
  "When non nil, a footer will be displayed at the bottom."
  :type 'boolean
  :group 'dashboard)
(make-obsolete-variable 'scientific-dashboard-set-footer
                        'scientific-dashboard-startupify-list "1.9.0")

(defcustom scientific-dashboard-footer-messages
  '("The one true editor, Emacs!"
    "Who the hell uses VIM anyway? Go Evil!"
    "Free as free speech, free as free Beer"
    "Happy coding!"
    "Vi Vi Vi, the editor of the beast"
    "Welcome to the church of Emacs"
    "While any text editor can save your files, only Emacs can save your soul"
    "I showed you my source code, pls respond")
  "A list of messages, one of which dashboard chooses to display."
  :type '(repeat string)
  :group 'dashboard)

(defcustom scientific-dashboard-icon-type (and (or scientific-dashboard-set-heading-icons
                                        scientific-dashboard-set-file-icons)
                                    (or (require 'nerd-icons nil t)
                                        (require 'all-the-icons nil t)))
  "Icon type used for dashboard.
The value can be one of: `all-the-icons', `nerd-icons'."
  :type 'symbol
  :group 'dashboard
  :set
  (lambda (k v)
    (pcase v
      ('all-the-icons
       (unless (require 'all-the-icons nil t)
         (setq v nil)))
      ('nerd-icons
       (unless (require 'nerd-icons nil t)
         (setq v nil))))
    (set k v)))

(defcustom scientific-dashboard-heading-icons
  (pcase scientific-dashboard-icon-type
    ('all-the-icons '((recents   . "history")
                      (bookmarks . "bookmark")
                      (agenda    . "calendar")
                      (projects  . "rocket")
                      (registers . "database")))
    ('nerd-icons '((recents   . "nf-oct-history")
                   (bookmarks . "nf-oct-bookmark")
                   (agenda    . "nf-oct-calendar")
                   (projects  . "nf-oct-rocket")
                   (registers . "nf-oct-database"))))
  "Association list for the icons of the heading sections.
Will be of the form `(SECTION . ICON)`, where SECTION could be any dashboard
section, for example: `recents' `bookmarks' `projects' `agenda' `registers'.

ICON could be the name of the icon belonging to `octicon' family
or (ICON-FUNCTION ICON-NAME), for example: \"nf-oct-file\" using
nerd-icons or (all-the-icons-faicon \"newspaper-o\") using all-the-icons."
  :type  '(alist :key-type symbol
                 :value-type (choice string (cons function string)))
  :group 'dashboard)

(defcustom scientific-dashboard-heading-icon-height 1.2
  "The height of the heading icon."
  :type 'float
  :group 'dashboard)

(defcustom scientific-dashboard-heading-icon-v-adjust 0.0
  "The v-adjust of the heading icon."
  :type 'float
  :group 'dashboard)

(defcustom scientific-dashboard-icon-file-height 1.0
  "The height of the file icons."
  :type 'float
  :group 'dashboard)

(defcustom scientific-dashboard-icon-file-v-adjust -0.05
  "The v-adjust of the file icons."
  :type 'float
  :group 'dashboard)

(defcustom scientific-dashboard-agenda-item-icon
  (pcase scientific-dashboard-icon-type
    ('all-the-icons (all-the-icons-octicon "primitive-dot" :height 1.0 :v-adjust 0.01))
    ('nerd-icons (nerd-icons-octicon "nf-oct-dot_fill" :height 1.0 :v-adjust 0.01)))
  "Agenda item icon."
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-agenda-action 'scientific-dashboard-agenda--visit-file-other-window
  "Function to call when dashboard make an action over agenda item."
  :type 'function
  :group 'dashboard)

(defcustom scientific-dashboard-remote-path-icon
  (pcase scientific-dashboard-icon-type
    ('all-the-icons (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
    ('nerd-icons (nerd-icons-codicon "nf-cod-radio_tower" :height 1.0 :v-adjust 0.01)))
  "Remote path icon."
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-show-shortcuts t
  "Whether to show shortcut keys for each section."
  :type 'boolean
  :group 'dashboard)

(defconst scientific-dashboard-banners-directory
  (concat (file-name-directory (locate-library "dashboard")) "banners/")
  "Default banner directory.")

(defconst scientific-dashboard-banner-official-png
  (concat scientific-dashboard-banners-directory "emacs.png")
  "Emacs banner image.")

(defconst scientific-dashboard-banner-logo-png
  (concat scientific-dashboard-banners-directory "logo.png")
  "Emacs banner image.")

(defcustom scientific-dashboard-banner-logo-title "Welcome to Emacs!"
  "Specify the startup banner."
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-banner-ascii "EMACS"
  "String to be shown in place of the startup banner
if `scientific-dashboard-startup-banner' is set to `ascii'."
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-navigator-buttons nil
  "Specify the navigator buttons.
The format is: `icon title help action face prefix suffix`.

Example:
`((\"☆\" \"Star\" \"Show stars\" (lambda (&rest _)
                                    (show-stars)) warning \"[\" \"]\"))"
  :type '(repeat (repeat (list string
                               string
                               string
                               function
                               (choice face
                                       (repeat :tag "Anonymous face" sexp))
                               (choice string
                                       (const nil))
                               (choice string
                                       (const nil)))))
  :group 'dashboard)

(defcustom scientific-dashboard-init-info #'scientific-dashboard-init--info
  "Custom function that must return a string to place instead of init-info."
  :type 'function
  :group 'dashboard)

(defun scientific-dashboard-init--time ()
  "Return Emacs starting time in string including seconds ending."
  (if (fboundp 'elpaca--queued)
      (format "%s seconds"
              (float-time (time-subtract elpaca-after-init-time
                                         before-init-time)))
    (emacs-init-time)))

(defun scientific-dashboard-init--packages-count ()
  "Get the intalled package count depending on package manager.
Supported package managers are: package.el, straight.el and elpaca.el."
  (let* ((package-count (if (bound-and-true-p package-alist)
                            (length package-activated-list)
                          0))
         (straight-count (if (boundp 'straight--profile-cache)
                             (hash-table-count straight--profile-cache)
                           0))
         (elpaca-count (if (fboundp 'elpaca--queued)
                           (length (elpaca--queued))
                         0)))
    (+ package-count straight-count elpaca-count)))


(defun scientific-dashboard-init--info ()
  "Format init message.
Use `scientific-dashboard-init--time' and `scientific-dashboard-init--package-count' to generate
init message."
  (let ((init-time (scientific-dashboard-init--time))
        (packages-count (scientific-dashboard-init--packages-count)))
    (if (zerop packages-count)
        (format "Emacs started in %s" init-time)
      (format "%d packages installed. Emacs started in %s."
              packages-count init-time))))

(defcustom scientific-dashboard-display-icons-p #'display-graphic-p
  "Predicate to determine whether dashboard should show icons.
Can be nil to not show icons and any truthy value to show them.  When set to a
function the result of the function will be interpreted as the predicate value."
  :type '(choice (function :tag "Predicate function")
                 (boolean :tag "Predicate value"))
  :group 'dashboard)

(defun scientific-dashboard-replace-displayable (str &optional rep)
  "Replace non-displayable character from STR.

Optional argument REP is the replacement string of non-displayable character."
  (if (stringp str)
      (let ((rep (or rep ""))
            (results (list)))
        (dolist (string (split-string str ""))
          (let* ((char (string-to-char string))
                 (string (if (char-displayable-p char)
                             string
                           rep)))
            (push string results)))
        (string-join (reverse results)))
    ""))

(defun scientific-dashboard-display-icons-p ()
  "Assert whether to show icons based on the `scientific-dashboard-display-icons-p' variable."
  (if (functionp scientific-dashboard-display-icons-p)
      (funcall scientific-dashboard-display-icons-p)
    scientific-dashboard-display-icons-p))

(defun scientific-dashboard-icon-for-dir (dir &rest args)
  "Get the formatted icon for DIR.
ARGS should be a plist containing `:height', `:v-adjust',
or `:face' properties."
  (scientific-dashboard-replace-displayable
   (pcase scientific-dashboard-icon-type
     ('all-the-icons (apply #'all-the-icons-icon-for-dir dir args))
     ('nerd-icons (apply #'nerd-icons-icon-for-dir dir args)))))

(defun scientific-dashboard-icon-for-file (file &rest args)
  "Get the formatted icon for FILE.
ARGS should be a plist containing `:height', `:v-adjust', or `:face' properties."
  (scientific-dashboard-replace-displayable
   (pcase scientific-dashboard-icon-type
     ('all-the-icons (apply #'all-the-icons-icon-for-file file args))
     ('nerd-icons (apply #'nerd-icons-icon-for-file file args)))))

(defun scientific-dashboard-octicon (name &rest args)
  "Get the formatted octicon by NAME.
ARGS should be a plist containing `:height', `:v-adjust', or `:face' properties."
  (scientific-dashboard-replace-displayable
   (pcase scientific-dashboard-icon-type
     ('all-the-icons (apply #'all-the-icons-octicon name args))
     ('nerd-icons (apply #'nerd-icons-octicon name args)))))

(defcustom scientific-dashboard-footer-icon
  (if (scientific-dashboard-display-icons-p)
      (pcase scientific-dashboard-icon-type
        ('all-the-icons
         (all-the-icons-fileicon "emacs"
                                 :height 1.1
                                 :v-adjust -0.05
                                 :face 'scientific-dashboard-footer-icon-face))
        ('nerd-icons
         (nerd-icons-sucicon "nf-custom-emacs"
                             :height 1.1
                             :v-adjust -0.05
                             :face 'scientific-dashboard-footer-icon-face)))
    (propertize ">" 'face 'scientific-dashboard-footer-icon-face))
  "Footer's icon.
It can be a string or a string list for display random icons."
  :type '(choice string
                 (repeat string))
  :group 'dashboard)

(defcustom scientific-dashboard-heading-shorcut-format " (%s)"
  "String for display key used in headings."
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-startup-banner 'official
  "Specify the banner type to use.
Value can be
 - \\='official  displays the official Emacs logo.
 - \\='logo  displays an alternative Emacs logo.
 - an integer which displays one of the text banners.
 - a string that specifies the path of an custom banner
   supported files types are gif/image/text/xbm.
 - a cons of 2 strings which specifies the path of an image to use
   and other path of a text file to use if image isn't supported.
 - a list that can display an random banner, supported values are:
   string (filepath), \\='official, \\='logo and integers."
  :type '(choice (const   :tag "official"  official)
                 (const   :tag "logo"      logo)
                 (const   :tag "ascii"     ascii)
                 (integer :tag "index of a text banner")
                 (string  :tag "path to an image or text banner")
                 (cons    :tag "image and text banner"
                          (string :tag "image banner path")
                          (string :tag "text banner path"))
                 (repeat :tag "random banners"
                         (choice (string  :tag "a path to an image or text banner")
                                 (const   :tag "official" official)
                                 (const   :tag "logo"     logo)
                                 (const   :tag "ascii"    ascii)
                                 (integer :tag "index of a text banner"))))
  :group 'dashboard)

(defcustom scientific-dashboard-item-generators
  '((recent-documents   . scientific-dashboard-insert-recent-documents)
    (citation-stats     . scientific-dashboard-insert-citation-stats)
    (concept-relationships . scientific-dashboard-insert-concept-relationships)
    (recents            . scientific-dashboard-insert-recents)
    (bookmarks          . scientific-dashboard-insert-bookmarks)
    (projects           . scientific-dashboard-insert-projects)
    (agenda             . scientific-dashboard-insert-agenda)
    (registers          . scientific-dashboard-insert-registers))
  "Association list of items to how to generate in the startup buffer.
Will be of the form `(list-type . list-function)'.
Possible values for list-type are: `recent-documents', `citation-stats', `concept-relationships', `recents', `bookmarks', `projects',
`agenda' ,`registers'."
  :type  '(alist :key-type symbol :value-type function)
  :group 'scientific-dashboard)

(defcustom scientific-dashboard-projects-backend 'project-el
  "The package that supplies the list of recent projects.
With the value `projectile', the projects widget uses the package
projectile (available in MELPA).  With the value `project-el',
the widget uses the package project (available in GNU ELPA).

To activate the projects widget, add e.g. `(projects . 10)' to
`scientific-dashboard-items' after making sure the necessary package is
installed."
  :type '(choice (const :tag "Use projectile" projectile)
                 (const :tag "Use project.el" project-el))
  :group 'dashboard)

(defcustom scientific-dashboard-remove-missing-entry nil
  "If non-nil, try to remove missing entries."
  :type 'boolean
  :group 'dashboard)

(defcustom scientific-dashboard-items
  '((recent-documents   . 5)
    (citation-stats     . 5)
    (concept-relationships . 5)
    (recents            . 5)
    (bookmarks          . 5)
    (agenda             . 5))
  "Association list of items to show in the startup buffer.
Will be of the form `(list-type . list-size)'.
If nil it is disabled.  Possible values for list-type are:
`recent-documents' `citation-stats' `concept-relationships' `recents' `bookmarks' `projects' `agenda' `registers'."
  :type '(repeat (choice
                  symbol
                  (cons symbol integer)))
  :group 'scientific-dashboard)

(defcustom scientific-dashboard-item-shortcuts
  '((recent-documents   . "d")
    (citation-stats     . "c")
    (concept-relationships . "o")
    (recents            . "r")
    (bookmarks          . "m")
    (projects           . "p")
    (agenda             . "a")
    (registers          . "e"))
  "Association list of items and their corresponding shortcuts.
Will be of the form `(list-type . keys)' as understood by `(kbd keys)'.
If nil, shortcuts are disabled.  If an entry's value is nil, that item's
shortcut is disabled.  See `scientific-dashboard-items' for possible values of list-type.'"
  :type '(alist :key-type symbol :value-type string)
  :group 'scientific-dashboard)

(defcustom scientific-dashboard-item-names nil
  "Association list of item heading names.
When an item is nil or not present, the default name is used.
Will be of the form `(default-name . new-name)'."
  :type '(alist :key-type string :value-type string)
  :options '("Recent Files:"
             "Bookmarks:"
             "Agenda for today:"
             "Agenda for the coming week:"
             "Registers:"
             "Projects:")
  :group 'dashboard)

(defcustom scientific-dashboard-items-default-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded."
  :type  'integer
  :group 'dashboard)

(defcustom scientific-dashboard-path-style nil
  "Style to display path."
  :type '(choice
          (const :tag "No specify" nil)
          (const :tag "Truncate the beginning part of the path" truncate-beginning)
          (const :tag "Truncate the middle part of the path" truncate-middle)
          (const :tag "Truncate the end part of the path" truncate-end))
  :group 'dashboard)

(defcustom scientific-dashboard-path-max-length 70
  "Maximum length for path to display."
  :type 'integer
  :group 'dashboard)

;;
;;; Faces

(defface scientific-dashboard-text-banner
  '((t (:inherit font-lock-keyword-face)))
  "Face used for text banners."
  :group 'dashboard)

(defface scientific-dashboard-banner-logo-title
  '((t :inherit default))
  "Face used for the banner title."
  :group 'dashboard)

(defface scientific-dashboard-navigator
  '((t (:inherit font-lock-keyword-face)))
  "Face used for the navigator."
  :group 'dashboard)

(defface scientific-dashboard-heading
  '((t (:inherit font-lock-keyword-face)))
  "Face used for widget headings."
  :group 'dashboard)

(defface scientific-dashboard-items-face
  '((t (:inherit widget-button)))
  "Face used for items."
  :group 'dashboard)

(defface scientific-dashboard-no-items-face
  '((t (:inherit widget-button)))
  "Face used for no items."
  :group 'dashboard)

(defface scientific-dashboard-footer-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for footer text."
  :group 'dashboard)

(defface scientific-dashboard-footer-icon-face
  '((t (:inherit scientific-dashboard-footer-face)))
  "Face used for icon in footer."
  :group 'dashboard)

(define-obsolete-face-alias
 'scientific-dashboard-text-banner-face 'scientific-dashboard-text-banner "1.2.6")
(define-obsolete-face-alias
 'scientific-dashboard-banner-logo-title-face 'scientific-dashboard-banner-logo-title "1.2.6")
(define-obsolete-face-alias
 'scientific-dashboard-heading-face 'scientific-dashboard-heading "1.2.6")

;;
;;; Util

(defmacro scientific-dashboard-mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defun scientific-dashboard-funcall-fboundp (fnc &rest args)
  "Call FNC with ARGS if exists."
  (when (fboundp fnc) (if args (funcall fnc args) (funcall fnc))))

;; TODO: Use function `string-pixel-width' after 29.1
(defun scientific-dashboard-string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun scientific-dashboard-str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (scientific-dashboard-string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

;;
;;; Widget helpers

(defun scientific-dashboard-subseq (seq end)
  "Return the subsequence of SEQ from 0 to END."
  (let ((len (length seq))) (butlast seq (- len (min len end)))))

(defun scientific-dashboard-get-shortcut-name (item)
  "Get the shortcut name to be used for ITEM."
  (let ((elem (rassoc item scientific-dashboard-item-shortcuts)))
    (and elem (car elem))))

(defun scientific-dashboard-get-shortcut (item)
  "Get the shortcut to be used for ITEM."
  (let ((elem (assq item scientific-dashboard-item-shortcuts)))
    (and elem (cdr elem))))

(defmacro scientific-dashboard-insert-shortcut (shortcut-id
                                     shortcut-char
                                     search-label
                                     &optional no-next-line)
  "Insert a shortcut SHORTCUT-CHAR for a given SEARCH-LABEL.

SHORTCUT-ID is the section identifier.

Optionally, provide NO-NEXT-LINE to move the cursor forward a line."
  (let* (;; Ensure punctuation and upper case in search string is not
         ;; used to construct the `defun'
         (name (downcase (replace-regexp-in-string "[[:punct:]]+" "" (format "%s" search-label))))
         ;; remove symbol quote
         (sym (intern (replace-regexp-in-string "'" "" (format "scientific-dashboard-jump-to-%s" shortcut-id)))))
    `(progn
       (eval-when-compile (defvar scientific-dashboard-mode-map))
       (defun ,sym nil
         ,(concat "Jump to " name ".
This code is dynamically generated in `scientific-dashboard-insert-shortcut'.")
         (interactive)
         (unless (search-forward ,search-label (point-max) t)
           (search-backward ,search-label (point-min) t))
         ,@(unless no-next-line '((forward-line 1)))
         (back-to-indentation))
       (eval-after-load 'dashboard
         (define-key scientific-dashboard-mode-map ,shortcut-char ',sym)))))

(defun scientific-dashboard-append (msg &optional _messagebuf)
  "Append MSG to dashboard buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create scientific-dashboard-buffer-name)
    (goto-char (point-max))
    (let ((inhibit-read-only t)) (insert msg))))

(defun scientific-dashboard-modify-heading-icons (alist)
  "Append ALIST items to `scientific-dashboard-heading-icons' to modify icons."
  (dolist (icon alist)
    (add-to-list 'scientific-dashboard-heading-icons icon)))

(defun scientific-dashboard-insert-page-break ()
  "Insert a page break line in dashboard buffer."
  (scientific-dashboard-append scientific-dashboard-page-separator))

(defun scientific-dashboard-insert-newline (&optional times)
  "When called without an argument, insert a newline.
When called with TIMES return a function that insert TIMES number of newlines."
  (if times
      (lambda ()
        (insert (make-string times (string-to-char "\n") t)))
    (insert "\n")))

(defun scientific-dashboard-insert-heading (heading &optional shortcut icon)
  "Insert a widget HEADING in dashboard buffer, adding SHORTCUT, ICON if provided."
  (when (and (scientific-dashboard-display-icons-p) scientific-dashboard-set-heading-icons icon)
    (insert icon " "))

  (insert (propertize heading 'face 'scientific-dashboard-heading))

  ;; Turn the inserted heading into an overlay, so that we may freely change
  ;; its name without breaking any of the functions that expect the default name.
  ;; If there isn't a suitable entry in `scientific-dashboard-item-names',
  ;; we fallback to using HEADING.  In that case we still want it to be an
  ;; overlay to maintain consistent behavior (such as the point movement)
  ;; between modified and default headings.
  (let ((ov (make-overlay (- (point) (length heading)) (point) nil t)))
    (overlay-put ov 'display (or (cdr (assoc heading scientific-dashboard-item-names)) heading))
    (overlay-put ov 'face 'scientific-dashboard-heading))
  (when shortcut (insert (format scientific-dashboard-heading-shorcut-format shortcut))))

(defun scientific-dashboard--find-max-width (start end)
  "Return the max width within the region START and END."
  (save-excursion
    (goto-char start)
    (let ((width 0))
      (while (< (point) end)
        (let* ((line-str (buffer-substring (line-beginning-position) (line-end-position)))
               (line-length (scientific-dashboard-str-len line-str)))
          (setq width (max width line-length)))
        (forward-line 1))
      width)))

(defun scientific-dashboard-center-text (start end)
  "Center the text between START and END."
  (let* ((width (scientific-dashboard--find-max-width start end))
         (prefix (propertize " " 'display `(space . (:align-to (- center ,(/ (float width) 2)))))))
    (add-text-properties start end `(line-prefix ,prefix indent-prefix ,prefix))))

(defun scientific-dashboard-insert-center (&rest strings)
  "Insert STRINGS in the center of the buffer."
  (let ((start (point)))
    (apply #'insert strings)
    (scientific-dashboard-center-text start (point))))

;;
;;; Banner

(defun scientific-dashboard-get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat scientific-dashboard-banners-directory (format "%d.txt" index)))

(defun scientific-dashboard--image-supported-p (img)
  "Return non-nil if IMG exists and is a supported image type."
  ;; In Emacs 29.1 we could use `image-supported-file-p'. However:
  ;; - We need to support Emacs 26.
  ;; - That function will only look at filenames, this one will inspect the file data itself.
  (and (file-exists-p img) (ignore-errors (image-type-available-p (image-type img)))))

(defun scientific-dashboard-choose-banner (banner)
  "Return a plist specifying the chosen banner based on BANNER."
  (pcase banner
    ('official
     (append (when (image-type-available-p 'png)
               (list :image scientific-dashboard-banner-official-png))
             (list :text (scientific-dashboard-get-banner-path 1))))
    ('logo
     (append (when (image-type-available-p 'png)
               (list :image scientific-dashboard-banner-logo-png))
             (list :text (scientific-dashboard-get-banner-path 1))))
    ('ascii
     (append (list :text scientific-dashboard-banner-ascii)))
    ((pred integerp)
     (list :text (scientific-dashboard-get-banner-path banner)))
    ((pred stringp)
     (pcase banner
       ((pred (lambda (f) (not (file-exists-p f))))
        (message "could not find banner %s, use default instead" banner)
        (list :text (scientific-dashboard-get-banner-path 1)))
       ((pred (string-suffix-p ".txt"))
        (list :text (if (file-exists-p banner)
                        banner
                      (message "could not find banner %s, use default instead" banner)
                      (scientific-dashboard-get-banner-path 1))))
       ((pred scientific-dashboard--image-supported-p)
        (list :image banner
              :text (scientific-dashboard-get-banner-path 1)))
       (_
        (message "unsupported file type %s" (file-name-nondirectory banner))
        (list :text (scientific-dashboard-get-banner-path 1)))))
    ((and
      (pred listp)
      (pred (lambda (c)
              (and (not (proper-list-p c))
                   (not (null c)))))
      `(,img . ,txt))
     (list :image (if (scientific-dashboard--image-supported-p img)
                      img
                    (message "could not find banner %s, use default instead" img)
                    scientific-dashboard-banner-official-png)
           :text (if (and (file-exists-p txt) (string-suffix-p ".txt" txt))
                     txt
                   (message "could not find banner %s, use default instead" txt)
                   (scientific-dashboard-get-banner-path 1))))
    ((and
      (pred proper-list-p)
      (pred (lambda (l) (not (null l)))))

     (let* ((max (length banner))
            (choose (nth (random max) banner)))
       (scientific-dashboard-choose-banner choose)))
    (_
     (user-error "Unsupported banner type: `%s'" banner)
     nil)))

(defun scientific-dashboard--image-animated-p (image-path)
  "Return if image is a gif or webp.
String -> bool.
Argument IMAGE-PATH path to the image."
  (memq (image-type image-path) '(gif webp)))

(defun scientific-dashboard--type-is-xbm-p (image-path)
  "Return if image is a xbm.
String -> bool.
Argument IMAGE-PATH path to the image."
  (eq 'xbm (image-type image-path)))

(defun scientific-dashboard-insert-banner ()
  "Insert the banner at the top of the dashboard."
  (goto-char (point-max))
  (when-let* ((banner (scientific-dashboard-choose-banner scientific-dashboard-startup-banner)))
    (insert "\n")
    (when (display-graphic-p) (insert "\n"))
    (let ((start (point))
          buffer-read-only
          text-width
          image-spec)
      ;; If specified, insert a text banner.
      (when-let* ((txt (plist-get banner :text)))
        (save-excursion
          (if (file-exists-p txt)
              (insert-file-contents txt)
            (insert txt)))
        (put-text-property start (point-max) 'face 'scientific-dashboard-text-banner)
        (setq text-width (scientific-dashboard--find-max-width start (point-max)))
        (goto-char (point-max)))
      ;; If specified, insert an image banner. When displayed in a graphical frame, this will
      ;; replace the text banner.
      (when-let* ((img (plist-get banner :image)))
        (let ((img-props
               (append (when (> scientific-dashboard-image-banner-max-width 0)
                         (list :max-width scientific-dashboard-image-banner-max-width))
                       (when (> scientific-dashboard-image-banner-max-height 0)
                         (list :max-height scientific-dashboard-image-banner-max-height))
                       scientific-dashboard-image-extra-props)))
          (setq image-spec
                (cond ((scientific-dashboard--image-animated-p img)
                       (create-image img))
                      ((scientific-dashboard--type-is-xbm-p img)
                       (create-image img))
                      ((image-type-available-p 'imagemagick)
                       (apply 'create-image img 'imagemagick nil img-props))
                      (t
                       (apply 'create-image img nil nil
                              (when (and (fboundp 'image-transforms-p)
                                         (memq 'scale (funcall 'image-transforms-p)))
                                img-props))))))
        (add-text-properties start (point) `(display ,image-spec))
        (when (ignore-errors (image-multi-frame-p image-spec)) (image-animate image-spec 0 t)))

      ;; Finally, center the banner (if any).
      (when-let* ((text-align-spec `(space . (:align-to (- center ,(/ text-width 2)))))
                  (image-align-spec `(space . (:align-to (- center (0.5 . ,image-spec)))))
                  (prop
                   (cond
                    ;; Both an image & text banner.
                    ((and image-spec text-width)
                     ;; The quoting is intentional. This is a conditional display spec that will
                     ;; align the banner at redisplay time.
                     `((when (display-graphic-p) . ,image-align-spec)
                       (when (not (display-graphic-p)) . ,text-align-spec)))
                    ;; One or the other.
                    (text-width text-align-spec)
                    (image-spec image-align-spec)
                    ;; No banner.
                    (t nil)))
                  (prefix (propertize " " 'display prop)))
        (add-text-properties start (point) `(line-prefix ,prefix wrap-prefix ,prefix)))
      (insert "\n")
      (add-text-properties start (point) '(cursor-intangible t inhibit-isearch t)))))

(defun scientific-dashboard-insert-banner-title ()
  "Insert `scientific-dashboard-banner-logo-title' if it's non-nil."
  (when scientific-dashboard-banner-logo-title
    (scientific-dashboard-insert-center (propertize scientific-dashboard-banner-logo-title 'face 'scientific-dashboard-banner-logo-title))
    (insert "\n")))

;;
;;; Initialize info
(defun scientific-dashboard-insert-init-info ()
  "Insert init info."
  (let ((init-info (cond ((stringp scientific-dashboard-init-info)
                          scientific-dashboard-init-info)
                         ((functionp scientific-dashboard-init-info)
                          (funcall scientific-dashboard-init-info))
                         (t
                          (user-error "Unknown init info type (%s): %s"
                                      (type-of scientific-dashboard-init-info) scientific-dashboard-init-info)))))
    (scientific-dashboard-insert-center
     (propertize init-info 'face 'font-lock-comment-face))))

(defun scientific-dashboard-insert-navigator ()
  "Insert Navigator of the dashboard."
  (when scientific-dashboard-navigator-buttons
    (dolist (line scientific-dashboard-navigator-buttons)
      (dolist (btn line)
        (let* ((icon (car btn))
               (title (cadr btn))
               (help (or (cadr (cdr btn)) ""))
               (action (or (cadr (cddr btn)) #'ignore))
               (face (or (cadr (cddr (cdr btn))) 'scientific-dashboard-navigator))
               (prefix (or (cadr (cddr (cddr btn))) (propertize "[" 'face face)))
               (suffix (or (cadr (cddr (cddr (cdr btn)))) (propertize "]" 'face face))))
          (widget-create 'item
                         :tag (concat
                               (when icon
                                 (propertize icon 'face
                                             (let ((prop-face (get-text-property 0 'face icon)))
                                               (if prop-face
                                                   `(:inherit ,prop-face :inherit ,face)
                                                 `(:inherit ,face)))))
                               (when (and icon title
                                          (not (string-equal icon ""))
                                          (not (string-equal title "")))
                                 (propertize " " 'face `(:inherit (variable-pitch
                                                                   ,face))))
                               (when title (propertize title 'face face)))
                         :help-echo help
                         :action action
                         :button-face 'scientific-dashboard-items-face
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]")
          (insert " ")))
      (scientific-dashboard-center-text (line-beginning-position) (line-end-position))
      (insert "\n"))))

(defmacro scientific-dashboard-insert-section (section-name list list-size shortcut-id shortcut-char action &rest widget-params)
  "Add a section with SECTION-NAME and LIST of LIST-SIZE items to the dashboard.

SHORTCUT-ID is the section identifier.
SHORTCUT-CHAR is the keyboard shortcut used to access the section.
ACTION is theaction taken when the user activates the widget button.
WIDGET-PARAMS are passed to the \"widget-create\" function."
  `(progn
     (scientific-dashboard-insert-heading ,section-name
                               (when (and ,list
                                          ,shortcut-char
                                          scientific-dashboard-show-shortcuts)
                                 ,shortcut-char)
                               (scientific-dashboard-heading-icon ,shortcut-id))
     (if ,list
         (when (and (scientific-dashboard-insert-section-list
                     ,section-name
                     (scientific-dashboard-subseq ,list ,list-size)
                     ,action
                     ,@widget-params)
                    ,shortcut-id ,shortcut-char)
           (scientific-dashboard-insert-shortcut ,shortcut-id ,shortcut-char ,section-name))
       (insert (propertize "\n    --- No items ---" 'face 'scientific-dashboard-no-items-face)))))

(defun scientific-dashboard-heading-icon (section)
  "Get the icon for SECTION from `scientific-dashboard-heading-icons'.
Return a space if icon is not found."
  (let ((args (list :height   scientific-dashboard-heading-icon-height
                    :v-adjust scientific-dashboard-heading-icon-v-adjust
                    :face     'scientific-dashboard-heading))
        (icon (assoc section scientific-dashboard-heading-icons)))
    (if icon (cond
              ((stringp (cdr icon)) (apply #'scientific-dashboard-octicon (cdr icon) args))
              ((listp (cdr icon)) (apply (cadr icon) (caddr icon) args))
              (t (error "Bad value %s in `scientific-dashboard-heading-icons'" icon)))
      "  ")))

;;
;;; Section list

(defmacro scientific-dashboard-insert-section-list (section-name list action &rest rest)
  "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST
to widget creation."
  `(when (car ,list)
     (mapc
      (lambda (el)
        (let ((tag ,@rest))
          (insert "\n")
          (insert (spaces-string (or standard-indent tab-width 4)))

          (when (and (scientific-dashboard-display-icons-p)
                     scientific-dashboard-set-file-icons)
            (let* ((path (car (last (split-string ,@rest " - "))))
                   (icon (if (and (not (file-remote-p path))
                                  (file-directory-p path))
                             (scientific-dashboard-icon-for-dir path
                                                     :height scientific-dashboard-icon-file-height
                                                     :v-adjust scientific-dashboard-icon-file-v-adjust)
                           (cond
                            ((or (string-equal ,section-name "Agenda for today:")
                                 (string-equal ,section-name "Agenda for the coming week:"))
                             scientific-dashboard-agenda-item-icon)
                            ((file-remote-p path)
                             scientific-dashboard-remote-path-icon)
                            (t (scientific-dashboard-icon-for-file (file-name-nondirectory path)
                                                        :height scientific-dashboard-icon-file-height
                                                        :v-adjust scientific-dashboard-icon-file-v-adjust))))))
              (setq tag (concat icon " " ,@rest))))

          (widget-create 'item
                         :tag tag
                         :action ,action
                         :button-face 'scientific-dashboard-items-face
                         :mouse-face 'highlight
                         :button-prefix ""
                         :button-suffix ""
                         :format "%[%t%]")))
      ,list)))

;;
;;; Footer

(defun scientific-dashboard-random-footer ()
  "Return a random footer from `scientific-dashboard-footer-messages'."
  (nth (random (length scientific-dashboard-footer-messages)) scientific-dashboard-footer-messages))

(defun scientific-dashboard-footer-icon ()
  "Return footer icon or a random icon if `scientific-dashboard-footer-messages' is a list."
  (if (and (not (null scientific-dashboard-footer-icon))
           (listp scientific-dashboard-footer-icon))
      (scientific-dashboard-replace-displayable
       (nth (random (length scientific-dashboard-footer-icon))
            scientific-dashboard-footer-icon))
    (scientific-dashboard-replace-displayable scientific-dashboard-footer-icon)))

(defun scientific-dashboard-insert-footer ()
  "Insert footer of dashboard."
  (when-let* ((footer (scientific-dashboard-random-footer))
              (footer-icon (scientific-dashboard-footer-icon)))
    (scientific-dashboard-insert-center
     (if (string-empty-p footer-icon) footer-icon
       (concat footer-icon " "))
     (propertize footer 'face 'scientific-dashboard-footer-face)
     "\n")))

;;
;;; Truncate

(defcustom scientific-dashboard-shorten-by-window-width nil
  "Shorten path by window edges."
  :type 'boolean
  :group 'dashboard)

(defcustom scientific-dashboard-shorten-path-offset 0
  "Shorten path offset on the edges."
  :type 'integer
  :group 'dashboard)

(defun scientific-dashboard-f-filename (path)
  "Return file name from PATH."
  (file-name-nondirectory path))

(defun scientific-dashboard-f-base (path)
  "Return directory name from PATH."
  (file-name-nondirectory (directory-file-name (file-name-directory path))))

(defun scientific-dashboard-truncate-string-ellipsis ()
  "Return the string used to indicate truncation."
  (if (fboundp 'truncate-string-ellipsis)
      (truncate-string-ellipsis)
    (or truncate-string-ellipsis
        "...")))

(defun scientific-dashboard-shorten-path-beginning (path)
  "Shorten PATH from beginning if exceeding maximum length."
  (let* ((len-path (length path))
         (slen-path (scientific-dashboard-str-len path))
         (len-rep (scientific-dashboard-str-len (scientific-dashboard-truncate-string-ellipsis)))
         (len-total (- scientific-dashboard-path-max-length len-rep))
         front)
    (if (<= slen-path scientific-dashboard-path-max-length) path
      (setq front (ignore-errors (substring path (- slen-path len-total) len-path)))
      (if front (concat (scientific-dashboard-truncate-string-ellipsis) front) ""))))

(defun scientific-dashboard-shorten-path-middle (path)
  "Shorten PATH from middle if exceeding maximum length."
  (let* ((len-path (length path))
         (slen-path (scientific-dashboard-str-len path))
         (len-rep (scientific-dashboard-str-len (scientific-dashboard-truncate-string-ellipsis)))
         (len-total (- scientific-dashboard-path-max-length len-rep))
         (center (/ len-total 2))
         (end-back center)
         (start-front (- slen-path center))
         back front)
    (if (<= slen-path scientific-dashboard-path-max-length) path
      (setq back (substring path 0 end-back)
            front (ignore-errors (substring path start-front len-path)))
      (if front (concat back (scientific-dashboard-truncate-string-ellipsis) front) ""))))

(defun scientific-dashboard-shorten-path-end (path)
  "Shorten PATH from end if exceeding maximum length."
  (let* ((len-path (length path))
         (slen-path (scientific-dashboard-str-len path))
         (len-rep (scientific-dashboard-str-len (scientific-dashboard-truncate-string-ellipsis)))
         (diff (- slen-path len-path))
         (len-total (- scientific-dashboard-path-max-length len-rep diff))
         back)
    (if (<= slen-path scientific-dashboard-path-max-length) path
      (setq back (ignore-errors (substring path 0 len-total)))
      (if (and back (< 0 scientific-dashboard-path-max-length))
          (concat back (scientific-dashboard-truncate-string-ellipsis)) ""))))

(defun scientific-dashboard--get-base-length (path type)
  "Return the length of the base from the PATH by TYPE."
  (let* ((is-dir (file-directory-p path))
         (base (if is-dir (scientific-dashboard-f-base path) (scientific-dashboard-f-filename path)))
         (option (cl-case type
                   (recents 'scientific-dashboard-recentf-show-base)
                   (bookmarks 'scientific-dashboard-bookmarks-show-base)
                   (projects 'scientific-dashboard-projects-show-base)))
         (option-val (symbol-value option))
         base-len)
    (cl-case option-val
      (`align (setq base-len (scientific-dashboard--align-length-by-type type)))
      (`nil (setq base-len 0))
      (t (setq base-len (length base))))
    base-len))

(defun scientific-dashboard-shorten-path (path type)
  "Shorten the PATH by TYPE."
  (setq path (abbreviate-file-name path))
  (let ((scientific-dashboard-path-max-length
         (if (and scientific-dashboard-path-style scientific-dashboard-shorten-by-window-width)
             (- (window-width) (scientific-dashboard--get-base-length path type)
                scientific-dashboard-shorten-path-offset)
           scientific-dashboard-path-max-length)))
    (cl-case scientific-dashboard-path-style
      (truncate-beginning (scientific-dashboard-shorten-path-beginning path))
      (truncate-middle (scientific-dashboard-shorten-path-middle path))
      (truncate-end (scientific-dashboard-shorten-path-end path))
      (t path))))

(defun scientific-dashboard-shorten-paths (paths alist type)
  "Shorten all path from PATHS by TYPE and store it to ALIST."
  (let (lst-display abbrev (index 0))
    (setf (symbol-value alist) nil)  ; reset
    (dolist (item paths)
      (setq abbrev (scientific-dashboard-shorten-path item type)
            ;; Add salt here, and use for extraction.
            ;; See function `scientific-dashboard-extract-key-path-alist'.
            abbrev (format "%s|%s" index abbrev))
      ;; store `abbrev' as id; and `item' with value
      (push (cons abbrev item) (symbol-value alist))
      (push abbrev lst-display)
      (cl-incf index))
    (reverse lst-display)))

(defun scientific-dashboard-extract-key-path-alist (key alist)
  "Remove salt from KEY, and return true shorten path from ALIST."
  (let* ((key (car (assoc key alist))) (split (split-string key "|")))
    (nth 1 split)))

(defun scientific-dashboard-expand-path-alist (key alist)
  "Get the full path (un-shorten) using KEY from ALIST."
  (cdr (assoc key alist)))

(defun scientific-dashboard--generate-align-format (fmt len)
  "Return FMT after inserting align LEN."
  (let ((pos (1+ (string-match-p "%s" fmt))))
    (concat (substring fmt 0 pos)
            (concat "-" (number-to-string len))
            (substring fmt pos (length fmt)))))

(defun scientific-dashboard--align-length-by-type (type)
  "Return the align length by TYPE of the section."
  (let ((len-item (cdr (assoc type scientific-dashboard-items))) (count 0) (align-length -1)
        len-list base)
    (cl-case type
      (`recents
       (require 'recentf)
       (setq len-list (length recentf-list))
       (while (and (< count len-item) (< count len-list))
         (setq base (nth count recentf-list)
               align-length (max align-length (scientific-dashboard-str-len (scientific-dashboard-f-filename base))))
         (cl-incf count)))
      (`bookmarks
       (let ((bookmarks-lst (bookmark-all-names)))
         (setq len-list (length bookmarks-lst))
         (while (and (< count len-item) (< count len-list))
           (setq base (nth count bookmarks-lst)
                 align-length (max align-length (scientific-dashboard-str-len base)))
           (cl-incf count))))
      (`projects
       (let ((projects-lst (scientific-dashboard-projects-backend-load-projects)))
         (setq len-list (length projects-lst))
         (while (and (< count len-item) (< count len-list))
           (setq base (nth count projects-lst)
                 align-length (max align-length (scientific-dashboard-str-len (scientific-dashboard-f-base base))))
           (cl-incf count))))
      (t (error "Unknown type for align length: %s" type)))
    align-length))

;;
;;; Recentf

(defcustom scientific-dashboard-recentf-show-base nil
  "Show the base file name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom scientific-dashboard-recentf-item-format "%s  %s"
  "Format to use when showing the base of the file name."
  :type 'string
  :group 'dashboard)

(defvar scientific-dashboard-recentf-alist nil
  "Alist records shorten's recent files and it's full paths.")

(defvar scientific-dashboard--recentf-cache-item-format nil
  "Cache to record the new generated align format.")

(defun scientific-dashboard-insert-recents (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (setq scientific-dashboard--recentf-cache-item-format nil)
  (scientific-dashboard-mute-apply
    (recentf-mode 1)
    (when scientific-dashboard-remove-missing-entry
      (ignore-errors (recentf-cleanup))))
  (scientific-dashboard-insert-section
   "Recent Files:"
   (scientific-dashboard-shorten-paths recentf-list 'scientific-dashboard-recentf-alist 'recents)
   list-size
   'recents
   (scientific-dashboard-get-shortcut 'recents)
   `(lambda (&rest _)
      (find-file-existing (scientific-dashboard-expand-path-alist ,el scientific-dashboard-recentf-alist)))
   (let* ((file (scientific-dashboard-expand-path-alist el scientific-dashboard-recentf-alist))
          (filename (scientific-dashboard-f-filename file))
          (path (scientific-dashboard-extract-key-path-alist el scientific-dashboard-recentf-alist)))
     (cl-case scientific-dashboard-recentf-show-base
       (`align
        (unless scientific-dashboard--recentf-cache-item-format
          (let* ((len-align (scientific-dashboard--align-length-by-type 'recents))
                 (new-fmt (scientific-dashboard--generate-align-format
                           scientific-dashboard-recentf-item-format len-align)))
            (setq scientific-dashboard--recentf-cache-item-format new-fmt)))
        (format scientific-dashboard--recentf-cache-item-format filename path))
       (`nil path)
       (t (format scientific-dashboard-recentf-item-format filename path))))))

;;
;;; Bookmarks

(defcustom scientific-dashboard-bookmarks-show-base t
  "Show the base file name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom scientific-dashboard-bookmarks-item-format "%s - %s"
  "Format to use when showing the base of the file name."
  :type 'string
  :group 'dashboard)

(defvar scientific-dashboard--bookmarks-cache-item-format nil
  "Cache to record the new generated align format.")

(defun scientific-dashboard-insert-bookmarks (list-size)
  "Add the list of LIST-SIZE items of bookmarks."
  (require 'bookmark)
  (scientific-dashboard-insert-section
   "Bookmarks:"
   (scientific-dashboard-subseq (bookmark-all-names) list-size)
   list-size
   'bookmarks
   (scientific-dashboard-get-shortcut 'bookmarks)
   `(lambda (&rest _) (bookmark-jump ,el))
   (if-let* ((filename el)
             (path (bookmark-get-filename el))
             (path-shorten (scientific-dashboard-shorten-path path 'bookmarks)))
       (cl-case scientific-dashboard-bookmarks-show-base
         (`align
          (unless scientific-dashboard--bookmarks-cache-item-format
            (let* ((len-align (scientific-dashboard--align-length-by-type 'bookmarks))
                   (new-fmt (scientific-dashboard--generate-align-format
                             scientific-dashboard-bookmarks-item-format len-align)))
              (setq scientific-dashboard--bookmarks-cache-item-format new-fmt)))
          (format scientific-dashboard--bookmarks-cache-item-format filename path-shorten))
         (`nil path-shorten)
         (t (format scientific-dashboard-bookmarks-item-format filename path-shorten)))
     el)))

;;
;;; Projects

(defcustom scientific-dashboard-projects-switch-function
  nil
  "Custom function to switch to projects from dashboard.
If non-NIL, should be bound to a function with one argument.  The function will
be called with the root directory of the project to switch to."
  :type '(choice (const :tag "Default" nil) function)
  :group 'dashboard)

(defcustom scientific-dashboard-projects-show-base nil
  "Show the project name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom scientific-dashboard-projects-item-format "%s  %s"
  "Format to use when showing the base of the project name."
  :type 'string
  :group 'dashboard)

(defvar scientific-dashboard-projects-alist nil
  "Alist records the shorten's project paths and it's full paths.")

(defvar scientific-dashboard--projects-cache-item-format nil
  "Cache to record the new generated align format.")

(defun scientific-dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items of projects."
  (setq scientific-dashboard--projects-cache-item-format nil)
  (scientific-dashboard-insert-section
   "Projects:"
   (scientific-dashboard-shorten-paths
    (scientific-dashboard-subseq (scientific-dashboard-projects-backend-load-projects) list-size)
    'scientific-dashboard-projects-alist 'projects)
   list-size
   'projects
   (scientific-dashboard-get-shortcut 'projects)
   `(lambda (&rest _)
      (funcall (scientific-dashboard-projects-backend-switch-function)
               (scientific-dashboard-expand-path-alist ,el scientific-dashboard-projects-alist)))
   (let* ((file (scientific-dashboard-expand-path-alist el scientific-dashboard-projects-alist))
          (filename (scientific-dashboard-f-base file))
          (path (scientific-dashboard-extract-key-path-alist el scientific-dashboard-projects-alist)))
     (cl-case scientific-dashboard-projects-show-base
       (`align
        (unless scientific-dashboard--projects-cache-item-format
          (let* ((len-align (scientific-dashboard--align-length-by-type 'projects))
                 (new-fmt (scientific-dashboard--generate-align-format
                           scientific-dashboard-projects-item-format len-align)))
            (setq scientific-dashboard--projects-cache-item-format new-fmt)))
        (format scientific-dashboard--projects-cache-item-format filename path))
       (`nil path)
       (t (format scientific-dashboard-projects-item-format filename path))))))

(defun scientific-dashboard-projects-backend-load-projects ()
  "Depending on `scientific-dashboard-projects-backend' load corresponding backend.
Return function that returns a list of projects."
  (cl-case scientific-dashboard-projects-backend
    (`projectile
     (require 'projectile)
     (when scientific-dashboard-remove-missing-entry
       (scientific-dashboard-mute-apply
         (ignore-errors (projectile-cleanup-known-projects))))
     (projectile-load-known-projects))
    (`project-el
     (require 'project)
     (when scientific-dashboard-remove-missing-entry
       (scientific-dashboard-mute-apply
         (ignore-errors
           (scientific-dashboard-funcall-fboundp #'project-forget-zombie-projects))))
     (project-known-project-roots))
    (t
     (display-warning '(dashboard)
                      "Invalid value for `scientific-dashboard-projects-backend'"
                      :error))))

(defun scientific-dashboard-projects-backend-switch-function ()
  "Return the function to switch to a project.
Custom variable `scientific-dashboard-projects-switch-function' variable takes preference
over custom backends."
  (or scientific-dashboard-projects-switch-function
      (cl-case scientific-dashboard-projects-backend
        (`projectile 'projectile-switch-project-by-name)
        (`project-el
         (lambda (project)
           "This function is used to switch to `PROJECT'."
           (let ((default-directory project))
             (project-find-file))))
        (t
         (display-warning '(dashboard)
                          "Invalid value for `scientific-dashboard-projects-backend'"
                          :error)))))

;;
;;; Org Agenda

(defcustom scientific-dashboard-week-agenda t
  "Show agenda weekly if its not nil."
  :type 'boolean
  :group 'dashboard)

(defcustom scientific-dashboard-agenda-time-string-format "%Y-%m-%d"
  "Format time of agenda entries."
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-match-agenda-entry nil
  "Match agenda to extra filter.
It is the MATCH attribute for `org-map-entries'"
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-agenda-release-buffers nil
  "If not nil use `org-release-buffers' after getting the entries."
  :type 'boolean
  :group 'dashboard)

(defcustom scientific-dashboard-filter-agenda-entry 'scientific-dashboard-filter-agenda-by-time
  "Function to filter `org-agenda' entries."
  :type '(choice
          (const :tag "No filter" scientific-dashboard-no-filter-agenda)
          (const :tag "Filter by time" scientific-dashboard-filter-agenda-by-time)
          (const :tag "Filter by todo" scientific-dashboard-filter-agenda-by-todo)
          (function :tag "Custom function"))
  :group 'dashboard)

(defcustom scientific-dashboard-agenda-sort-strategy nil
  "A list of strategies to sort the agenda.  If nil agenda is not sorted."
  :type '(repeat (choice (const priority-up) (const priority-down)
                         (const time-up) (const time-down)
                         (const todo-state-up) (const todo-state-down)))
  :group 'dashboard)

(defcustom scientific-dashboard-agenda-prefix-format " %i %-12:c %s "
  "Format for each entry in the agenda.
When the scientific-dashboard-agenda is created this format is inserted into
`org-agenda-prefix-format' as `scientific-dashboard-agenda' and compiled with
`org-compile-prefix-format' previous calling `scientific-dashboard-agenda-entry-format' for
each agenda entry."
  :type 'string
  :group 'dashboard)

(defcustom scientific-dashboard-agenda-tags-format 'identity
  "Function to format the org agenda tags.
Any custom function would receives the tags from `org-get-tags'"
  :type '(choice
          (const :tag "Show tags" identity)
          (const :tag "Hide tags" ignore)
          (function :tag "Custom function"))
  :group 'dashboard)

(defun scientific-dashboard-agenda-entry-format ()
  "Format agenda entry to show it on dashboard.

Also,it set text properties that latter are used to sort entries and perform
different actions."
  (let* ((scheduled-time (org-get-scheduled-time (point)))
         (deadline-time (org-get-deadline-time (point)))
         (entry-timestamp (scientific-dashboard-agenda--entry-timestamp (point)))
         (entry-time (or scheduled-time deadline-time entry-timestamp))
         (item (org-agenda-format-item
                (scientific-dashboard-agenda--formatted-time)
                (scientific-dashboard-agenda--formatted-headline)
                (org-outline-level)
                (org-get-category)
                (scientific-dashboard-agenda--formatted-tags)))
         (todo-state (org-get-todo-state))
         (item-priority (org-get-priority (org-get-heading t t nil t)))
         (todo-index (and todo-state
                          (length (member todo-state org-todo-keywords-1))))
         (entry-data (list 'scientific-dashboard-agenda-file (buffer-file-name)
                           'scientific-dashboard-agenda-loc (point)
                           'scientific-dashboard-agenda-priority item-priority
                           'scientific-dashboard-agenda-todo-index todo-index
                           'scientific-dashboard-agenda-time entry-time)))
    (add-text-properties 0 (length item) entry-data item)
    item))

(defun scientific-dashboard-agenda--entry-timestamp (point)
  "Get the timestamp from an entry at POINT."
  (when-let* ((timestamp (org-entry-get point "TIMESTAMP")))
    (org-time-string-to-time timestamp)))

(defun scientific-dashboard-agenda--formatted-headline ()
  "Set agenda faces to `HEADLINE' when face text property is nil."
  (let* ((headline (org-link-display-format (org-get-heading t t t t)))
         (todo (or (org-get-todo-state) ""))
         (org-level-face (nth (- (org-outline-level) 1) org-level-faces))
         (todo-state (format org-agenda-todo-keyword-format todo)))
    (scientific-dashboard-agenda--set-face org-level-face headline)
    (scientific-dashboard-agenda--set-face (org-get-todo-face todo) todo-state)
    (concat todo-state " " headline)))

(defun scientific-dashboard-agenda--set-face (face text)
  "Add FACE to TEXT but inherit height from `scientific-dashboard-items-face'.
If not height is found on FACE or `scientific-dashboard-items-face' use `default'."
  (let ((height (face-attribute 'scientific-dashboard-items-face :height nil 'default)))
    (add-face-text-property 0 (length text) `((:height ,height) ,face) nil text)))

(defun scientific-dashboard-agenda--formatted-time ()
  "Get the scheduled or dead time of an entry.  If no time is found return nil."
  (when-let* ((time (or (org-get-scheduled-time (point)) (org-get-deadline-time (point))
                        (scientific-dashboard-agenda--entry-timestamp (point)))))
    (format-time-string scientific-dashboard-agenda-time-string-format time)))

(defun scientific-dashboard-agenda--formatted-tags ()
  "Apply `scientific-dashboard-agenda-tags-format' to org-element tags."
  (when scientific-dashboard-agenda-tags-format
    (funcall scientific-dashboard-agenda-tags-format (org-get-tags))))

(defun scientific-dashboard-due-date-for-agenda ()
  "Return due-date for agenda period."
  (if scientific-dashboard-week-agenda
      (time-add (current-time) (* 86400 8))
    (time-add (current-time) 86400)))

(defun scientific-dashboard-filter-agenda-by-time ()
  "Include entry if it has a scheduled-time or deadline-time in the future.
An entry is included if this function returns nil and excluded if returns a
point."
  (let ((scheduled-time (org-get-scheduled-time (point)))
        (deadline-time (org-get-deadline-time (point)))
        (entry-timestamp (scientific-dashboard-agenda--entry-timestamp (point)))
        (due-date (scientific-dashboard-due-date-for-agenda))
        (now (current-time)))
    (unless (and (not (org-entry-is-done-p))
                 (not (org-in-archived-heading-p))
                 (or (and scheduled-time
                          (time-less-p scheduled-time due-date))
                     (and deadline-time
                          (time-less-p deadline-time due-date))
                     (and entry-timestamp
                          (time-less-p now entry-timestamp)
                          (time-less-p entry-timestamp due-date))))
      (point))))

(defun scientific-dashboard-filter-agenda-by-todo ()
  "Include entry if it is todo and not done.
An entry is included if this function returns nil and excluded
if returns a point."
  (unless (and (org-entry-is-todo-p)
               (not (org-entry-is-done-p))
               (not (org-in-archived-heading-p)))
    (point)))

(defun scientific-dashboard-no-filter-agenda ()
  "No filter agenda entries."
  (when (org-entry-is-done-p) (point)))

(defun scientific-dashboard-get-agenda ()
  "Get agenda items for today or for a week from now."
  (if-let* ((prefix-format (assoc 'scientific-dashboard-agenda org-agenda-prefix-format)))
      (setcdr prefix-format scientific-dashboard-agenda-prefix-format)
    (push (cons 'scientific-dashboard-agenda scientific-dashboard-agenda-prefix-format) org-agenda-prefix-format))
  (org-compile-prefix-format 'scientific-dashboard-agenda)
  (prog1 (org-map-entries 'scientific-dashboard-agenda-entry-format
                          scientific-dashboard-match-agenda-entry
                          'agenda
                          scientific-dashboard-filter-agenda-entry)
    (scientific-dashboard-agenda--release-buffers)))

(defun scientific-dashboard-agenda--release-buffers ()
  "Release agenda buffers buffers.
This is what `org-agenda-exit' do."
  (when scientific-dashboard-agenda-release-buffers
    (org-release-buffers org-agenda-new-buffers)
    (setq org-agenda-new-buffers nil)))

(defun scientific-dashboard-agenda--sorted-agenda ()
  "Return agenda sorted by time.

For now, it only works when scientific-dashboard-agenda has been filter by time and
scientific-dashboard-agenda-sort is not nil."
  (let ((agenda (scientific-dashboard-get-agenda))
        (sort-function (scientific-dashboard-agenda--sort-function)))
    (sort agenda sort-function)))

(defun scientific-dashboard-agenda--sort-function ()
  "Get the function use to sorted the agenda.

Depending on the list `scientific-dashboard-agenda-sorting-strategy' use this strategies to
build a predicate to compare each enty.
This is similar as `org-entries-lessp' but with a different aproach."
  (scientific-dashboard-agenda--build-sort-function scientific-dashboard-agenda-sort-strategy))

(defun scientific-dashboard-agenda--build-sort-function (strategies)
  "Build a predicate to sort the dashboard agenda.

If `STRATEGIES' is nil then sort using the nil predicate.  Look for the strategy
predicate, the attributes of the entry and compare entries.  If no predicate is
found for the strategy it uses nil predicate."
  (if (null strategies) (lambda (_dont _care) nil)
    (let ((predicate (scientific-dashboard-agenda--build-sort-function-predicate
                      (car strategies)))
          (attribute (scientific-dashboard-agenda--build-sort-function-attribute
                      (car strategies))))
      (if (null predicate) (lambda (_dont _care) nil)
        (lambda (entry1 entry2)
          (scientific-dashboard-agenda--compare-entries entry1 entry2 (cdr strategies)
                                             predicate attribute))))))

(defun scientific-dashboard-agenda--build-sort-function-predicate (strategy)
  "Return the predicate to compare two entryes depending on the `STRATEGY'."
  (cl-case strategy
    (`priority-up '>)
    (`priority-down '<)
    (`time-up 'time-less-p)
    (`time-down (lambda (a b) (time-less-p b a)))
    (`todo-state-up '>)
    (`todo-state-down '<)))

(defun scientific-dashboard-agenda--build-sort-function-attribute (strategy)
  "Return the argument to compare two entries depending to the `STRATEGY'."
  (cond
   ((memq strategy '(priority-up priority-down)) 'scientific-dashboard-agenda-priority)
   ((memq strategy '(time-up time-down)) 'scientific-dashboard-agenda-time)
   ((memq strategy '(todo-state-up todo-state-down)) 'scientific-dashboard-agenda-todo-index)
   (t nil)))

(defun scientific-dashboard-agenda--compare-entries (entry1 entry2 strategies predicate attribute)
  "Compare `ENTRY1' and `ENTRY2' by `ATTRIBUTE' using `PREDICATE'.
If both attributes are nil or equals the next strategy in `STRATEGIES' is used
to compare."
  (let ((arg1 (get-text-property 0 attribute entry1))
        (arg2 (get-text-property 0 attribute entry2)))
    (cond
     ((or (and (null arg1) (null arg2)) (equal arg1 arg2))
      (apply (scientific-dashboard-agenda--build-sort-function strategies) (list entry1 entry2)))
     ((null arg1) nil)
     ((null arg2) t)
     (t (apply predicate (list arg1 arg2))))))

(defun scientific-dashboard-agenda--visit-file (file point)
  "Action on agenda-entry that visit a FILE at POINT."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (goto-char point)
      (switch-to-buffer buffer)
      (recenter-top-bottom))))

(defun scientific-dashboard-agenda--visit-file-other-window (file point)
  "Visit FILE at POINT of an agenda item in other window."
  (let ((buffer (find-file-other-window file)))
    (with-current-buffer buffer (goto-char point) (recenter-top-bottom))))

(defun scientific-dashboard-insert-agenda (list-size)
  "Add the list of LIST-SIZE items of agenda."
  (require 'org-agenda)
  (scientific-dashboard-insert-section
   (if scientific-dashboard-week-agenda
       "Agenda for the coming week:"
     "Agenda for today:")
   (scientific-dashboard-agenda--sorted-agenda)
   list-size
   'agenda
   (scientific-dashboard-get-shortcut 'agenda)
   `(lambda (&rest _)
      (let ((file (get-text-property 0 'scientific-dashboard-agenda-file ,el))
            (point (get-text-property 0 'scientific-dashboard-agenda-loc ,el)))
        (funcall scientific-dashboard-agenda-action file point)))
   (format "%s" el)))

;;
;;; Registers

(defun scientific-dashboard-insert-registers (list-size)
  "Add the list of LIST-SIZE items of registers."
  (require 'register)
  (scientific-dashboard-insert-section
   "Registers:"
   register-alist
   list-size
   'registers
   (scientific-dashboard-get-shortcut 'registers)
   (lambda (&rest _) (jump-to-register (car el)))
   (format "%c - %s" (car el) (register-describe-oneline (car el)))))

;;; Recent Documents

(defun scientific-dashboard-insert-recent-documents (list-size)
  "Insert recent documents from the scientific mapping system."
  (when (and (featurep 'doc-engine) (fboundp 'doc-engine-get-recent-documents))
    (let ((recent-docs (doc-engine-get-recent-documents list-size)))
      (scientific-dashboard-insert-section
       "Recent Documents:"
       recent-docs
       list-size
       'recent-documents
       (scientific-dashboard-get-shortcut 'recent-documents)
       (lambda (&rest _) (find-file (cdr el)))
       (car el)))))

;;; Citation Stats

(defun scientific-dashboard-insert-citation-stats (list-size)
  "Insert citation statistics from the scientific mapping system."
  (when (and (featurep 'citation-database) (fboundp 'citation-database-get-stats))
    (let ((stats (citation-database-get-stats)))
      (scientific-dashboard-insert-section
       "Citation Stats:"
       stats
       list-size
       'citation-stats
       (scientific-dashboard-get-shortcut 'citation-stats)
       nil
       (format "%s: %s" (car el) (cdr el))))))

;;; Concept Relationships

(defun scientific-dashboard-insert-concept-relationships (list-size)
  "Insert recent concept relationships from the scientific mapping system."
  (when (and (featurep 'concept-relationships) (fboundp 'concept-relationships-get-recent))
    (let ((concepts (concept-relationships-get-recent list-size)))
      (scientific-dashboard-insert-section
       "Recent Concepts:"
       concepts
       list-size
       'concept-relationships
       (scientific-dashboard-get-shortcut 'concept-relationships)
       (lambda (&rest _) (concept-relationships-open (car el)))
       (car el)))))

(provide 'scientific-dashboard-widgets)
;;; scientific-dashboard-widgets.el ends here
