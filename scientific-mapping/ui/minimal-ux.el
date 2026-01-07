;;; minimal-ux.el --- Minimal UX configuration for pure content focus -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This file configures Emacs for minimal, distraction-free research work.
;; Removes mode line, tabs, scroll bars, and all UI clutter - pure content focus.

;;; Code:

(defgroup minimal-ux ()
  "Minimal UX configuration."
  :group 'scientific-mapping)

(defcustom minimal-ux-enable t
  "Enable minimal UX configuration."
  :group 'minimal-ux
  :type 'boolean)

;;;; Minimal UI Configuration

;;;###autoload
(defun minimal-ux-enable ()
  "Enable minimal, distraction-free research interface."
  (interactive)
  (when minimal-ux-enable
    ;; Remove mode line
    (setq-default mode-line-format nil)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq mode-line-format nil)))

    ;; Remove scroll bars
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)

    ;; Remove tool bar
    (tool-bar-mode -1)

    ;; Remove menu bar
    (menu-bar-mode -1)

    ;; Remove tab bar
    (tab-bar-mode -1)
    (tab-line-mode -1)

    ;; Remove fringes
    (set-fringe-style 0)

    ;; Minimal window margins
    (set-window-margins nil 2 2)

    ;; Use Modus theme (built-in, no custom themes)
    (load-theme 'modus-operandi t)

    ;; Disable cursor blinking
    (blink-cursor-mode -1)

    ;; Disable line numbers (pure content focus)
    (global-display-line-numbers-mode -1)

    ;; Disable visual line mode for hard wrapping
    (global-visual-line-mode -1)

    ;; Clean buffer names
    (minimal-ux-clean-buffer-names)

    ;; Setup hooks for new buffers
    (add-hook 'find-file-hook #'minimal-ux-setup-buffer)
    (add-hook 'org-mode-hook #'minimal-ux-setup-org-buffer)

    (message "Minimal UX enabled - pure content focus")))

;;;###autoload
(defun minimal-ux-disable ()
  "Disable minimal UX and restore standard interface."
  (interactive)
    ;; Restore mode line
    (setq-default mode-line-format
                  '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

    ;; Restore scroll bars
    (scroll-bar-mode 1)

    ;; Restore tool bar
    (tool-bar-mode 1)

    ;; Restore menu bar
    (menu-bar-mode 1)

    ;; Restore fringes
    (set-fringe-style nil)

    ;; Reset margins
    (set-window-margins nil 0 0)

    ;; Re-enable line numbers
    (global-display-line-numbers-mode 1)

    ;; Remove hooks
    (remove-hook 'find-file-hook #'minimal-ux-setup-buffer)
    (remove-hook 'org-mode-hook #'minimal-ux-setup-org-buffer)

    (message "Minimal UX disabled - standard interface restored"))

;;;; Buffer Setup Functions

(defun minimal-ux-setup-buffer ()
  "Setup buffer for minimal UX."
  (when minimal-ux-enable
    ;; Remove mode line for this buffer
    (setq-local mode-line-format nil)

    ;; Clean buffer name for research files
    (when (minimal-ux-is-research-buffer-p)
      (minimal-ux-clean-buffer-name))))

(defun minimal-ux-setup-org-buffer ()
  "Setup Org buffer for minimal research experience."
  (when minimal-ux-enable
    ;; Hide Org markup for pure content reading
    (minimal-ux-toggle-org-markup)

    ;; Set up clean Org appearance
    (minimal-ux-clean-org-appearance)))

(defun minimal-ux-is-research-buffer-p ()
  "Check if current buffer is a research document."
  (or (and (buffer-file-name)
           (string-match-p "\\.org$" (buffer-file-name)))
      (eq major-mode 'org-mode)
      (member (current-buffer) (mapcar #'get-file-buffer (doc-engine-all-files)))))

(defun minimal-ux-clean-buffer-names ()
  "Clean up buffer names for research work."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (minimal-ux-is-research-buffer-p)
        (minimal-ux-clean-buffer-name)))))

(defun minimal-ux-clean-buffer-name ()
  "Clean buffer name for research documents."
  (let ((name (buffer-name)))
    ;; Remove directory paths and extensions for cleaner names
    (when (string-match "\\(.+\\)\\.org" name)
      (rename-buffer (match-string 1 name) t))))

;;;; Org Mode Minimal Appearance

(defvar minimal-ux-org-markup-hidden nil
  "Whether Org markup is currently hidden.")

;;;###autoload
(defun minimal-ux-toggle-org-markup ()
  "Toggle visibility of Org markup for pure content reading."
  (interactive)
  (if minimal-ux-org-markup-hidden
      (minimal-ux-show-org-markup)
    (minimal-ux-hide-org-markup)))

(defun minimal-ux-hide-org-markup ()
  "Hide Org markup for distraction-free reading."
  (interactive)
  (when (eq major-mode 'org-mode)
    ;; Hide stars
    (font-lock-add-keywords
     nil
     '(("^\\*+ " (0 (prog1 nil
                      (put-text-property (match-beginning 0) (match-end 0)
                                       'invisible t))))))

    ;; Hide properties
    (add-to-invisibility-spec '(org-properties . t))
    (font-lock-add-keywords
     nil
     '(("^:PROPERTIES:$" (0 '(face nil invisible org-properties)))
       ("^:END:$" (0 '(face nil invisible org-properties)))
       ("^:\\(.+\\):" (0 '(face nil invisible org-properties)))))

    ;; Hide drawers
    (add-to-invisibility-spec '(org-drawers . t))

    (setq minimal-ux-org-markup-hidden t)
    (font-lock-fontify-buffer)
    (message "Org markup hidden - pure content focus")))

(defun minimal-ux-show-org-markup ()
  "Show Org markup."
  (interactive)
  (remove-from-invisibility-spec '(org-properties . t))
  (remove-from-invisibility-spec '(org-drawers . t))
  (font-lock-remove-keywords nil '("^\\*+ " "^:PROPERTIES:$" "^:END:$" "^:\\(.+\\):"))
  (setq minimal-ux-org-markup-hidden nil)
  (font-lock-fontify-buffer)
  (message "Org markup shown"))

(defun minimal-ux-clean-org-appearance ()
  "Setup clean Org appearance for research reading."
  (when minimal-ux-enable
    ;; Use clean fonts and spacing
    (setq-local line-spacing 0.1)

    ;; Clean heading appearance
    (set-face-attribute 'org-level-1 nil :height 1.2 :weight 'normal :foreground (face-foreground 'default))
    (set-face-attribute 'org-level-2 nil :height 1.1 :weight 'normal :foreground (face-foreground 'default))
    (set-face-attribute 'org-level-3 nil :height 1.0 :weight 'normal :foreground (face-foreground 'default))

    ;; Minimal link appearance
    (set-face-attribute 'org-link nil :underline nil :foreground (face-foreground 'default))
    (set-face-attribute 'org-footnote nil :underline nil :foreground (face-foreground 'default))

    ;; Clean list appearance
    (set-face-attribute 'org-list-dt nil :weight 'normal)
    (set-face-attribute 'org-checkbox nil :foreground (face-foreground 'default))))

;;;; Research Workflow Integration

;;;###autoload
(defun minimal-ux-research-session ()
  "Start a minimal research session with pure content focus."
  (interactive)
  (minimal-ux-enable)
  (tutorial-system-first-session-walkthrough)
  (message "Research session started - pure content focus"))

;;;###autoload
(defun minimal-ux-toggle-research-mode ()
  "Toggle between minimal research mode and standard Emacs."
  (interactive)
  (if minimal-ux-enable
      (minimal-ux-disable)
    (minimal-ux-enable)))

;;;; Keyboard-Only Workflow

(defvar minimal-ux-keyboard-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Pure keyboard navigation
    (define-key map (kbd "C-c r q") 'doc-engine-quick-note)
    (define-key map (kbd "C-c r i") 'scientific-mapping-import-paper)
    (define-key map (kbd "C-c r a") 'ai-integration-analyze-document)
    (define-key map (kbd "C-c r v") 'viz-engine-open)
    (define-key map (kbd "C-c r s") 'workflow-integration-unified-search)
    (define-key map (kbd "C-c r g") 'git-integration-smart-commit)
    (define-key map (kbd "C-c r m") 'minimal-ux-toggle-org-markup)
    (define-key map (kbd "C-c r f") 'org-file-manager)
    (define-key map (kbd "C-c r w") 'workflow-integration-frictionless-research-session)
    map)
  "Minimal keyboard-only research workflow map.")

;;;###autoload
(defun minimal-ux-enable-keyboard-workflow ()
  "Enable pure keyboard research workflow."
  (interactive)
  (use-local-map minimal-ux-keyboard-mode-map)
  (message "Pure keyboard research workflow enabled"))

;;;; Initialization

(defun minimal-ux-initialize ()
  "Initialize minimal UX."
  (when minimal-ux-enable
    (minimal-ux-enable))
  (message "Minimal UX initialized"))

;; Auto-initialize
(minimal-ux-initialize)

(provide 'minimal-ux)

;;; minimal-ux.el ends here

;;; Usage:
;;
;; Minimal UX provides pure content focus for research work:
;; - minimal-ux-enable: Enable minimal, distraction-free interface
;; - minimal-ux-disable: Restore standard Emacs interface
;; - minimal-ux-toggle-org-markup: Hide/show Org markup
;; - minimal-ux-research-session: Start focused research session
;; - minimal-ux-enable-keyboard-workflow: Pure keyboard navigation
;;
;; Features:
;; - No mode line, tabs, scroll bars, or UI clutter
;; - Clean buffer names and Org appearance
;; - Modus theme integration
;; - Pure content focus for maximum productivity