;;; git-integration.el --- Git/Magit integration for research workflows -*- lexical-binding: t -*-

;; Copyright (C) 2025  Scientific Knowledge Mapping System
;; Author: Scientific Tools Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This file provides deep Git/Magit integration as a core pillar of the
;; scientific mapping system. Research documents are automatically versioned,
;; changes tracked, and workflows integrated with Git operations.

;;; Code:

;; (require 'scientific-mapping)  ; Package main feature loaded via scientific-mapping.el
(eval-when-compile (require 'subr-x))

(defgroup git-integration ()
  "Git integration for research workflows."
  :group 'scientific-mapping)

(defcustom git-integration-auto-commit t
  "Automatically commit document changes."
  :group 'git-integration
  :type 'boolean)

(defcustom git-integration-auto-commit-delay 30
  "Delay in seconds before auto-committing changes."
  :group 'git-integration
  :type 'integer)

(defcustom git-integration-smart-commit-messages t
  "Generate smart commit messages based on document changes."
  :group 'git-integration
  :type 'boolean)

(defcustom git-integration-research-branching t
  "Create research-specific branches for different projects."
  :group 'git-integration
  :type 'boolean)

(defvar git-integration-auto-commit-timer nil
  "Timer for auto-committing changes.")

(defvar git-integration-last-commit-time nil
  "Timestamp of last auto-commit.")

;;;; Core Git Integration

;;;###autoload
(defun git-integration-init-research-repo ()
  "Initialize a Git repository for research documents."
  (interactive)
  (let ((dir (or (read-directory-name "Research directory: " default-directory)
                 doc-engine-directory)))
    (when (and dir (not (file-exists-p (expand-file-name ".git" dir))))
      (let ((default-directory dir))
        (shell-command "git init")
        (shell-command "git config user.name \"Research Workflow\"")
        (shell-command "git config user.email \"research@workflow.local\"")

        ;; Create .gitignore for research
        (with-temp-file ".gitignore"
          (insert "# Research artifacts\n"
                  "*.pdf\n"
                  "*.log\n"
                  "*~\n"
                  "#.org#\n"
                  "auto-save-list\n"
                  "*.bak\n"
                  "# API keys and sensitive data\n"
                  ".env\n"
                  "secrets\n"))

        ;; Initial commit
        (shell-command "git add .gitignore")
        (shell-command "git commit -m \"Initialize research repository\"")

        (message "Research repository initialized in %s" dir)))))

;;;###autoload
(defun git-integration-smart-commit ()
  "Create a smart commit for current research changes."
  (interactive)
  (if (not (git-integration-git-repo-p))
      (when (y-or-n-p "No Git repository found. Initialize one? ")
        (git-integration-init-research-repo))
    (let ((status (git-integration-get-status))
          (message (git-integration-generate-commit-message)))
      (when status
        (shell-command "git add -A")
        (shell-command (format "git commit -m \"%s\"" message))
        (message "Committed: %s" message)))))

(defun git-integration-generate-commit-message ()
  "Generate a smart commit message based on changes."
  (let ((changed-files (git-integration-get-changed-files))
        (new-files (git-integration-get-new-files)))
    (cond
     ;; New research documents
     (new-files
      (format "Add research documents: %s"
              (string-join (mapcar #'file-name-base new-files) ", ")))

     ;; Document modifications
     (changed-files
      (let ((doc-changes (seq-filter (lambda (f) (string-match-p "\\.org$" f)) changed-files)))
        (if doc-changes
            (format "Update research documents: %s"
                    (string-join (mapcar #'file-name-base doc-changes) ", "))
          (format "Update research files: %d files" (length changed-files)))))

     ;; Default
     (t "Update research materials"))))

(defun git-integration-get-status ()
  "Get Git repository status."
  (when (git-integration-git-repo-p)
    (shell-command-to-string "git status --porcelain")))

(defun git-integration-get-changed-files ()
  "Get list of changed files."
  (when (git-integration-git-repo-p)
    (let ((output (shell-command-to-string "git diff --name-only")))
      (split-string output "\n" t))))

(defun git-integration-get-new-files ()
  "Get list of new files."
  (when (git-integration-git-repo-p)
    (let ((output (shell-command-to-string "git ls-files --others --exclude-standard")))
      (split-string output "\n" t))))

(defun git-integration-git-repo-p ()
  "Check if current directory is a Git repository."
  (file-exists-p ".git"))

;;;; Automatic Version Control

(defun git-integration-enable-auto-commit ()
  "Enable automatic committing of research changes."
  (interactive)
  (when git-integration-auto-commit
    (git-integration-start-auto-commit-timer)
    (add-hook 'after-save-hook #'git-integration-after-save-hook)
    (message "Auto-commit enabled")))

(defun git-integration-disable-auto-commit ()
  "Disable automatic committing."
  (interactive)
  (git-integration-stop-auto-commit-timer)
  (remove-hook 'after-save-hook #'git-integration-after-save-hook)
  (message "Auto-commit disabled"))

(defun git-integration-start-auto-commit-timer ()
  "Start the auto-commit timer."
  (when git-integration-auto-commit-timer
    (cancel-timer git-integration-auto-commit-timer))
  (setq git-integration-auto-commit-timer
        (run-with-timer git-integration-auto-commit-delay
                       git-integration-auto-commit-delay
                       #'git-integration-check-and-commit)))

(defun git-integration-stop-auto-commit-timer ()
  "Stop the auto-commit timer."
  (when git-integration-auto-commit-timer
    (cancel-timer git-integration-auto-commit-timer)
    (setq git-integration-auto-commit-timer nil)))

(defun git-integration-after-save-hook ()
  "Hook that runs after saving a research document."
  (when (and git-integration-auto-commit
             (git-integration-is-research-file-p))
    (run-with-timer 2 nil #'git-integration-delayed-commit)))

(defun git-integration-is-research-file-p ()
  "Check if current file is a research document."
  (and (buffer-file-name)
       (or (string-match-p "\\.org$" (buffer-file-name))
           (member (buffer-file-name) (doc-engine-all-files)))))

(defun git-integration-check-and-commit ()
  "Check for changes and commit if needed."
  (when (and git-integration-auto-commit
             (git-integration-git-repo-p))
    (let ((status (git-integration-get-status)))
      (when (and status (not (string-empty-p status)))
        (git-integration-smart-commit)
        (setq git-integration-last-commit-time (current-time))))))

(defun git-integration-delayed-commit ()
  "Perform delayed commit after file save."
  (when (and (not git-integration-last-commit-time)
             (> (float-time (time-subtract (current-time) git-integration-last-commit-time))
                git-integration-auto-commit-delay))
    (git-integration-check-and-commit)))

;;;; Research Branching

;;;###autoload
(defun git-integration-create-research-branch (topic)
  "Create a new branch for a research topic."
  (interactive "sResearch topic: ")
  (let ((branch-name (format "research/%s"
                            (replace-regexp-in-string "[^a-zA-Z0-9-]" "-"
                                                    (downcase topic)))))
    (shell-command (format "git checkout -b %s" branch-name))
    (message "Created and switched to research branch: %s" branch-name)))

;;;###autoload
(defun git-integration-merge-research-branch (branch)
  "Merge a research branch into main."
  (interactive
   (list (completing-read "Branch to merge: "
                         (git-integration-get-branches))))
  (shell-command (format "git checkout main && git merge %s" branch))
  (message "Merged %s into main" branch))

(defun git-integration-get-branches ()
  "Get list of Git branches."
  (let ((output (shell-command-to-string "git branch -a")))
    (seq-filter (lambda (line) (not (string-empty-p line)))
               (split-string output "\n"))))

;;;; Research History & Versioning

;;;###autoload
(defun git-integration-research-history (file)
  "Show version history for a research document."
  (interactive
   (list (read-file-name "Document: " nil (buffer-file-name))))
  (let ((buffer (get-buffer-create "*Research Document History*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "#+TITLE: Version History: %s\n\n" (file-name-nondirectory file)))
      (insert "* Commit History\n\n")

      (let ((history (shell-command-to-string
                     (format "git log --oneline --follow -- %s" file))))
        (dolist (line (split-string history "\n" t))
          (when (string-match "\\([a-f0-9]+\\) \\(.+\\)" line)
            (let ((commit (match-string 1 line))
                  (message (match-string 2 line)))
              (insert (format "- %s: %s\n" commit message))))))

      (goto-char (point-min))
      (org-mode))
    (switch-to-buffer buffer)))

;;;###autoload
(defun git-integration-compare-research-versions (file rev1 rev2)
  "Compare two versions of a research document."
  (interactive
   (list (read-file-name "Document: " nil (buffer-file-name))
         (read-string "Revision 1: " "HEAD~1")
         (read-string "Revision 2: " "HEAD")))
  (let ((buffer (get-buffer-create "*Research Version Comparison*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "#+TITLE: Version Comparison: %s\n\n" (file-name-nondirectory file)))
      (insert (format "* Comparing %s..%s\n\n" rev1 rev2))

      (let ((diff (shell-command-to-string
                  (format "git diff %s %s -- %s" rev1 rev2 file))))
        (if (string-empty-p diff)
            (insert "No differences found.\n")
          (insert "** Differences\n\n")
          (insert "#+BEGIN_SRC diff\n")
          (insert diff)
          (insert "#+END_SRC\n")))

      (goto-char (point-min))
      (org-mode))
    (switch-to-buffer buffer)))

;;;; Research Workflow Integration

;;;###autoload
(defun git-integration-commit-research-milestone (milestone)
  "Commit a research milestone with detailed message."
  (interactive "sMilestone description: ")
  (let ((message (format "Milestone: %s

- Research progress update
- Document changes committed
- Knowledge graph updated
- Citations synchronized

Committed: %s" milestone (format-time-string "%Y-%m-%d %H:%M"))))
    (shell-command "git add -A")
    (shell-command (format "git commit -m \"%s\"" message))
    (message "Milestone committed: %s" milestone)))

;;;###autoload
(defun git-integration-create-research-tag (version)
  "Create a Git tag for a research version."
  (interactive "sVersion/tag name: ")
  (let ((tag-message (format "Research version %s

- Document collection complete
- Analysis finished
- Results compiled
- Ready for submission

Tagged: %s" version (format-time-string "%Y-%m-%d %H:%M"))))
    (shell-command (format "git tag -a %s -m \"%s\"" version tag-message))
    (message "Research version tagged: %s" version)))

;;;; Initialization

(defun git-integration-initialize ()
  "Initialize Git integration."
  (when git-integration-auto-commit
    (git-integration-enable-auto-commit))
  (message "Git integration initialized"))

;; Auto-initialize
(git-integration-initialize)

(provide 'git-integration)

;;; git-integration.el ends here

;;; Usage:
;;
;; Git integration provides version control as a core research workflow pillar:
;; - git-integration-init-research-repo: Initialize Git repo for research
;; - git-integration-smart-commit: Auto-commit with intelligent messages
;; - git-integration-research-history: View document version history
;; - git-integration-create-research-branch: Branch per research topic
;; - git-integration-commit-research-milestone: Milestone commits
;; - git-integration-create-research-tag: Version tagging
;;
;; Automatic features:
;; - Auto-commit on document save
;; - Smart commit message generation
;; - Research workflow integration
;; - Branch management for different projects