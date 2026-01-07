;;; core/helpers.el --- General helper functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Scientific Knowledge Mapping System

;;; Commentary:

;; Common utility functions used throughout scientific-mapping.
;; - String manipulation
;; - List operations
;; - Timing utilities
;; - Debugging helpers

;;; Code:

(defun scientific-mapping--trim (string)
  "Trim whitespace from STRING."
  (replace-regexp-in-string "^[[:space:]]\\|[[:space:]]$" "" string))

(defun scientific-mapping--split (string &optional delim)
  "Split STRING by DELIM (default: comma)."
  (let ((delim (or delim ",")))
    (mapcar #'scientific-mapping--trim
            (split-string string delim t))))

(defun scientific-mapping--join (list &optional delim)
  "Join LIST into string with DELIM (default: comma)."
  (let ((delim (or delim ", ")))
    (mapconcat #'identity list delim)))

(defun scientific-mapping--uuid ()
  "Generate a simple UUID-like string."
  (format "%s-%s-%s-%s"
          (md5 (number-to-string (random)))
          (md5 (number-to-string (float-time)))
          (md5 (number-to-string (current-time)))
          (md5 (user-login-name))))

(defun scientific-mapping--timestamp (&optional format)
  "Return current timestamp. FORMAT: 'iso, 'human, or 'compact."
  (let ((ts (current-time)))
    (pcase (or format 'iso)
      ('iso (format-time-string "%Y-%m-%dT%H:%M:%S" ts))
      ('human (format-time-string "%Y-%m-%d %a %H:%M" ts))
      ('compact (format-time-string "%Y%m%dT%H%M%S" ts)))))

(defun scientific-mapping--measure-time (fn)
  "Measure and report time taken by FN."
  (let ((start (current-time)))
    (prog1 (funcall fn)
      (message "Elapsed: %.3f seconds"
               (float-time (time-subtract (current-time) start))))))

(defun scientific-mapping--debug (&rest args)
  "Print debug message ARGS when debug mode is enabled."
  (when (bound-and-true-p scientific-mapping-debug)
    (apply #'message (concat "[debug] " (car args)) (cdr args))))

(defun scientific-mapping--safe-call (fn &rest args)
  "Call FN with ARGS, return nil on error."
  (condition-case err
      (apply fn args)
    (error
     (message "scientific-mapping error: %s" (error-message-string err))
     nil)))

(defun scientific-mapping--plist-get (plist key &optional default)
  "Get KEY from PLIST with DEFAULT if not found."
  (let ((val (plist-member plist key)))
    (if val (cadr val) default)))

(defun scientific-mapping--alist-get (alist key &optional default)
  "Get KEY from ALIST with DEFAULT if not found."
  (or (alist-get key alist) default))

(provide 'core/helpers)
;;; core/helpers.el ends here
