;;; tests/agenda-integration-tests.el --- Tests for org-agenda integration -*- lexical-binding: t -*-

(require 'ert)
(require 'scientific-mapping)
(require 'org-agenda)

(ert-deftest scientific-mapping-agenda-setup-test ()
  "Test that agenda integration is properly set up."
  (let ((original-agenda-files org-agenda-files))
    (unwind-protect
        (progn
          (scientific-mapping-setup-agenda)
          ;; Check that agenda files are added
          (should (member (car scientific-mapping-agenda-files) org-agenda-files))
          ;; Check that agenda file is created
          (should (file-exists-p (car scientific-mapping-agenda-files))))
      ;; Cleanup
      (setq org-agenda-files original-agenda-files)
      (when (file-exists-p (car scientific-mapping-agenda-files))
        (delete-file (car scientific-mapping-agenda-files))))))

(ert-deftest scientific-mapping-schedule-paper-review-test ()
  "Test scheduling a paper review in agenda."
  (let ((test-agenda-file (expand-file-name "test-agenda.org" temporary-file-directory))
        (original-agenda-files scientific-mapping-agenda-files))
    (unwind-protect
        (progn
          (setq scientific-mapping-agenda-files (list test-agenda-file))
          (scientific-mapping-schedule-paper-review "10.1000/test.doi")
          ;; Check that agenda file contains the scheduled item
          (should (file-exists-p test-agenda-file))
          (with-current-buffer (find-file-noselect test-agenda-file)
            (should (search-forward "10.1000/test.doi" nil t))))
      ;; Cleanup
      (setq scientific-mapping-agenda-files original-agenda-files)
      (when (file-exists-p test-agenda-file)
        (delete-file test-agenda-file)))))

(ert-deftest scientific-mapping-create-research-project-test ()
  "Test creating a research project in agenda."
  (let ((test-agenda-file (expand-file-name "test-project.org" temporary-file-directory))
        (original-agenda-files scientific-mapping-agenda-files))
    (unwind-protect
        (progn
          (setq scientific-mapping-agenda-files (list test-agenda-file))
          (scientific-mapping-create-research-project "Test Project" "A test research project")
          ;; Check that agenda file contains the project
          (should (file-exists-p test-agenda-file))
          (with-current-buffer (find-file-noselect test-agenda-file)
            (should (search-forward "Test Project" nil t))
            (should (search-forward "Project Planning" nil t))
            (should (search-forward "Literature Review" nil t))))
      ;; Cleanup
      (setq scientific-mapping-agenda-files original-agenda-files)
      (when (file-exists-p test-agenda-file)
        (delete-file test-agenda-file)))))

(provide 'agenda-integration-tests)