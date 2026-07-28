;;; reaktor-org-agenda.el --- Agenda commands -*- lexical-binding: t -*-

;; Author: Aleksandar Terentić

;;; Commentary:

;; Two commands for the agenda buffer: one reloads it from disk after the
;; Org files have been changed elsewhere, the other hides everything dated
;; ahead of today so only what is actionable now remains.

;;; Code:

;; Loaded from org-agenda's own configuration, so requiring it back is free.
(require 'cl-lib)
(require 'org-agenda)

(defun reaktor/org-agenda-reload-from-disk ()
  "Kill all Org buffers and reload agenda from disk."
  (interactive)
  (dolist (buf (org-buffer-list))
    (kill-buffer buf))
  (org-agenda-list)
  (org-agenda-to-appt))

(defvar reaktor/org-agenda-hide-future nil
  "When non-nil, hide entries scheduled/deadlined/timestamped in the future.")

(defun reaktor/org-agenda-skip-future ()
  "Skip entry if its scheduled/deadline/timestamp is in the future."
  (let ((end (save-excursion (org-end-of-subtree t))))
    (when (cl-some (lambda (ts) (and ts (> (org-time-stamp-to-now ts) 0)))
                   (list (org-entry-get nil "SCHEDULED")
                         (org-entry-get nil "DEADLINE")
                         (org-entry-get nil "TIMESTAMP")))
      end)))

(defun reaktor/org-agenda-toggle-future ()
  "Toggle hiding of future scheduled/deadline/timestamp entries."
  (interactive)
  (setq reaktor/org-agenda-hide-future (not reaktor/org-agenda-hide-future))
  (if reaktor/org-agenda-hide-future
      (setq org-agenda-skip-function-global #'reaktor/org-agenda-skip-future)
    (setq org-agenda-skip-function-global nil))
  (message (if reaktor/org-agenda-hide-future
               "Hiding future entries"
             "Showing all entries"))
  (org-agenda-redo))

(provide 'reaktor-org-agenda)
;;; reaktor-org-agenda.el ends here
