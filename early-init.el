;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before package.el and the GUI is initialized.
;; Use it for performance optimizations and early setup.

;;; Code:

;; GC optimization for startup - maximize threshold during init
(setq gc-cons-threshold most-positive-fixnum)

;; Reset GC to a reasonable value after startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 2 1024 1024)))) ; Reset to 2MB

;; Load local machine-specific settings if they exist
(let ((local-early-init (expand-file-name "early-init-local.el" user-emacs-directory)))
  (when (file-exists-p local-early-init)
    (load local-early-init)))

;;; early-init.el ends here
