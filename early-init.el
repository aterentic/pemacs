;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before package.el and the GUI is initialized.
;; Use it for performance optimizations and early setup.

;;; Code:

;; UI tweaks - disable before GUI starts to prevent flashing
(setq inhibit-startup-screen t)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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
