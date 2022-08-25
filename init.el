;;; package --- init.el

;; Author: Aleksandar Terentić

;;; Commentary:

;; Personal Emacs environment

;;; Code:

(setq user-full-name    "Aleksandar Terentić"
      user-mail-address "aterentic@pm.me")

(setq inhibit-startup-screen t)
(setq visible-bell t)
;;; (setq ring-bell-function 'ignore)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq large-file-warning-threshold 100000000)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; enable subword-mode for all programming langs
(add-hook 'prog-mode-hook 'subword-mode)

;;; prettify-symbols-mode
(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode 1)

(delete-selection-mode t)
(desktop-save-mode 1)
;;; (server-mode)

(setq require-final-newline t)

;;; package archives
(require 'package)

;; stable picks up only tags from github.com
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; themes: solarized-theme, material-theme, monokai-theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t)
  (set-face-background 'default "#1f201b"))

;;; modeline
(use-package powerline
  :config
  (powerline-default-theme)
  (display-time-mode t)
  (setq display-time-day-and-date t)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t))
(use-package nyan-mode
  :config
  (nyan-mode 1)
  (nyan-toggle-wavy-trail)
  (nyan-start-animation))

;;; zone
(use-package zone-nyan)
(use-package zone-sl)
(use-package zone-rainbow)
(use-package zone
  :config
  (zone-when-idle 180)
  (setq zone-programs
      (vconcat zone-programs [zone-nyan zone-sl zone-rainbow])))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package exec-path-from-shell)
  ;; :config
  ;; (when (memq window-system '(mac ns x))
  ;;   (exec-path-from-shell-initialize)))

;;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(use-package org-tree-slide)
(use-package org-superstar
  :config
  (org-superstar-configure-like-org-bullets)
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))
(setq org-agenda-custom-commands
      '(("ct" tags-todo "TODO=\"TODO\"-job-nabavka"
	 ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
	("ck" tags-todo "TODO=\"TODO\"-nabavka+kupovina")
	("cp" tags-todo "TODO=\"PACK\"")
	("cn" tags-todo "TODO=\"TODO\"+nabavka+kupovina")
	("cr" tags-todo "TODO=\"TODO\"+reading")
	("cj" tags-todo "TODO=\"TODO\"+job")))

;;; naive way of alerting on elapsed timer
(setq org-show-notification-handler
      '(lambda (notification)
	 (let ((flash-sec 1.0))
           (invert-face 'mode-line)
	   (run-with-timer flash-sec nil #'invert-face 'mode-line)
	   (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
	   (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)
	   (run-with-timer (* 4 flash-sec) nil #'invert-face 'mode-line)
	   (run-with-timer (* 5 flash-sec) nil #'invert-face 'mode-line))))

(use-package wgrep)

(use-package deft
  :config
  (setq deft-extensions '("org"))
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-recursive t))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package highlight-indentation)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package fireplace)

(use-package paredit)

(use-package uuidgen)

(use-package tagedit)

(use-package htmlize)

(use-package dedicated)

(use-package pdf-tools)

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :custom
  (company-idle-delay 0.25) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package projectile)

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :config
  ;; (setq lsp-file-watch-threshold 20000)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-response-timeout 30)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :hook
  (css-mode . lsp-deferred))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(use-package dockerfile-mode)

(use-package csv-mode)

(use-package yaml-mode)

(use-package markdown-mode)

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

;;; elm
(use-package elm-mode
  :hook
  (elm-mode . lsp-deferred)
  (elm-mode . (lambda () (setq-local indent-tabs-mode nil))))

(use-package flycheck-elm
 :hook
 (flycheck-mode . flycheck-elm-setup))

;;; haskell
(use-package haskell-mode)
(use-package tidal)

;;; idris
(use-package idris-mode)

;;; rust
;; (use-package rustic)
;;   ;; :config
;;   ;; (setq rustic-format-on-save t))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  (setq lsp-rust-analyzer-server-display-inlay-hints nil)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; golang
(use-package go-mode
  :bind
  ;; TODO, local: ("M-," . compile)
  ;; TODO, local: ("M-." . godef-jump)
  :hook
  (go-mode . lsp-deferred)
  ;; TODO, make it go-mode local: (before-save . (lambda () (lsp-format-buffer) (lsp-organize-imports)))
  :config
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-eldoc-render-all t)
  (setq compile-command "echo Building...; go build -v -o /dev/null; echo Testing...; go test -v; echo Linter...; golint")
  (setq compilation-read-command nil))
(use-package gotest)

;;; javascript
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :hook
  (web-mode . (lambda () (setq-local tab-width 4) (setq web-mode-code-indent-offset 2))))

(use-package js2-mode
  :config
  (setq js2-highlight-level 3)
  :hook
  (js-mode . (lambda () (setq-local tab-width 4)))
  (js-mode . js2-minor-mode))
(use-package js2-refactor
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  :hook
  (js2-mode . js2-refactor-mode))
(use-package prettier-js
  :hook
  (js2-mode . prettier-js-mode)
  (web-mode . prettier-js-mode))
(use-package ac-js2
  :hook
  (js2-mode . ac-js2-mode))

;;; typescript

(use-package tide)

;;; python
(use-package elpy
  :config
  (elpy-enable)
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setq python-shell-interpreter "ipython3" python-shell-interpreter-args "-i")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-test-discover-runner-command '("python3" "-m" "unittest"))
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package py-autopep8
  :hook
  (elpy-mode . py-autopep8-enable-on-save))

;; ;;; clojure
;; (use-package clojure-mode)
;; (use-package cider)


(setq grep-find-ignored-directories (append grep-find-ignored-directories '("node_modules")))

;;; local defaults
(if (file-exists-p "~/.emacs.d/default.el") (load-file "~/.emacs.d/default.el"))

