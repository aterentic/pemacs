;;; package --- init.el

;; Author: Aleksandar Terentić

;;; Commentary:

;; Personal Emacs environment

;;; Code:

(setq user-full-name    "Aleksandar Terentić"
      user-mail-address "aterentic@gmail.com")

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(toggle-frame-fullscreen)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq large-file-warning-threshold 100000000)

;; scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq grep-find-ignored-directories '(".git" "vendor" "node_modules"))

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
(server-mode)

;;; package archives
(require 'package)

;; stable picks up only tags from github.com
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

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
  (load-theme 'monokai t))

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

;;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(use-package org-tree-slide)


(use-package deft
  :config
  (define-key global-map "\C-c\C-d" 'deft)
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

(use-package pocket-reader)

(use-package sonic-pi)

(use-package pdf-tools)

(use-package yasnippet
  :commands yas-minor-mode)

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1))

(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package projectile)

(use-package lsp-mode
  :commands lsp lsp-deferred
  :hook
  (css-mode . lsp-deferred))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
	lsp-ui-peek-enable t
	lsp-ui-sideline-enable t
	lsp-ui-imenu-enable t
	lsp-ui-flycheck-enable t))

(use-package company-lsp
  :commands company-lsp)

(use-package dockerfile-mode)

(use-package csv-mode)

(use-package yaml-mode)

(use-package markdown-mode)

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

;;; elm
(use-package elm-mode
  :config
  (add-to-list 'company-backends 'company-elm)
  (setq elm-package-json "elm.json")
  :hook
  (elm-mode . lsp-deferred))

(use-package flycheck-elm
 :hook
 (flycheck-mode . flycheck-elm-setup))

;;; haskell
(use-package haskell-mode)
(use-package tidal)

;;; idris
(use-package idris-mode)

;; golang
(use-package go-mode
  :bind
  ("M-," . compile)
  ("M-." . godef-jump)
  :hook
  (go-mode . linum-mode)
  (go-mode . yas-minor-mode)
  (go-mode . lsp-deferred)
  (before-save . (lambda () (lsp-format-buffer) (lsp-organize-imports)))
  :config
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-eldoc-render-all t)
  (setq compile-command "echo Building...; go build -v -o /dev/null; echo Testing...; go test -v; echo Linter...; golint")
  (setq compilation-read-command nil))

;;; javascript
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :hook
  (web-mode . (lambda () (setq tab-width 4) (setq web-mode-code-indent-offset 2))))

(use-package js2-mode
  :config
  (setq js2-highlight-level 3)
  :hook
  (js-mode . (lambda () (setq tab-width 4)))
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

;;; clojure
(use-package clojure-mode)
(use-package cider)

;;; local defaults
(if (file-exists-p "~/.emacs.d/default.el") (load-file "~/.emacs.d/default.el"))

;;; init.el ends here
