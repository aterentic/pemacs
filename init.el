;;; package --- init.el

;; Author: Aleksandar Terentić

;;; Commentary:

;; Personal Emacs environment

;;; Code:

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; temporary

(setq user-full-name    "Aleksandar Terentić"
      user-mail-address "aterentic@gmail.com")

;; disable startup screen
(setq inhibit-startup-screen t)

(toggle-frame-fullscreen)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq large-file-warning-threshold 100000000)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq grep-find-ignored-directories '(".git" "vendor" "node_modules"))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

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
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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

;;; themes: color-theme-solarized, material-theme

(use-package material-theme
  :config
  (load-theme 'material t))

(use-package fireplace)

;;; modeline
(use-package powerline
  :config
  (powerline-default-theme)
  (display-time-mode t)
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

;;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(use-package org-tree-slide)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package exec-path-from-shell)

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package highlight-indentation)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package uuidgen)

(use-package paredit)

(use-package csv-mode)

(use-package yaml-mode)

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

(use-package dockerfile-mode)

(use-package markdown-mode)

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

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package company
  :config
  (global-company-mode))

;; golang
(use-package go-mode
  :bind (:map go-mode-map
	      ("M-." . godef-jump)
	      ("C-c C-k" . godoc)
	      ("C-c C-b" . pop-tag-mark)
	      ("C-c C-c" . compile))
  :hook
  (go-mode . linum-mode)
  (before-save . gofmt-before-save)
  :config
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (setq compile-command "echo Building...; go build -v -o /dev/null; echo Testing...; go test -v; echo Linter...; golint")
  (setq compilation-read-command nil))
(use-package go-rename)
(use-package go-direx)
(use-package go-guru)
(use-package gotest)
(use-package godoctor)
(use-package go-eldoc)
(use-package flycheck-golangci-lint
  :hook
  (flycheck-mode . flycheck-golangci-lint-setup))
(use-package company-go
  :config
  (add-to-list 'company-backends 'company-go))

;;; elm
(use-package elm-mode)
(use-package flycheck-elm
  :hook
  (flycheck-mode . flycheck-elm-setup))

;;; haskell
(use-package haskell-mode)
(use-package intero
  :hook
  (haskell-mode . intero-mode))

(use-package tidal)

;;; idris
(use-package idris-mode)

(use-package pocket-reader)

(use-package sonic-pi)

(use-package pdf-tools)

(defvar package-list
  '(js2-mode js2-refactor web-mode
	     ac-js2 prettier-js 
	     clojure-mode cider projectile
	     tagedit 
	     htmlize
	     elpy py-autopep8 dedicated))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; javascript
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(require 'js2-refactor)
(add-hook 'js-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")
(setq js2-highlight-level 3)

(require 'web-mode)
(add-hook 'web-mode-hook (lambda () (setq tab-width 4) (setq web-mode-code-indent-offset 2)))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;; python
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq elpy-test-discover-runner-command '("python3" "-m" "unittest"))
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython3" python-shell-interpreter-args "-i")
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(add-to-list 'company-backends 'company-elm)


(if (file-exists-p "~/.emacs.d/default.el") (load-file "~/.emacs.d/default.el"))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(package-selected-packages
   (quote
    (htmlize pdf-tools zone-rainbow zone-sl zone-nyan nyan-mode color-theme-solarized exec-path-from-shell powerline yaml-mode godoctor gotest go-guru go-direx go-rename go-mode intero idris-mode magit tagedit rainbow-delimiters projectile tidal sonic-pi cider flycheck-elm elm-mode clojure-mode org-tree-slide markdown-mode prettier-js ac-js2 pocket-reader dockerfile-mode json-mode web-mode js2-refactor js2-mode csv-mode flycheck yasnippet company company-go helm paredit uuidgen move-text)))
 '(safe-local-variable-values
   (quote
    ((elm-package-json . "elm.json")
     (elm-compile-arguments "--output=elm.js" "--debug")
     (elm-reactor-arguments "--port" "8000")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
