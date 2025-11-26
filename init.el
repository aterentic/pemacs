;;; package --- init.el -*- lexical-binding: t -*-

;; Author: Aleksandar Terentić

;;; Commentary:

;; Personal Emacs environment

;;; Code:

(setq user-full-name    "Aleksandar Terentić"
      user-mail-address "aterentic@pm.me")

(setq large-file-warning-threshold 100000000)

;; enable y/n answers
(setq use-short-answers t)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; enable subword-mode for all programming langs
(add-hook 'prog-mode-hook 'subword-mode)

;;; prettify-symbols-mode
(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode 1)

(delete-selection-mode t)
(desktop-save-mode 1)
;;; (server-mode)

;;; (setq require-final-newline t)

;;; package archives
(require 'package)

;; stable picks up only tags from github.com
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; Use a separate file for custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ;;; themes: doom-themes, solarized-theme, zenburn-theme
(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t))

;;; modeline

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

(use-package time
  :ensure nil
  :config
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-default-load-average nil)
  (display-time-mode 1))

;;; moody - beautiful ribbon-style modeline
(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;; minions - hide minor modes in a menu
(use-package minions
  :config
  (minions-mode 1))

;;; nyan-mode - nyan cat in the modeline
(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail nil
        nyan-minimum-window-width 40)
  :config
  (nyan-mode 1))

;;; zone
(use-package zone-nyan
  :defer t)
(use-package zone-sl
  :defer t)
(use-package zone-rainbow
  :defer t)
(use-package zone
  :defer t
  :config
  (zone-when-idle 180)
  (setq zone-programs
	(vconcat zone-programs [zone-nyan zone-sl zone-rainbow])))

(use-package centered-cursor-mode
  :defer t)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package buffer-move
  :defer t)

;;; org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (defun reaktor/org-agenda-reload-from-disk ()
    "Kill all Org buffers and reload agenda from disk."
    (interactive)
    (dolist (buf (org-buffer-list))
      (kill-buffer buf))
    (org-agenda-list))
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "G") #'reaktor/org-agenda-reload-from-disk)))

(use-package org-tree-slide)
(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))
;;; (setq org-log-done t)
(setq org-tag-alist '(
		      ("alarm" . ?a)
		      ("kupovina" . ?k)
		      ("ikea" . ?i)
		      ("kids" . ?c)
		      ("nabavka" . ?n)
		      ("plan" . ?p)
		      ("reminder" . ?r)
		      ("review" . ?v)
		      ("leto" . ?l)
		      ("zima" . ?z)))
(setq org-agenda-custom-commands
      '(("ct" "TODO" tags-todo "TODO=\"TODO\"-job-nabavka-reading-kupovina"
	 ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
	("ckk" "Kupovina" tags-todo "TODO=\"TODO\"-nabavka+kupovina-review")
	("ckr" "Kupovina (pregled)" tags-todo "TODO=\"TODO\"+kupovina+review")
	("ckn" "Kupovina (nabavka)" tags-todo "TODO=\"TODO\"+nabavka+kupovina-review")
	("cp" "Pakovanje" tags-todo "TODO=\"PACK\"")
	("cr" "Reading" tags-todo "TODO=\"TODO\"+reading")
	("p" "Putovanje TODO/Pack" tags-todo "+putovanje+TODO={TODO\\|PACK}")
	("r" "Reminders"
	 ((agenda "" ((org-agenda-span 'day))))
         ((org-agenda-tag-filter '("+reminder"))))
	("j" "Job" tags-todo "TODO=\"TODO\"+job")))

(use-package wgrep
  :defer t)

;;; Calendar configuration for Belgrade, Serbia
(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-latitude 44.8167)
  (calendar-longitude 20.4667)
  (calendar-location-name "Belgrade, Serbia")
  (calendar-week-start-day 1) ; Week starts on Monday (European standard)
  (calendar-date-style 'european)
  (calendar-time-zone 60)  ; CET (UTC+1)
  (calendar-standard-time-zone-name "CET")
  (calendar-daylight-time-zone-name "CEST")

  ;; Show only Serbian holidays (disable default US/Christian holidays)
  (holiday-general-holidays nil)  ; Disable general holidays
  (holiday-christian-holidays nil)  ; Disable Christian holidays
  (holiday-hebrew-holidays nil)  ; Disable Hebrew holidays
  (holiday-islamic-holidays nil)  ; Disable Islamic holidays
  (holiday-bahai-holidays nil)  ; Disable Bahai holidays
  (holiday-oriental-holidays nil)  ; Disable Oriental holidays
  (holiday-solar-holidays nil)  ; Disable solar holidays

  ;; Serbian holidays only
  (holiday-local-holidays
   '((holiday-fixed 1 1 "Nova Godina")
     (holiday-fixed 1 2 "Nova Godina (drugi dan)")
     (holiday-fixed 1 7 "Božić (pravoslavni)")
     (holiday-fixed 2 15 "Dan državnosti Srbije")
     (holiday-fixed 2 16 "Dan državnosti Srbije (drugi dan)")
     (holiday-easter-etc -2 "Veliki Petak (pravoslavni)")
     (holiday-easter-etc 0 "Vaskrs (pravoslavni)")
     (holiday-easter-etc 1 "Vaskršnji Ponedeljak (pravoslavni)")
     (holiday-fixed 5 1 "Praznik rada")
     (holiday-fixed 5 2 "Praznik rada (drugi dan)")
     (holiday-fixed 11 11 "Dan primirja u Prvom svetskom ratu")))
  ;; Show holidays
  (calendar-mark-holidays-flag t)
  :config
  ;; Mark today
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(use-package deft
  :defer t
  :config
  (setq deft-extensions '("org"))
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-recursive t))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package highlight-indentation
  :defer t)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package fireplace
  :defer t)

(use-package paredit
  :defer t)

(use-package uuidgen
  :defer t)

(use-package tagedit
  :defer t)

(use-package htmlize
  :defer t)

(use-package dedicated
  :defer t)

(use-package pdf-tools
  :defer t)

;;; Vertico - vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

;;; Orderless - flexible completion matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Consult - enhanced commands with preview
(use-package consult
  :bind
  (("C-s" . consult-line)           ; Search in buffer
   ("C-x b" . consult-buffer)       ; Enhanced buffer switching
   ("C-x r b" . consult-bookmark)   ; Bookmarks
   ("M-g g" . consult-goto-line)    ; Go to line
   ("M-g M-g" . consult-goto-line)
   ("M-s r" . consult-ripgrep)      ; Ripgrep search
   ("M-s f" . consult-find)         ; Find files
   ("M-s g" . consult-grep)         ; Grep
   ("M-y" . consult-yank-pop))      ; Enhanced yank-pop
  :config
  ;; Use Consult for xref
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; Marginalia - rich annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;;; Embark - contextual actions
(use-package embark
  :bind
  (("C-." . embark-act)             ; contextual actions
   ("C-;" . embark-dwim)            ; "do what I mean"
   ("C-h B" . embark-bindings))     ; describe bindings
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; Embark-Consult integration
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Corfu - in-buffer completion popup
(use-package corfu
  :init
  (global-corfu-mode))

;;; Cape - completion-at-point extensions
(use-package cape
  :init
  ;; Add completion backends
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(setq-default indent-tabs-mode nil
		      tab-width 4)

(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)
  :config
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion))

(use-package magit
  :defer t
  :bind (:map magit-diff-mode-map
        ("o" . magit-diff-visit-worktree-file-other-window)))

(setopt ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-package projectile
  :defer t)

(use-package lsp-mode
  :commands lsp
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-ui-mode))
  :config
  (setq lsp-file-watch-threshold 20000)
  ;; Disable semgrep-ls (static analysis tool)
  (setq lsp-disabled-clients '(semgrep-ls))
  :custom
  (lsp-completion-provider :capf)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-cargo-all-targets t)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-response-timeout 180)
  (lsp-warn-no-matched-clients nil))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))

(use-package consult-lsp
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ("C-c l s" . consult-lsp-symbols)
              ("C-c l d" . consult-lsp-diagnostics)))

(use-package dockerfile-mode)

(use-package feature-mode)

(use-package csv-mode)

(use-package yaml-mode
  :defer t)

(use-package markdown-mode
  :defer t)

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

(use-package css-mode
  :hook
  (css-mode . lsp-deferred))

;;; elm
(use-package elm-mode
  :hook
  (elm-mode . lsp-deferred)
  (elm-mode . (lambda () (setq-local indent-tabs-mode nil))))

;;; haskell
(use-package haskell-mode
  :defer t)
(use-package tidal
  :defer t)

;;; idris
(use-package idris-mode
  :defer t)

(use-package rustic
  :ensure
  :config
  (setq rustic-format-display-method 'ignore)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-trigger 'on-save)
  )

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
(use-package gotest
  :defer t)

;;; javascript
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
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

;;; typescript
(use-package tide
  :defer t)

;;; python
(use-package python
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :config
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "-i"))

;; ;;; clojure
(use-package clojure-mode
  :defer t)
(use-package cider
  :defer t)

(with-eval-after-load 'grep
  (add-to-list 'grep-find-ignored-directories "node_modules"))

;;; local defaults
(if (file-exists-p "~/.emacs.d/default.el") (load-file "~/.emacs.d/default.el"))

(use-package ligature
  :defer t)

;;; init.el ends here
