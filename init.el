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

;; Global keybindings using use-package
(use-package emacs
  :ensure nil
  :bind (("M-o" . other-window)
         ("C-x C-b" . ibuffer)))
;;; enable subword-mode for all programming langs
(add-hook 'prog-mode-hook 'subword-mode)

;;; prettify-symbols-mode
(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode 1)

(delete-selection-mode t)
(desktop-save-mode 1)

;;; package archives
(require 'package)

;; stable picks up only tags from github.com
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; use package (built-in since Emacs 29)
(require 'use-package)
(setq use-package-always-ensure t)

;; Allow package.el to upgrade built-in packages (e.g. transient, which
;; magit/forge need a newer version of than Emacs ships with)
(setq package-install-upgrade-built-in t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Use a separate file for custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Tree-sitter configuration
(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (json "https://github.com/tree-sitter/tree-sitter-json")))

(defun reaktor/install-treesit-grammars ()
  "Install every grammar in `treesit-language-source-alist' that is missing.
Each installation clones and compiles the grammar, so this is a
deliberate command rather than something to do while starting up."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang)))))

;; Remap modes to use tree-sitter (Rust uses rustic-mode which works well as-is)
;; auto-mode-alist names javascript-mode, and the lookup does not follow the
;; alias to js-mode, so the alias needs its own entry
(setq major-mode-remap-alist
      '((go-mode . go-ts-mode)
        (js-mode . js-ts-mode)
        (javascript-mode . js-ts-mode)
        (python-mode . python-ts-mode)
        (json-mode . json-ts-mode)))

;; Enable LSP and formatting for tree-sitter modes
(defun my/setup-go-ts-mode ()
  "Setup for go-ts-mode."
  (lsp-deferred)
  ;; gofmt-before-save only acts in go-mode, and gofmt carries no autoload
  (require 'go-mode)
  (add-hook 'before-save-hook #'gofmt nil t))

(add-hook 'go-ts-mode-hook #'my/setup-go-ts-mode)
(add-hook 'python-ts-mode-hook #'lsp-deferred)
(add-hook 'js-ts-mode-hook #'lsp-deferred)
(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
;; tsx-ts-mode descends from typescript-ts-base-mode, not typescript-ts-mode
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)

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
  :ensure nil
  :demand t
  :config
  (zone-when-idle 180)
  (setq zone-programs
	(vconcat zone-programs [zone-nyan zone-sl zone-rainbow])))

(use-package centered-cursor-mode
  :defer t)

;; Works around zone (Emacs 31, no upstream report): zone--prepare-frames hides
;; the cursor by putting (bar . 0) on every visible frame, but zone saves the
;; original cursor-type of only the frame it started from, so every other frame
;; keeps the zero-width bar until it dies.  The default of t would consult that
;; frame parameter; naming a shape here means buffers never look at it.
;; Remove once zone restores the cursor on every frame it modifies.
(setq-default cursor-type 'box)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package vterm
  :defer t)

(use-package which-key
  :ensure nil
  :config
  (which-key-mode)
  (which-key-setup-side-window-right)
  :custom
  (which-key-idle-delay 0.3))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 100))

(use-package buffer-move
  :defer t)

;;; org-mode

(defvar reaktor/org-base-dir "~/org/"
  "Base directory for Org files.
Set this in early-init-local.el to override it for a machine.")

(defconst reaktor/org-private-dir (expand-file-name "private/" reaktor/org-base-dir)
  "Directory holding private Org files.")
(defconst reaktor/org-shared-dir (expand-file-name "shared/" reaktor/org-base-dir)
  "Directory holding Org files shared across devices.")
(defconst reaktor/org-templates-dir (expand-file-name "templates/" reaktor/org-base-dir)
  "Directory holding Org capture templates.")

(require 'reaktor-imdb)

(setq org-default-notes-file (expand-file-name "inbox.org" reaktor/org-private-dir))

;; Org capture templates
(setq org-capture-templates
      `(("t" "Task" entry
         (file org-default-notes-file)
         "* TODO %?\n%U\n%a"
         :empty-lines 1)
        ("v" "Vacation/Trip" entry
         (file+headline ,(expand-file-name "putovanja.org" reaktor/org-shared-dir) "2026")
         (file ,(expand-file-name "vacation.org" reaktor/org-templates-dir))
         :empty-lines 1)
        ("e" "Excursion/Day Trip" entry
         (file+headline ,(expand-file-name "putovanja.org" reaktor/org-shared-dir) "Izlet")
         (file ,(expand-file-name "excursion.org" reaktor/org-templates-dir))
         :empty-lines 1)
        ("m" "Movie (IMDB)" entry
         (file+headline ,(expand-file-name "culture.org" reaktor/org-shared-dir) "Movies")
         (file ,(expand-file-name "movie.org" reaktor/org-templates-dir))
         :empty-lines 1)))

;; Org tag list
(setq org-tag-persistent-alist '(
		      ("alarm" . ?a)
		      ("kids" . ?k)
              ("project" . ?p)
		      ("reminder" . ?r)
		      ("review" . ?v)
		      ("shopping" . ?b)
		      ("supplies" . ?s)
              ("travel" . ?t)))

(declare-function reaktor/org-agenda-reload-from-disk "reaktor-org-agenda")
(declare-function reaktor/org-agenda-toggle-future "reaktor-org-agenda")

(use-package org-agenda
  :ensure nil
  :defer t
  :init
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-files
        (append
         (directory-files reaktor/org-private-dir t "\\.org$")
         (directory-files reaktor/org-shared-dir t "\\.org$")))
  ;; Deferred items belong on the calendar, not in the TODO lists
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-todo-ignore-scheduled 'future)
  (setq org-agenda-todo-ignore-deadlines 'future)
  (setq org-agenda-custom-commands
	'(("d" "Daily"
	   ((agenda "" ((org-agenda-span 'day)
			(org-agenda-skip-function
			 '(let ((tags (org-get-tags)))
			    (when (or (member "reminder" tags)
				      (member "plan" tags)
				      (member "project" tags))
			      (org-end-of-subtree t))))))
	    (agenda "" ((org-agenda-span 'day)
			(org-agenda-skip-function
			 '(let ((tags (org-get-tags)))
			    (unless (member "reminder" tags)
			      (org-end-of-subtree t))))
			(org-agenda-overriding-header "Reminders")))
	    (agenda "" ((org-agenda-span 'day)
			(org-agenda-skip-function
			 '(let ((tags (org-get-tags)))
			    (unless (or (member "plan" tags)
					(member "project" tags))
			      (org-end-of-subtree t))))
			(org-agenda-overriding-header "Plans & Projects")))))
	  ("ct" "TODO" tags-todo "TODO=\"TODO\"-job-nabavka-reading-kupovina")
	  ("cr" "Reading" tags-todo "TODO=\"TODO\"+reading")
	  ("s" . "Shopping")
	  ("ss" "Shopping" tags-todo "TODO=\"TODO\"-nabavka+kupovina-review")
	  ("sr" "Shopping (review)" tags-todo "TODO=\"TODO\"+kupovina+review")
	  ("sk" "Shopping (restock)" tags-todo "TODO=\"NEED\"|TODO=\"TODO\"+nabavka+kupovina")
	  ("t" . "Travel")
	  ("tt" "Travel" tags-todo "+putovanje+TODO={TODO\\|PACK}")
	  ("tp" "Packing" tags-todo "TODO=\"PACK\"")
	  ("r" "Reminders"
	   ((agenda "" ((org-agenda-span 'day))))
           ((org-agenda-tag-filter-preset '("+reminder"))))
	  ("p" "Projects" tags-todo "TODO=\"TODO\"+project")
	  ("w" "Watch" tags-todo "TODO=\"TODO\"+watch")))
  :config
  (require 'reaktor-org-agenda)
  (define-key org-agenda-mode-map (kbd "C-c g") #'reaktor/org-agenda-reload-from-disk)
  (define-key org-agenda-mode-map (kbd "C-c f") #'reaktor/org-agenda-toggle-future))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  ;; :set installs the blocker hook, so this must not be a plain setq
  :custom
  (org-enforce-todo-dependencies t)
  (org-link-elisp-skip-confirm-regexp "^(org-agenda ")
  (org-startup-indented t)
  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-log-into-drawer t))

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 3)))

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-timestamp nil))

;; Not on MELPA; keeps block backgrounds aligned under org-indent's
;; virtual indentation, which org-modern alone does not handle.  Pinned to a
;; reviewed tag so upstream cannot land code that runs here unread.
(use-package org-modern-indent
  :vc (:url "https://github.com/jdtsmith/org-modern-indent" :rev "v0.5.3")
  :hook (org-indent-mode . org-modern-indent-mode))

(use-package org-crypt
  :ensure nil
  :after org
  :config
  (setq epa-pinentry-mode 'loopback)    ;; prompt in Emacs
  (setq org-crypt-tag-matcher "crypt")
  (add-to-list 'org-tags-exclude-from-inheritance "crypt")
  (setq org-crypt-key nil)             ;; symmetric (passphrase)
  (org-crypt-use-before-save-magic))   ;; encrypt on save

(use-package org-habit
  :after org
  :ensure nil
  :config
  (setq org-habit-show-habits t
        org-habit-graph-column 50))

;;; appt - appointment notifications
(use-package alert
  :custom
  (alert-default-style 'osx-notifier))

(use-package appt
  :ensure nil
  :after (org alert)
  :custom
  (appt-message-warning-time 15)
  (appt-display-interval 5)
  :config
  (setq appt-disp-window-function
        (lambda (min-to-appt _new-time msg)
          (let ((msgs (if (listp msg) msg (list msg))))
            (dolist (m msgs)
              (alert m :title (format "In %s min" min-to-appt))))))
  (setq appt-delete-window-function #'ignore)
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'after-save-hook #'org-agenda-to-appt nil t)))
  (appt-activate 1)
  (org-agenda-to-appt))

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
  :init
  (setq deft-directory reaktor/org-base-dir)
  (setq deft-recursive-ignore-dir-regexp
        (concat "\\(?:" "\\." "\\|\\.\\." "\\|.stversions" "\\)$"))
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
   ("M-s a" . consult-org-agenda)       ; Org agenda
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

;;; Consult-dir - directory navigation with Vertico
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; Consult-projectile bridge - projectile integration with Consult
(use-package consult-projectile
  :bind (("C-c p p" . consult-projectile-switch-project)
         ("C-c p f" . consult-projectile-find-file)
         ("C-c p d" . consult-projectile-find-dir)))

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
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion)
  ;; Disable copilot in special buffers
  (add-to-list 'copilot-disable-predicates
               (lambda () (string-match-p "^\\*" (buffer-name)))))

(use-package magit
  :defer t
  :bind (:map magit-diff-mode-map
        ("o" . magit-diff-visit-worktree-file-other-window)))

(use-package forge
  :after magit)

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package git-link
  :defer t)

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
  ;; lsp-mode carries no variable for this; gopls reads it from its settings
  (lsp-register-custom-settings '(("gopls.staticcheck" t t)))
  :custom
  (lsp-completion-provider :none)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
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

(use-package dockerfile-mode
  :defer t)

(use-package feature-mode
  :defer t)

(use-package csv-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-command "pandoc -f gfm -t html5")
  (let ((res-dir (expand-file-name "resources" user-emacs-directory)))
    (setq markdown-css-paths
          (list (concat "file://" (expand-file-name "github-markdown.css" res-dir))
                (concat "file://" (expand-file-name "syntax-highlight.css" res-dir))
                (concat "file://" (expand-file-name "markdown-preview.css" res-dir))))
    (setq markdown-xhtml-header-content
          (format "<script src=\"file://%s\"></script>"
                  (expand-file-name "markdown-preview.js" res-dir)))
    (setq markdown-xhtml-body-preamble "<div class=\"markdown-body\">"
          markdown-xhtml-body-epilogue "</div>")))

(use-package json-mode
  :defer t
  :mode "\\.json\\'")

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
  :hook
  (rustic-mode . lsp-deferred)
  :config
  (setq rustic-format-display-method 'ignore))

;; golang
(use-package go-mode
  :defer t)

(use-package gotest
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c t f" . go-test-current-file)
              ("C-c t t" . go-test-current-test)
              ("C-c t p" . go-test-current-project)))

;;; javascript
(defun reaktor/web-mode-setup ()
  "Indentation for `web-mode' buffers."
  (setq-local tab-width 4)
  (setq-local web-mode-code-indent-offset 2))

(use-package web-mode
  :defer t
  :hook (web-mode . reaktor/web-mode-setup))

(use-package prettier-js
  :hook
  (js-ts-mode . prettier-js-mode)
  (web-mode . prettier-js-mode))

;;; typescript
(use-package tide
  :defer t)

;;; python
(use-package python
  :ensure nil
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

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enable the mode
  (global-ligature-mode t))

;;; Load machine-specific settings if they exist
(let ((local-init (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p local-init)
    (load local-init)))

;;; init.el ends here
