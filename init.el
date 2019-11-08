;;; package --- init.el

;; Author: Aleksandar Terentić

;;; Commentary:

;; Personal Emacs environment

;;; Code:

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; temporary

(setq user-full-name    "Aleksandar Terentić"
      user-mail-address "aterentic@gmail.com")

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

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

;; mode line settings
(display-time-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; delete the selection with a keypress
(delete-selection-mode t)

;;; enable subword-mode for all programming langs
(add-hook 'prog-mode-hook 'subword-mode)

;;; prettify-symbols-mode
(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode 1)

(desktop-save-mode 1)
(server-mode)

(require 'package)

;; stable picks up only tags from github.com
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

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

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package uuidgen)

(use-package paredit)

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1))

(use-package tidal)

(defvar package-list
  '(company yasnippet flycheck
	      csv-mode js2-mode js2-refactor web-mode json-mode dockerfile-mode
	      pocket-reader ac-js2 prettier-js markdown-mode org-tree-slide
	      clojure-mode elm-mode flycheck-elm cider sonic-pi projectile
	      rainbow-delimiters tagedit magit haskell-mode idris-mode intero
	      go-mode go-rename go-direx go-guru gotest godoctor org-super-agenda
	      company-go yaml-mode powerline exec-path-from-shell
	      nyan-mode zone-nyan zone-sl zone-rainbow pdf-tools htmlize fireplace
	      go-eldoc flycheck-golangci-lint highlight-indentation elpy py-autopep8 dedicated))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; flycheck
(global-flycheck-mode)

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; org
(require 'org)
(require 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; go
(require 'go-mode)
(defun go-mode-setup ()
  (linum-mode 1)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-k") 'godoc)
  (local-set-key (kbd "C-c C-b") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-c") 'compile)
  (setq compile-command "echo Building...; go build -v -o /dev/null; echo Testing...; go test -v; echo Linter...; golint")
  (setq compilation-read-command nil)
  (add-hook 'flycheck-mode-hook 'flycheck-golangci-lint-setup))
(add-hook 'go-mode-hook 'go-mode-setup)
  
;;; elm
(eval-after-load 'flycheck '(flycheck-elm-setup))

;;; haskell
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

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
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(require 'web-mode)
(add-hook 'web-mode-hook (lambda () (setq tab-width 4) (setq web-mode-code-indent-offset 2)))
(add-hook 'web-mode-hook
      (lambda ()
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))
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

;;; company
(require 'company)
(require 'company-go)
(add-to-list 'company-backends 'company-go)
(add-to-list 'company-backends 'company-elm)
(add-hook 'after-init-hook 'global-company-mode)

;;; zone
(require 'zone)
(zone-when-idle 180)
(setq zone-programs
      (vconcat zone-programs [zone-nyan zone-sl zone-rainbow]))

;;; powerline
(require 'powerline)
(require 'nyan-mode)
(powerline-default-theme)
(nyan-mode 1)
(nyan-toggle-wavy-trail)
(nyan-start-animation)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq grep-find-ignored-directories '(".git" "vendor" "node_modules"))

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
    (htmlize pdf-tools zone-rainbow zone-sl zone-nyan nyan-mode color-theme-solarized exec-path-from-shell powerline yaml-mode godoctor gotest go-guru go-direx go-rename go-mode intero idris-mode magit tagedit rainbow-delimiters projectile tidal sonic-pi cider flycheck-elm elm-mode clojure-mode org-tree-slide markdown-mode prettier-js ac-js2 pocket-reader dockerfile-mode json-mode web-mode js2-refactor js2-mode csv-mode flycheck yasnippet company company-go helm paredit uuidgen move-text))))
