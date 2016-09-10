(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("tromey" . "http://tromey.com/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;; install required packages
(defvar packages '(neotree
		   move-text
		   paredit
		   helm
		   auto-complete
		   company
		   yasnippet
		   flycheck
		   js2-mode
		   js2-refactor
		   json-mode
		   ac-js2
		   markdown-mode
		   clojure-mode
		   elm-mode
		   cider
		   projectile
		   rainbow-delimiters
		   tagedit
		   magit
		   haskell-mode
		   intero
		   go-mode
		   go-rename
		   go-autocomplete
		   gotest
		   company-go
		   powerline
		   monokai-theme
		   exec-path-from-shell
		   color-theme-solarized
		   nyan-mode))

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook (lambda () (js2-minor-mode-exit)))

(add-hook 'js-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(setq js2-highlight-level 3)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;; neotree
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-width 40)

;;; golang
(require 'go-autocomplete)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-k") 'godoc)))

;;; haskell
(add-hook 'haskell-mode-hook 'intero-mode)

(global-flycheck-mode)

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq user-full-name "Aleksandar Terentic")
(setq user-mail-address "aterentic@gmail.com")

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

(toggle-frame-fullscreen)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(ns-command-modifier (quote meta))
 '(ns-right-command-modifier (quote super)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(move-text-default-bindings)

;;; powerline
(require 'powerline)
(setq powerline-arrow-shape 'arrow)
(powerline-default-theme)
(nyan-mode 1)
(nyan-toggle-wavy-trail)
(nyan-start-animation)

;;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

(if (file-exists-p "~/.emacs.d/default.el")
    (load-file "~/.emacs.d/default.el"))
