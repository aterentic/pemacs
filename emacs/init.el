(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("tromey" . "http://tromey.com/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;; install required packages
(defvar packages '(neotree
		   paredit
		   auto-complete
		   yasnippet
		   js2-mode
		   js2-refactor
		   ac-js2
		   markdown-mode
		   clojure-mode
		   cider
		   projectile
		   rainbow-delimiters
		   tagedit
		   magit
		   haskell-mode
		   monokai-theme
		   color-theme-solarized))

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-hook 'js-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(setq js2-highlight-level 3)

;;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

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

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq user-full-name "Aleksandar Terentic")
(setq user-mail-address "aterentic@gmail.com")

(toggle-frame-fullscreen)
