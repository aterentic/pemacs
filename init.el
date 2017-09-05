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
(defvar packages '(move-text
		   uuidgen
		   paredit
		   helm
		   auto-complete
		   company
		   yasnippet
		   flycheck
		   org-present
		   js2-mode
		   js2-refactor
                   web-mode
		   json-mode
		   ac-js2
		   prettier-js
		   markdown-mode
		   clojure-mode
		   elm-mode
		   cider
		   projectile
		   rainbow-delimiters
		   tagedit
		   magit
		   haskell-mode
		   idris-mode
		   intero
		   go-mode
		   go-rename
		   go-autocomplete
                   go-direx
		   go-guru
		   gotest
		   company-go
		   yaml-mode
		   powerline
		   monokai-theme
		   exec-path-from-shell
		   color-theme-solarized
		   nyan-mode
		   zone-nyan
		   zone-sl
		   zone-rainbow
		   pdf-tools))

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

(add-hook 'json-mode-hook (lambda () (js2-minor-mode-exit)))

(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)

(require 'js2-refactor)
(add-hook 'js-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(add-hook 'web-mode-hook (lambda ()
			   (setq tab-width 4)
			   (setq web-mode-code-indent-offset 2)))
(add-hook 'web-mode-hook
      (lambda ()
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))

(setq js2-highlight-level 3)

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

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
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor" "node_modules")))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(ns-command-modifier (quote meta))
 '(ns-right-command-modifier (quote super))
 '(package-selected-packages
   (quote
    (zone-sl zone-nyan zone-rainbow go-direx uuidgen erc highlight-tail zone-matrix tagedit rainbow-delimiters powerline paredit nyan-mode move-text json-mode js2-refactor gotest go-rename go-autocomplete exec-path-from-shell company-go color-theme-solarized ac-js2))))

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

(require 'zone)
(zone-when-idle 60)

;;; tidal
(add-to-list 'load-path "~/.emacs.d/tidal")
(require 'haskell-mode)
(require 'tidal)

;;; org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(if (file-exists-p "~/.emacs.d/default.el")
    (load-file "~/.emacs.d/default.el"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
