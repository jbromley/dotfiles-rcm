;;; init.el --- Emacs custom configuration

;;; Commentary:

;;; Code:

(load "server")
(unless (server-running-p) (server-start))

;; Set up Emacs package manager.
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Set up load paths and other paths.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))
(defvar local-bin (concat (file-name-as-directory (getenv "HOME"))
			  ".local/bin"))
(setq exec-path (append (list local-bin) exec-path))

;; Basic editing configuration
(show-paren-mode t)
(column-number-mode t)
(windmove-default-keybindings)
(setq-default fill-column 80)

;; Smooth scrolling
(use-package smooth-scrolling
  :init (setq smooth-scroll-margin 4)
  :config (smooth-scrolling-mode))

;; Flycheck
(use-package flycheck
  :init (progn
	  (setq flycheck-python-flake8-executable
		(concat (file-name-as-directory local-bin) "flake8")
		flycheck-c/c++-clang-executable "/usr/bin/clang"))
  :config (global-flycheck-mode)
  :diminish flycheck-mode)

;; Company mode
(use-package company
  :bind (("C-." . company-complete))
  :config (global-company-mode))

;; JEDI for Python autocompletion
(use-package jedi
  :init (progn
	  (setq jedi:complete-on-dot t)
	  (defun jb/python-mode-hook ()
	    (add-to-list 'company-backends 'company-jedi)
	    (jedi:setup)))
  :config (progn
	    (add-hook 'python-mode-hook 'jb/python-mode-hook)))

;; ;; YCMD
;; (use-package ycmd
;;   :init (progn
;; 	  (set-variable 'ycmd-server-command '("/usr/bin/ycmd"))
;; 	  (require 'ycmd-next-error))
;;   :config (global-ycmd-mode))

;; (use-package company-ycmd
;;   :config (company-ycmd-setup))

;; Markdown mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; YAML mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode)))

;; Fill Column Indicator
;; (use-package fill-column-indicator
;;   :init (setq fci-rule-use-dashes t
;; 	      fci-dash-pattern 0.25
;; 	      fci-rule-color "red")
;;   :config (progn
;; 	    (define-globalized-minor-mode global-fci-mode fci-mode
;; 	      (lambda () (fci-mode 1)))
;; 	    (global-fci-mode 1)))

;; Helm
(require 'setup-helm)

;; Magit
(use-package magit)
  
;; Themes
;(require 'setup-themes)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c-mode . "k&r")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(custom-safe-themes
   (quote
    ("c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "3d5307e5d6eb221ce17b0c952aa4cf65dbb3fa4a360e12a71e03aab78e0176c5" "3cddc1775f6c26573a69315dacd5fd45a6cd04df539b6354281d316985f254f3" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (column-marker company-ycmd ycmd dockerfile-mode company-jedi jedi ubuntu-theme eziam-theme monochrome-theme quasi-monochrome-theme markdown-mode use-package hydandata-light-theme flycheck-python flycheck helm-swoop helm yaml-mode cmake-mode yasnippet smooth-scrolling color-theme-sanityinc-tomorrow magit solarized-theme)))
 '(semantic-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)

;;; init.el ends here
