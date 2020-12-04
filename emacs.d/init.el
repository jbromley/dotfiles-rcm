;;;; init.el --- Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up the package manager
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;;; Add custom Emacs Lisp directory to the load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

;;; Set large file size limits
(setq large-file-warning-threshold 16000000)

;;; Run the Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General editing configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode t)
(column-number-mode t)
(setq-default fill-column 80)
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;;; Integrate with system clipboard and selection.
(setq select-enable-primary t
      select-enable-clipboard t)

;;; Set the default formats for modes derived from cc-mode.
(setq c-default-style (quote
                       ((c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (java-mode . "java"))))

;;; Always turn on auto-fill.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Fix the prompt for sql-interactive-mode with PostgreSQL.
;;; Old: "^[_[:alpha:]]*[=][#>] ", "^[_[:alpha:]]*[-][#>] "
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (when (string= sql-product "postgres")
	      (setq sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
	      (setq sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] ")))
)
;;; Disable vc
(setq vc-handled-backends nil)

;;; Avoid garbage collection while in the mini-buffer.
(add-hook 'minibuffer-setup-hook
	  (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
	  (lambda () (setq gc-cons-threshold 800000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; which key
(use-package which-key
  :config (which-key-mode))

;;; windmove
(use-package windmove
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t)
  :bind
  (("C-c <right>" . windmove-right)
   ("C-c <left>" . windmove-left)
   ("C-c <up>" . windmove-up)
   ("C-c <down>" . windmove-down)))

;;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	("M-0" . treemacs-select-window)
	("C-x t 1" . treemacs-no-delete-other-windows)
	("C-x t t" . treemacs)
	("C-x t B" . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;;; Completion
(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config (setq company-backends
		'(company-capf company-files
			       (company-dabbrev-code company-gtags company-etags company-keywords company-dabbrev))))

;;; The silver searcher integration
(use-package ag)

;;; Fuzzy finder integration
(use-package fzf)

;;; Smooth scrolling
(use-package smooth-scrolling
  :init (setq smooth-scroll-margin 4)
  :config (smooth-scrolling-mode))

;;; Ivy selection
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers nil
	enable-recursive-minibuffers t
	ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  :bind (("C-c r" . ivy-resume))
  :diminish)

;;; Counsel
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-c z") 'counsel-fzf))

;;; Swiper
(use-package swiper
  :config (global-set-key "\C-s" 'swiper))

;;; Projectile mode for project management
(use-package projectile
  :config (projectile-mode 1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))

;;; Magit mode
(use-package magit
  :bind (("C-x v G" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages and file format packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; LSP mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;;; Org mode
(use-package org-superstar)
(use-package org
  :config
  (defun org-graphics-for-bullets ()
    (if (display-graphic-p)
	(org-superstar-mode 1)))
  (setq org-directory "~/Org"
	org-agenda-files '("~/Org/")
	org-confirm-babel-evaluate nil
	org-agenda-exporter-settings '((ps-print-color-p nil)
				       (org-agenda-add-entry-text-maxlines 0)
				       (htmlize-output-type 'css))
	org-hierarchical-todo-statistics nil
	org-enforce-todo-dependencies t
	org-enforce-todo-checkbox-dependencies t
	org-agenda-dim-blocked-tasks t
	org-log-done t
	org-default-notes-file (concat org-directory "/Notes.org")
	org-catch-invisible-edits 'show)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link))
  :hook ((org-mode . org-graphics-for-bullets)))

;;; Go
(use-package go-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (defun jb/lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook ((go-mode . jb/lsp-go-install-save-hooks)
	 (go-mode . (lambda () (setq-default tab-width 4)))))

(add-hook 'go-mode-hook #'jb/lsp-go-install-save-hooks)

;;; Markdown editing
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; YAML editing
(use-package yaml-mode)

;;; Common Lisp/SLIME
(use-package slime
  :init
  (require 'slime-autoloads)
  :config
  (setq inferior-lisp-program "/home/jay/.asdf/installs/sbcl/2.0.7/bin/sbcl"
	slime-contribs '(slime-fancy)))

;;; Themes and theme switching
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package oceanic-theme
  :defer t)
(use-package modus-operandi-theme
  :defer t)
(use-package modus-vivendi-theme
  :defer t)
(use-package spacemacs-theme
  :defer t)

(use-package theme-looper
  :config
  ;; (theme-looper-set-favorite-themes '(*default* spacemacs-light spacemacs-dark
  ;; 				      modus-operandi modus-vivendi oceanic))
  (global-set-key (kbd "C-{") 'theme-looper-enable-previous-theme)
  (global-set-key (kbd "C-}") 'theme-looper-enable-next-theme))

(load-theme 'spacemacs-dark)
