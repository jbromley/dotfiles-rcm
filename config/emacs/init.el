;;;; init.el --- Emacs configuration

;;; Code;

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
(setq-default fill-column 80
	      indent-tabs-mode nil)

(setq custom-file (expand-file-name "~/.config/emacs/custom.el"))
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

;;; initialize the path
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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

;;; All the icons
(use-package all-the-icons)
(use-package all-the-icons-dired)

;;; Treemacs
(use-package treemacs
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
  :commands (treemacs)
  :after (lsp-mode)
  :bind
  (:map global-map
	("M-0" . treemacs-select-window)
	("C-x t 1" . treemacs-no-delete-other-windows)
	("C-x t t" . treemacs)
	("C-x t B" . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired)

(use-package treemacs-magit
  :after treemacs magit)


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

;;; Snippets
(use-package java-snippets)

;;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; Emacs Debug Adapter Protocol
(use-package dap-mode
  :after (lsp-mode)
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

;;; Treemacs for LSP
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))

;;; LSP mode
(use-package lsp-ui
:after (lsp-mode)
:bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
:init (setq lsp-ui-doc-delay 1.5
      lsp-ui-doc-position 'bottom
	  lsp-ui-doc-max-width 100))

(use-package lsp-mode
  :config
    (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
    (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
	(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . lsp-deferred)
	 (go-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
	 (web-mode . lsp-deferred)))

;;; Java
(use-package lsp-java
  :hook (java-mode-hook . lsp))

;;; Org mode
(use-package org-superstar)
(use-package org
  :config
  (defun org-graphics-for-bullets ()
    (if (display-graphic-p)
	(org-superstar-mode 1)))
  (defun insert-jira-link (start end)
    "Prompt user to enter an issue number and generate an Org mode
link to the JIRA issue."
    (interactive "r")
    (let ((issue (if (use-region-p)
                     (buffer-substring start end)
                   (read-string "JIRA issue: "))))
      (message "Linking issue %s" issue)
      (when (use-region-p)
        (delete-region start end))
      (insert (format "[[https://jira.appliedinvention.com/browse/%s][%s]]" issue issue))))
  (setq org-directory "~/Org"
	org-agenda-files '("~/Org/")
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
        org-todo-keyword-faces '(("IN-PROGRESS" . org-document-title)
                                 ("WAITING" . compilation-warning)
                                 ("CANCELED" . (:foreground gray50 :weight bold)))
	org-confirm-babel-evaluate nil
	org-agenda-exporter-settings '((ps-print-color-p nil)
				       (org-agnenda-add-entry-text-maxlines 0)
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
         ("C-c j" . insert-jira-link)
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
  (setq inferior-lisp-program "~/.asdf/shims/sbcl"
        slime-lisp-implementations '((sbcl ("~/.asdf/shims/sbcl" "--core" "/opt/slime/sbcl.core-for-slime"))
	                             (ecl ("/usr/bin/ecl")))
        slime-contribs '(slime-fancy)))

;;; Typescript
(use-package typescript-mode
  :mode (("\\.ts[x]?\\'" . typescript-mode)))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

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

;; (load-theme 'spacemacs-dark)
