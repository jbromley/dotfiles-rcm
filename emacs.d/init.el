;;; init.el --- Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up the Emacs package manager
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

;;; Set some memory limits
(setq gc-cons-threshold 128000000
      large-file-warning-threshold 16000000)

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

;;; Integrate with system clipboard and selection.
(setq select-enable-primary t
      select-enable-clipboard t)

;;; Set the default formats for modes derived from cc-mode.
(setq c-default-style (quote
                       ((c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu"))))

;;; Always turn on auto-fill.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Set up global highlight mode in GUI modes.
(if (display-graphic-p)
    (global-hl-line-mode 1))

(defun turn-off-hl-line-mode ()
  "Toggle line highlighting in current buffer"
  (interactive)
  (setq-local global-hl-line-mode
              (null global-hl-line-mode)))

(add-hook 'lisp-interaction-mode-hook 'turn-off-hl-line-mode)
(add-hook 'eww-mode-hook 'turn-off-hl-line-mode)
(add-hook 'term-mode-hook 'turn-off-hl-line-mode)
(add-hook 'slime-repl-mode-hook 'turn-off-hl-line-mode)
(add-hook 'alchemist-iex-mode-hook 'turn-off-hl-line-mode)

;;; Fix the prompt for sql-interactive-mode with PostgreSQL.
;;; Old: "^[_[:alpha:]]*[=][#>] ", "^[_[:alpha:]]*[-][#>] "
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (when (string= sql-product "postgres")
	      (setq sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
	      (setq sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "))))

;;; Disable vc
(setq vc-handled-backends nil)

;;; Convenience global bindings.
(global-set-key (kbd "C-c q") 'auto-fill-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dashboard
(use-package dashboard
  :preface
  (defun jb/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
   time and garbage collections."
   (setq dashboard-banner-logo-title
         (format "Emacs ready in %.2f seconds with %d garbage collections."
                 (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  (add-hook 'dashboard-mode-hook 'jb/dashboard-banner)
  (add-hook 'dashboard-mode-hook 'turn-off-hl-line-mode)
  :if (< (length command-line-args) 2)
  :config
  (setq dashboard-startup-banner 'logo
	dashboard-items '((recents . 10)
			  (bookmarks . 10)))
  (dashboard-setup-startup-hook))

;;; Diminish (clean up the mode line)
(use-package diminish)

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

;;; Completion
(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config (setq company-backends
		'(company-capf company-files
			       (company-dabbrev-code company-gtags company-etags company-keywords company-dabbrev))))

;;; The silver searcher integration
;;; TODO Check ag package options.
(use-package ag)

;;; Fuzzy finder integration
;;; TODO Check fzf package options.
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

;;; GNU global tags
;; (use-package ggtags
;;   :bind
;;   (:map ggtags-mode-map
;; 	("C-c g s" . ggtags-find-other-symbol)
;; 	("C-c g h" . ggtags-view-tag-history)
;; 	("C-c g r" . ggtags-find-reference)
;; 	("C-c g f" . ggtags-find-file)
;; 	("C-c g c" . ggtags-create-tags)
;; 	("C-c g u" . ggtags-update-tags)
;; 	("M-." . ggtags-find-tag-dwim)
;; 	("M-," . pop-tag-mark)
;; 	("C-c <" . ggtags-prev-mark)
;; 	("C-c >" . ggtags-next-mark)))

;;; Elpy

(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
        (error (elpy-rgrep-symbol
                   (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

(use-package elpy
  :bind (:map elpy-mode-map
	      ("M-." . elpy-goto-definition-or-rgrep)
	      ("C-M-n" . elpy-nav-forward-block)
	      ("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode))
  :init (elpy-enable)
  :config (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

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
	org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)"
				      "|" "DONE(d!)" "CANCELED(c@)"))
	org-todo-keyword-faces '(("IN-PROGRESS" . "blue")
				 ("WAITING" . "orange")
				 ("CANCELED" . "gray"))
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

;;; Markdown editing
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Java editing
;; (use-package meghanada
;;   :config
;;   (setq meghanada-java-path "java"
;; 	meghanada-maven-path "mvn")
;;   :hook ((compilation-filter . (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
;; 	 (java-mode . (lambda () (meghanada-mode t) (flycheck-mode +1)))))

;;; YAML editing
(use-package yaml-mode)

;;; Magit mode
(use-package magit
  :bind (("C-x v G" . magit-status)))

;;; Clojure
(use-package paredit
  :config
  (defun setup-paredit-for-lisp-interaction ()
    (enable-paredit-mode)
    (local-set-key [C-return] 'eval-print-last-sexp))
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-mode-hook . enable-paredit-mode)
	 (lisp-interaction-mode . setup-paredit-for-lisp-interaction))
  :diminish (paredit-mode . "([])"))

(use-package enh-ruby-mode)

(use-package clojure-mode-extra-font-locking)

(use-package clojure-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))
  :hook ((clojure-mode . enable-paredit-mode)
	 (clojure-mode . subword-mode)))

(use-package cider
  :config
  (setq cider-repl-pop-to-buffer-on-connect t
	cider-show-error-buffer t
	cider-auto-select-error-buffer t
	cider-repl-history-file "~/.emacs.d/cider-history"
	cider-repl-wrap-history t)
  (defun cider-start-http-server ()
    (interactive)
    (cider-load-current-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))
  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval (format "(user/reset)")))
  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))
  :bind
  (:map clojure-mode-map
	("C-c C-v" . cider-start-http-server)
	("C-M-r" . cider-refresh)
	("C-c u" . cider-user-ns))
  (:map cider-mode-map
	("C-c u" . cider-user-ns))
  :hook ((cider-mode . eldoc-mode)
	 (cider-mode . turn-off-hl-line-mode)
	 (cider-repl-mode . enable-paredit-mode)))

;;; Common Lisp/SLIME
(use-package slime
  :init
  (require 'slime-autoloads)
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
	slime-contribs '(slime-fancy)))

;;; Elixir
(use-package alchemist
  :config (setq alchemist-key-command-prefix (kbd "C-c x")))

;;; TypeScript
;; (use-package tide
;;   :config (progn
;; 	    (tide-setup)
;; 	    (flycheck-mode +1)
;; 	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
;; 	    (eldoc-mode +1)
;; 	    (tide-hl-identifier-mode +1))
;;   :hook (before-save-hook . tide-format-before-save))

;;; Themes and theme switching
(use-package eink-theme
  :defer t)
(use-package modus-operandi-theme
  :defer t)
(use-package modus-vivendi-theme
  :defer t)
(use-package spacemacs-theme
  :defer t)
(use-package xresources-theme
  :defer t)

(use-package theme-looper
  :config
  (global-set-key (kbd "C-{") 'theme-looper-enable-previous-theme)
  (global-set-key (kbd "C-}") 'theme-looper-enable-next-theme))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep customizations separate so machine-specific configurations do not get
;; saved in the dotfiles repo.
(load custom-file)

(put 'narrow-to-region 'disabled nil)
