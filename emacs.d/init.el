;;; init.el --- Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run the Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Set up the Emacs package manager
(require 'package)
;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MacOS-specific configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (string-equal system-type "darwin") (display-graphic-p))
  (setq default-frame-alist '((width . 164) (height . 60)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General editing configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode t)
(column-number-mode t)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(windmove-default-keybindings)
(setq-default fill-column 80)
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

;; Set the default formats for modes derived from cc-mode.
(setq c-default-style (quote
                       ((c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu"))))

;; Make K & R C style use four-space tabs.
(setf (cadr (assoc "k&r" c-style-alist)) '(c-basic-offset . 4))

;; Disable vc
(setq vc-handled-backends nil)

;; Always turn on auto-fill.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Fix the prompt for sql-interactive-mode with PostgreSQL.
;; Old: "^[_[:alpha:]]*[=][#>] ", "^[_[:alpha:]]*[-][#>] "
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (when (string= sql-product "postgres")
	      (setq sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
	      (setq sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "))))

;; Convenience global bindings.
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Completion
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Smooth scrolling
(use-package smooth-scrolling
  :init (setq smooth-scroll-margin 4)
  :config (smooth-scrolling-mode))

;; Ivy selection
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers nil
	enable-recursive-minibuffers t
	ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  :bind (("C-c r" . ivy-resume)))

;; Load recentf buffer at start if there is no file.
(use-package init-open-recentf)

;; Counsel
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag))

;; GNU global tags
(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (setq c-basic-offset 4)	
      (ggtags-mode 1)))
  :bind
  (:map ggtags-mode-map
	("C-c g s" . ggtags-find-other-symbol)
	("C-c g h" . ggtags-view-tag-history)
	("C-c g r" . ggtags-find-reference)
	("C-c g f" . ggtags-find-file)
	("C-c g c" . ggtags-create-tags)
	("C-c g u" . ggtags-update-tags)
	("M-." . ggtags-find-tag-dwim)
	("M-," . pop-tag-mark)
	("C-c <" . ggtags-prev-mark)
	("C-c >" . ggtags-next-mark)))

;; Elpy
(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable))
  
;; Swiper
(use-package swiper
  :config (global-set-key "\C-s" 'swiper))
;; Nice org-mode bullets
(use-package org-bullets)

;; Org mode
(use-package org
  :config
  (add-hook 'org-mode-hook (lambda ()
			     (if (display-graphic-p) (org-bullets-mode 1))))
  (setq org-directory "~/Org"
	org-agenda-files '("~/Org/")
	org-confirm-babel-evaluate nil
	org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)"
				      "|" "DONE(d!)" "CANCELED(c@)"))
	org-todo-keyword-faces '(("IN-PROGRESS" . "cyan") ("WAITING" . "orange") ("CANCELED" . "red"))
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
	 ("C-c l" . org-store-link)))

;; Markdown editing
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Java editing
(use-package meghanada
  :config
  (setq meghanada-java-path "java"
	meghanada-maven-path "mvn")
  (add-hook 'java-mode-hook
	    (lambda ()
	      (meghanada-mode t)
	      (flycheck-mode +1)))
  (add-hook 'compilation-filter-hook
	    (lambda () (ansi-color-apply-on-region (point-min) (point-max)))))

;; YAML editing
(use-package yaml-mode)
  
;; Magit mode
(use-package magit
  :bind (("C-x v G" . magit-status)))

;; Projectile mode for project management
(use-package projectile
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))
	      
;; Theme switching
(use-package theme-looper
  :config
  (theme-looper-set-ignored-themes '(leuven light-blue tango-dark tsdh-dark wheatgrass solarized tao))
  (global-set-key (kbd "C-{") 'theme-looper-enable-previous-theme)
  (global-set-key (kbd "C-}") 'theme-looper-enable-next-theme))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep customizations separate so machine-specific configurations do not get
;; saved in the dotfiles repo.
(load custom-file)

(recentf-mode 1)
(init-open-recentf)
(put 'narrow-to-region 'disabled nil)
