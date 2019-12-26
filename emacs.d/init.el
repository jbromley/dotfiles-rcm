;;; init.el --- Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the Emacs package manager
(require 'package)
;; (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; Set some memory limits
(setq gc-cons-threshold 67108864
      large-file-warning-threshold 268435456)

;; Run the Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General editing configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode t)
(column-number-mode t)
(windmove-default-keybindings)
(setq-default fill-column 80)
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

;; Integrate with system clipboard and selection.
(setq select-enable-primary t
      select-enable-clipboard t)
      
;; Set the default formats for modes derived from cc-mode.
(setq c-default-style (quote
                       ((c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu"))))

;; Always turn on auto-fill.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Set up global line numbers.
(global-hl-line-mode 1)

(defun turn-off-hl-line-mode ()
  "Toggle line highlighting in current buffer"
  (interactive)
  (setq-local global-hl-line-mode
              (null global-hl-line-mode)))

(add-hook 'lisp-interaction-mode-hook 'turn-off-hl-line-mode)
(add-hook 'eww-mode-hook 'turn-off-hl-line-mode)
(add-hook 'term-mode-hook 'turn-off-hl-line-mode)

;; Set up buffer line numbers.
;; (global-display-line-numbers-mode 1)
;; (setq display-line-numbers-grow-only t)

;; (defun turn-off-line-numbers ()
;;   "Turn off line numbers in the current buffer."
;;   (interactive)
;;   (display-line-numbers-mode 0))

;; (add-hook 'lisp-interaction-mode-hook 'turn-off-line-numbers)
;; (add-hook 'eww-mode-hook 'turn-off-line-numbers)
;; (add-hook 'term-mode-hook 'turn-off-line-numbers)
;; (add-hook 'org-mode-hook 'turn-off-line-numbers)

;; Use shellcheck to check bash scripts.
(add-hook 'sh-mode-hook 'flycheck-mode)

;; Fix the prompt for sql-interactive-mode with PostgreSQL.
;; Old: "^[_[:alpha:]]*[=][#>] ", "^[_[:alpha:]]*[-][#>] "
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (when (string= sql-product "postgres")
	      (setq sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
	      (setq sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "))))

;; Disable vc
(setq vc-handled-backends nil)

;; Convenience global bindings.
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diminish (clean up the mode line)
(use-package diminish)

;; Completion
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode)
  :diminish (company-mode . "Co"))

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
  :bind (("C-c r" . ivy-resume))
  :diminish)

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
	      ;; Make K & R C style use four-space tabs.
	      (setf (cadr (assoc "k&r" c-style-alist)) '(c-basic-offset . 4))
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

;; Clojure

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook (lambda ()
					  (enable-paredit-mode)
					  (local-set-key [C-return] 'eval-print-last-sexp)))
  :diminish (paredit-mode . "([])"))


(use-package enh-ruby-mode)

(use-package clojure-mode-extra-font-locking)

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (setq inferior-lisp-program "lein repl")
	      (font-lock-add-keywords
	       nil
	       '(("(\\(facts?\\)" (1 font-lock-keyword-face))
		 ("(\\(background?\\)" (1 font-lock-keyword-face))))
	      (define-clojure-indent (fact 1))
	      (define-clojure-indent (facts 1))))
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))

(use-package cider
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
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
	("C-c u" . cider-user-ns)))

;; Common Lisp/SLIME

(use-package slime
  :init
  (require 'slime-autoloads)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
	slime-contribs '(slime-fancy)))

;; TypeScript

(use-package tide
  :config (progn
	    (tide-setup)
	    (flycheck-mode +1)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode +1)
	    (tide-hl-identifier-mode +1))
  :hook (before-save-hook . tide-format-before-save))

;; Theme switching
(use-package theme-looper
  :config
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
