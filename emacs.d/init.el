;;; init.el --- Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the Emacs package manager
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; Add custom Emacs Lisp directory to the load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

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

;; Set up global highlight mode.
(global-hl-line-mode 1)

(defun turn-off-hl-line-mode ()
  "Toggle line highlighting in current buffer"
  (interactive)
  (setq-local global-hl-line-mode
              (null global-hl-line-mode)))

(add-hook 'lisp-interaction-mode-hook 'turn-off-hl-line-mode)
(add-hook 'eww-mode-hook 'turn-off-hl-line-mode)
(add-hook 'term-mode-hook 'turn-off-hl-line-mode)
(add-hook 'slime-repl-mode-hook 'turn-off-hl-line-mode)

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

;; Dashboard

(use-package dashboard
  :preface
  (defun jb/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
   time and garbage collections."
   (setq dashboard-banner-logo-title
         (format "Emacs ready in %.2f seconds with %d garbage collections."
                 (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  ; (add-hook 'after-init-hook 'dashboard-refresh-buffer) 
  (add-hook 'dashboard-mode-hook 'jb/dashboard-banner)
  (add-hook 'dashboard-mode-hook 'turn-off-hl-line-mode)
  :if (< (length command-line-args) 2)
  :config
  (setq dashboard-startup-banner 'logo
	dashboard-items '((recents . 10)
			  (bookmarks . 10)))
  (dashboard-setup-startup-hook))

;; Diminish (clean up the mode line)

(use-package diminish)

;; windmove

(use-package windmove
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t)
  :bind
  (("C-c <right>" . windmove-right)
   ("C-c <left>" . windmove-left)
   ("C-c <up>" . windmove-up)
   ("C-c <down>" . windmove-down)))

;; ;; Ivy-posframe

;; (use-package ivy-posframe
;;   :diminish
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;; 	'((t . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-mode 1))

;; Completion
(use-package company
  :diminish (company-mode . "Co")
  :hook (after-init . global-company-mode))

;; The silver searcher integration
(use-package ag)

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

;; Counsel
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag))

;; GNU global tags
(use-package ggtags
  :config
  (defun config-ggtabs-c ()
    (setq c-basic-offset 4)
    (setf (cadr (assoc "k&r" c-style-alist)) '(c-basic-offset . 4))
    (ggtags-mode 1))
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
	("C-c >" . ggtags-next-mark))
  :hook (c-mode-common . config-ggtags-c))

;; Elpy
(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :bind
  (:map elpy-mode-map
	("M-." . elpy-goto-definition)
	("M-S-." . elpy-goto-definition-other-window)))

  
;; Swiper
(use-package swiper
  :config (global-set-key "\C-s" 'swiper))

;; Nice org-mode bullets
(use-package org-bullets)

;; Org mind maps
;; Possible engines are dot, neato, twopi, fdp, sfdp, twopi, and circo.

(use-package org-mind-map
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot"))

;; Org mode
(use-package org
  :config
  (defun org-graphics-for-bullets ()
    (if (display-graphic-p)
	(org-bullets-mode 1)))
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
	 ("C-c l" . org-store-link))
  :hook ((org-mode . org-graphics-for-bullets)))

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
  :hook ((compilation-filter . (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
	 (java-mode . (lambda () (meghanada-mode t) (flycheck-mode +1)))))

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
  ;; XXX: Not sure what the following code does, eliminating to test if I need it.
  ;; (add-hook 'clojure-mode-hook
  ;; 	    (lambda ()
  ;; 	      (setq inferior-lisp-program "lein repl")
  ;; 	      (font-lock-add-keywords
  ;; 	       nil
  ;; 	       '(("(\\(facts?\\)" (1 font-lock-keyword-face))
  ;; 		 ("(\\(background?\\)" (1 font-lock-keyword-face))))
  ;; 	      (define-clojure-indent (fact 1))
  ;; 	      (define-clojure-indent (facts 1))))
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

;; Common Lisp/SLIME

(use-package slime
  :init
  (require 'slime-autoloads)
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
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

(put 'narrow-to-region 'disabled nil)
