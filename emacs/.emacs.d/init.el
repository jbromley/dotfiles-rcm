;;; init.el --- Emacs custom configuration

;;; Commentary:

;;; Code:
(require 'server)
(unless (server-running-p)
  (server-start))

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
(defvar local-bin (concat (file-name-as-directory (getenv "HOME"))
			  ".local/bin"))
(setq exec-path (append (list local-bin) exec-path))

;; Basic editing configuration
(show-paren-mode t)
(column-number-mode t)
(windmove-default-keybindings)
(setq-default fill-column 80)

;; cc-mode selection
(setq c-default-style (quote
		       ((c-mode . "k&r")
			(c++-mode . "stroustrup")
			(java-mode . "java")
			(awk-mode . "awk")
			(other . "gnu"))))

;; Smooth scrolling
(use-package smooth-scrolling
  :init (setq smooth-scroll-margin 4)
  :config (smooth-scrolling-mode))

;; Company
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; Helm
(use-package helm
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)

    ; Use curl when available
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ; Helm configuration variables
    (setq helm-candidate-number-limit 64
	  helm-google-suggest-use-curl-p t
	  helm-scroll-amount 4
	  helm-split-window-in-side-p t
	  helm-echo-input-in-header-line t
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t
          helm-buffer-skip-remote-checking t
          helm-mode-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-apropos-fuzzy-match t
          helm-locate-fuzzy-match t
          helm-display-header-line nil)

    ; Key bindings for particular maps.
    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

    ; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ; (define-key helm-map (kbd "C-z") 'helm-select-action)

    ; (define-key helm-grep-mode-map (kbd "<return>")
    ;   'helm-grep-mode-jump-other-window)
    ; (define-key helm-grep-mode-map (kbd "n")
    ;   'helm-grep-mode-jump-other-window-forward)
    ; (define-key helm-grep-mode-map (kbd "p")
    ;   'helm-grep-mode-jump-other-window-backward)

    (unless (boundp 'completion-in-region-function)
      (define-key lisp-interaction-mode-map [remap completion-at-point]
	'helm-lisp-completion-at-point)
      (define-key emacs-lisp-mode-map [remap completion-at-point]
	'helm-lisp-completion-at-point))

    (helm-mode 1))
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x c o" . helm-occur)
	 ("M-/" . helm-dabbrev)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x c r" . helm-recentf)
	 ("C-x c SPC" . helm-all-mark-rings)
	 ("C-x c w" . helm-wikipedia-suggest)
	 ("C-x c g" . helm-google-suggest)
	 ("C-x c x" . helm-register)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 :map helm-grep-mode-map
	 ("<return>" . helm-grep-mode-jump-other-window)
	 ("n" . helm-grep-mode-jump-other-window-forward)
	 ("p" . helm-grep-mode-jump-other-window-backward)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Emacs Speaks Statistics (ESS) mode
(use-package ess
  :init (require 'ess-site))

(use-package polymode
  :init (progn
	  (require 'poly-R)
	  (require 'poly-markdown))
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
	    (eval-when-compile
	      (require 'polymode-core)
	      (defvar pm/chunkmode))
	    (declare-function pm-map-over-spans "polymode-core")
	    (declare-function pm-narrow-to-span "polymode-core")

	    (defun rmd-send-chunk ()
	      "Send current R chunk to ESS process."
	      (interactive)
	      (and (eq (oref pm/chunkmode :mode) 'r-mode)
		   (pm-with-narrowed-to-span nil
		     (goto-char (point-min))
		     (forward-line)
		     (ess-eval-region (point) (point-max) nil nil 'R))))

	    (defun rmd-send-buffer (arg)
	      "Send all R code blocks in buffer to ESS process. With prefix send regions above point."
	      (interactive "P")
	      (save-restriction
		(widen)
		(save-excursion
		  (pm-map-over-spans 'rmd-send-chunk (point-min)
				     (if arg (point) (point-max))))))))

;; YAML mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode)))

;; Magit mode
(use-package magit)

(use-package slime
  :init
  (progn
    (load (expand-file-name "~/Code/Quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" default)))
 '(package-selected-packages
   (quote
    (chess color-theme-sanityinc-tomorrow company ess magit markdown-mode polymode slime smooth-scrolling yaml-mode leuven-theme async helm use-package)))
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
