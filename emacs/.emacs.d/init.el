
;;; init.el --- Emacs custom configuration

;;; Commentary:

;;; Code:

;; No startup screen.
(setq inhibit-startup-screen t)

;; Run the Emacs server.,
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
  :init (progn
	  (require 'ess-site)
	  (setq ess-default-style 'RStudio)))

;; Polymode for R/Markdown code
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
				     (if arg (point) (point-max))))))

	    (defun jb/polymode-insert-new-chunk (&optional chunk-params)
	      "Insert a new R chunk into the file. CHUNK-PARAMS provides any parameters that will be applied to the chunk."
	      (interactive "sChunk params: ")
	      (insert (concat "\n```{r" (if (not (null chunk-params)) (concat " " chunk-params) "") "}\n"))
	      (save-excursion
		(newline)
		(insert "```\n")
		(previous-line))))
  :bind (("C-c M-c" . rmd-send-chunk)
	 ("C-c M-b" . rmd-send-buffer)
	 ("M-n M-i" . jb/polymode-insert-new-chunk)))

;; YAML mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode)))

;; Magit mode
(use-package magit)

;; Org mode
(use-package org
  :init (progn
	  (use-package org-bullets)
	  (use-package org-present
	    :config (progn
		      (add-hook 'org-present-mode-hook
				(lambda ()
				  (org-present-big)
				  (org-display-inline-images)
				  (org-present-hide-cursor)
				  (org-present-read-only)))
		      (add-hook 'org-present-mode-quit-hook
				(lambda ()
				  (org-present-small)
				  (org-remove-inline-images)
				  (org-present-show-cursor)
				  (org-present-read-write))))))
  :config (progn
	    ;; Use UTF-8 bullets.
	    (add-hook 'org-mode-hook
		      (lambda () (org-bullets-mode 1)))

	    ;; Org-babel configuration
	    (org-babel-do-load-languages
	     'org-babel-load-languages
	     '((R . t)
	       (latex . t)))
	    (setq org-confirm-babel-evaluate nil)))
  
(use-package todotxt
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\todo.txt\\'" . todotxt-mode))
	  (setq todotxt-file (expand-file-name "~/Documents/todo/todo.txt"))))

(use-package slime
  :init
  (progn
    (load (expand-file-name "~/Code/Quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl")))

;; Define functions to manage frame alpha.
(defun set-alpha (alpha)
  "Sets the opacity of the current frame. ALPHA should be an integer
between 0 and 100. Note that Emacs itself may have a lower limit on
opacity that is greater than zero."
  (when (and (>= alpha frame-alpha-lower-limit) (<= alpha 100))
    (modify-frame-parameters nil (list (cons 'alpha alpha)))))

(defun change-alpha (amt)
  "Changes the current frame's opacity by AMT."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha))
	 (old-alpha (if alpha-or-nil alpha-or-nil 100))
	 (new-alpha (+ old-alpha amt)))
    (when (and (>= new-alpha frame-alpha-lower-limit) (<= new-alpha 100))
      (set-alpha new-alpha))))

(global-set-key (kbd "C-8") '(lambda() (interactive) (change-alpha -1)))
(global-set-key (kbd "C-9") '(lambda() (interactive) (change-alpha 1)))
(global-set-key (kbd "C-0") '(lambda() (interactive) (set-alpha 100)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
    ("1bd383f15ee7345c270b82c5e41554754b2a56e14c2ddaa2127c3590d0303b95" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" default)))
 '(package-selected-packages
   (quote
    (rust-mode flatui-theme bubbleberry-theme pomidor json-mode org org-bullets org-present htmlize inkpot-theme todotxt chess color-theme-sanityinc-tomorrow company ess magit markdown-mode polymode slime smooth-scrolling yaml-mode leuven-theme async helm use-package)))
 '(tramp-syntax (quote default) nil (tramp))
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (quote
    ((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ba8baf")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
