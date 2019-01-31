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
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Set up load paths and other paths.
;; (defvar elisp-path (expand-file-name "~/.emacs.d/elisp/")
;;       local-pkgs '("polymode" "poly-markdown" "poly-noweb" "poly-R"))
;; (add-to-list 'load-path (mapcar (lambda (pkg) (concat elisp-path pkg)) local-pkgs))
      
(defvar local-bin (concat
		   (file-name-as-directory (getenv "HOME")) ".local/bin"))
(setq exec-path (append (list local-bin) exec-path))
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

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

    ;; Use curl when available
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ;; Helm configuration variables
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

    ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ;; (define-key helm-map (kbd "C-z") 'helm-select-action)
    
    ;; (define-key helm-grep-mode-map (kbd "<return>")
    ;;   'helm-grep-mode-jump-other-window)
    ;; (define-key helm-grep-mode-map (kbd "n")
    ;;   'helm-grep-mode-jump-other-window-forward)
    ;; (define-key helm-grep-mode-map (kbd "p")
    ;;   'helm-grep-mode-jump-other-window-backward)

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
          (setq ess-default-style 'RStudio
                ess-indent-with-fancy-comments nil
		ess-roxy-str "#'")))

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
	  (use-package org-pomodoro
	    :config (progn
		      (setq org-pomodoro-audio-player "ogg123"
			    org-pomodoro-finished-sound "/usr/share/sounds/freedesktop/stereo/complete.oga"
			    org-pomodoro-finished-sound-args "-q"
			    org-pomodoro-killed-sound "/usr/share/sounds/freedesktop/stereo/suspend-error.oga"
			    org-pomodoro-killed-sound-args "-q"
			    org-pomodoro-killed-sound-p t
			    org-pomodoro-long-break-sound "/usr/share/sounds/freedesktop/stereo/phone-incoming-call.oga"
			    org-pomodoro-long-break-sound-args "-q"
			    org-pomodoro-short-break-sound "/usr/share/sounds/freedesktop/stereo/complete.oga"
			    org-pomodoro-short-break-sound-args "-q"
			    org-pomodoro-start-sound "/usr/share/sounds/freedesktop/stereo/complete.oga"
			    org-pomodoro-start-sound-args "-q"
			    org-pomodoro-start-sound-p t)
		      (add-hook 'org-pomodoro-break-finished-hook
				(lambda ()
				  (interactive)
				  (org-pomodoro '(16)))))))
  :config (progn
            ;; Use UTF-8 bullets.
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

            ;; Org-babel configuration
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((R . t)
               (latex . t)
	       (ditaa . t)))
            (setq org-directory "~/Org"
		  org-confirm-babel-evaluate nil
		  org-todo-keywords '((sequence "TODO(t)"
						"IN-PROGRESS(i)"
						"WAITING(w@/!)"
						"|"
						"DONE(d!)"
						"CANCELED(c@)"))
		  org-todo-keyword-faces '(("IN-PROGRESS" . "cyan")
					   ("WAITING" . "orange")
					   ("CANCELED" . "red"))
		  org-agenda-exporter-settings '((ps-print-color-p nil)
						 (org-agenda-add-entry-text-maxlines 0)
						 (htmlize-output-type 'css))
		  org-hierarchical-todo-statistics nil
		  org-enforce-todo-dependencies t
		  org-enforce-todo-checkbox-dependencies t
		  org-agenda-dim-blocked-tasks t
		  org-log-done t
		  org-default-notes-file (concat org-directory "/Notes.org")))
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)))

(use-package jdee
  :init (progn
	  (setq jdee-jdk-registry (quote
				    (("1.8.0" . "/usr/lib/jvm/java-8-openjdk-amd64")
				    ("1.11.0" . "/usr/lib/jvm/java-11-openjdk-amd64")))
		jdee-jdk (quote ("1.8.0"))
		jdee-server-dir "/home/jay/.local/opt/jdee-server/")
	  ;; Clean up ANSI codes in Maven compile output.
	  (add-hook 'compilation-filter-hook
		    (lambda () (ansi-color-apply-on-region (point-min) (point-max))))))

(use-package slime
  :init
  (progn
    (load (expand-file-name "~/Quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl")))

;; Fix the prompt for sql-interactive-mode with PostgreSQL.
;; Old: "^[_[:alpha:]]*[=][#>] ", "^[_[:alpha:]]*[-][#>] "
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (when (string= sql-product "postgres")
	      (setq sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
	      (setq sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "))))

(defun jb/clojure-setup ()
  (setq inferior-lisp-program "lein repl")
  (font-lock-add-keywords
   nil
   '(("(\\(facts?\\)"
      (1 font-lock-keyword-face))
     ("(\\(background?\\)"
      (1 font-lock-keyword-face))))
  (define-clojure-indent (fact 1))
  (define-clojure-indent (facts 1)))

(use-package paredit
  :hook ((clojure-mode . enable-paredit-mode)
         (cider-repl-mode . paredit-mode)))

(use-package clojure-mode
  :hook (clojure-mode . jb/clojure-setup)
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
            (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
            (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))))
;;(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))

(use-package cider
  :hook (cider-mode . cider-turn-on-eldoc-mode)
  :config (progn
            (setq cider-repl-pop-to-buffer-on-connect t
                  cider-show-error-buffer t
                  cider-auto-select-error-buffer t
                  cider-repl-wrap-history t)))

(use-package tuareg)

(use-package fsharp-mode
  :mode (("\\.fs[iylx]?$" . fsharp-mode))
  :config (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-"
		fsharp-compiler "/usr/bin/fsharpc"))
  
;; Toggle mode-line colors for basic theme.
(defun mode-line-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(mode-line mode-line-inactive))
        (invisible-color "#e8e8e8")
        (visible-color "#a1b56c"))
    (cond ((string= visible-color (face-attribute 'mode-line :background))
           (mapcar (lambda (face)
                     (set-face-background face invisible-color)
                     (set-face-attribute face nil :height 20))
                   faces-to-toggle))
          (t
           (mapcar (lambda (face)
                     (set-face-background face visible-color)
                     (set-face-attribute face nil :height (face-attribute 'default :height)))
                   faces-to-toggle)))))

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

(put 'narrow-to-region 'disabled nil)

(load custom-file)
