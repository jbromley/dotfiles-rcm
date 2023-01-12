;;; init.el --- Emacs configuration
;;
;; Author: J. Bromley <https://github.com/jbromley>
;; Maintainer: J. Bromley <jbromley@gmail.com>
;; Keywords: emacs configuration
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))

;;; Commentary:
;; Cross-platform (Linx, macOS, almost Windows) Emacs configuration file.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration setup - package archives and use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the package manager
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq use-package-always-ensure t)

;; Set large file size limits
(setq large-file-warning-threshold 16000000)

;; Run the Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General editing configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen t)

(column-number-mode t)
(setq-default fill-column 80
              indent-tabs-mode nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Automatically update buffers from disk.
(global-auto-revert-mode 1)

;; Save last place in a file.
(save-place-mode 1)

;; Enable mini-buffer history.
(setq history-length 8)
(savehist-mode 1)

;; Integrate with system clipboard and selection.
(setq select-enable-primary t
      select-enable-clipboard t)

;; Set the default formats for modes derived from cc-mode.
(setq-default c-basic-offset 4)
(defvar c-default-style)
(setq c-default-style (quote
                       ((c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (java-mode . "java"))))

;; Manage parentheses
(show-paren-mode t)
(setq show-paren-style 'parenthesis
      show-paren-when-point-inside-paren t)
(electric-pair-mode t)

;; Always turn on auto-fill.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Avoid garbage collection while in the mini-buffer.
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold 16000000)))

;; Lisp interaction mode - eval with Ctrl+Return
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "C-<return>") 'eval-print-last-sexp)))

;; Fix the prompt for sql-interactive-mode with PostgreSQL.
;; Old: "^[_[:alpha:]]*[=][#>] ", "^[_[:alpha:]]*[-][#>] "
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (defvar sql-product)
            (defvar sql-prompt-regexp)
            (defvar sql-prompt-cont-regexp)
            (when (string= sql-product "postgres")
              (setq sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
              (setq sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Icons!
(use-package all-the-icons
  :if (display-graphic-p))

;; Dashboard
(use-package dashboard
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config
  (dashboard-setup-startup-hook))

;; Diminish modeline text
(use-package diminish)

;; initialize the path
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

;; Expand regions intelligently
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; Copy/paste from terminal emacs in X11
(use-package xclip
  :config
  (if (not (display-graphic-p))
    (xclip-mode 1)))

;; which key
(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.5))

;; windmove
(use-package windmove
  :config
  (windmove-default-keybindings)
  :custom
  (windmove-wrap-around t))

;; The silver searcher integration
(use-package ag)

;; Fuzzy finder integration
(use-package fzf)

;; Smooth scrolling
(use-package smooth-scrolling
  :custom (smooth-scroll-margin 4)
  :config (smooth-scrolling-mode))

;; Swiper
(use-package swiper
  :bind (("C-s" . swiper)))

(use-package project
  :init
  (defun jb/find-mix-project (dir)
    "Find a parent directory of DIR containing a 'mix.exs' file."
    (let ((dir (locate-dominating-file dir "mix.exs")))
      (and dir (cons 'explicit dir))))
  (defmethod project-root ((project (head explicit)))
    (cdr project))
  :config
  (add-hook 'project-find-functions #'jb/find-mix-project))

;; ;; Projectile mode for project management
;; (use-package projectile
;;   :config
;;   (defun elixir/find-mix-project (dir)
;;     "Try to locate an Elixir project root by 'mix.exs' above DIR."
;;     (let ((mix-root (locate-dominating-file dir "mix.exs")))
;;       (message "Found Elixir project root in '%s' starting from '%s'" mix-root dir)
;;       (if (stringp mix-root)
;;           `(transient . ,mix-root)
;;         nil)))
;;   (add-hook 'project-find-functions 'elixir/find-mix-project nil nil)
;;   (projectile-mode 1)
;;   :bind
;;   (:map projectile-mode-map
;;         ("C-c p" . projectile-command-map)))

;; Magit mode
(use-package magit
  :custom (vc-handled-backends nil)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch)))

;; Ligatures
(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures '(prog-mode racket-repl-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  (global-ligature-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-flex orderless-regexp))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind ("M-a" . marginalia-cycle)
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h b" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; ;; M-s bindings (search-map)
   ;; ("M-s d" . consult-find)
   ;; ("M-s D" . consult-locate)
   ;; ("M-s g" . consult-grep)
   ;; ("M-s G" . consult-git-grep)
   ;; ("M-s r" . consult-ripgrep)
   ;; ("M-s l" . consult-line)
   ;; ("M-s L" . consult-line-multi)
   ;; ("M-s m" . consult-multi-occur)
   ;; ("M-s k" . consult-keep-lines)
   ;; ("M-s u" . consult-focus-lines)
   ;; ;; Isearch integration
   ;; ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  ;; Use projectile for defining projects.
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :init
  (global-corfu-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ox-gfm)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org
  :config
  (setq org-publish-project-alist
        '(("org"
           :base-directory "~/Org/"
           :base-extension "org"
           :publishing-directory "~/Public/"
           :recursive t
           :publishing-function org-html-publish-to-html)
          ("static"
           :base-directory "~/Org/"
           :base-extension "css\\|png\\|svg\\|ico"
           :publishing-directory "~/Public/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("projects"
           :components ("org" "static"))))
  :custom
  (org-directory (expand-file-name  "~/Org"))
  (org-agenda-files '("~/Org/"))
  (org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (org-todo-keyword-faces '(("IN-PROGRESS" . org-agenda-structure)
                            ("WAITING" . compilation-warning)
                            ("CANCELED" . (:foreground gray50 :weight bold))))
  (org-confirm-babel-evaluate nil)
  (org-agenda-exporter-settings '((ps-print-color-p nil)
                                  (org-agenda-add-entry-text-maxlines 0)
                                  (htmlize-output-type 'css)))
  (org-hierarchical-todo-statistics nil)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-agenda-dim-blocked-tasks t)
  (org-log-done t)
  (org-default-notes-file (concat org-directory "/Notes.org"))
  (org-catch-invisible-edits 'show)
  (org-hide-leading-stars t)
  (org-html-postamble t)
  (org-html-postamble-format '(("en" "<hr/><p style=\"text-align:center\">Modified: %C</p>")))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-<up>" . org-timestamp-up)
         ("C-<down>" . org-timestamp-down))
  :hook ((org-mode . (lambda () (require 'ox-gfm nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages and file format packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elixir
(use-package elixir-mode
  :commands (elixir-mode)
  :hook (elixir-mode . eglot-ensure))

;; Web mode - HTML, CSS, JSON
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")
                                       ("tsx" . "\\.ts[x]?\\'"))))

;; YAML editing
(use-package yaml-mode
  :mode
  ("\\.yaml\\'" "\\.yml\\'")
  :commands
  (yaml-mode))

;; Markdown editing
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom (markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; CMake
(use-package cmake-project)
(use-package cmake-mode
  :hook
  (cmake-mode . cmake-project-mode))

;; Paredit
(use-package paredit
  :diminish "()"
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (racket-mode . enable-paredit-mode)
         (racket-repl-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

;; Racket - racket mode
(use-package racket-mode
  :config (require 'lsp-racket)
  :bind
  (:map racket-mode-map
        ("C-\\" . racket-insert-lambda)))

;; Common Lisp - SLY
(use-package sly
  :custom
  (inferior-lisp-program (expand-file-name "~/.asdf/shims/sbcl"))
  (sly-lisp-implementations '((sbcl ("~/.asdf/shims/sbcl") :coding-system utf-8-unix)
                              (ecl ("/usr/bin/ecl"))))
  :bind
  (:map sly-prefix-map ("M-h" . sly-documentation-lookup)))

;; snippets
(use-package yasnippet
  :functions yas-reload-all
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode))

(use-package yasnippet-snippets)
(use-package elixir-yasnippets)

;; Lua
(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP (eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(elixir-mode "/opt/elixir-ls/language_server.sh"))
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes and theme switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dracula-theme)
(use-package exotica-theme)

(load-theme 'dracula t t)

(use-package theme-looper
  :config
  (theme-looper-set-favorite-themes '(dracula
                                      exotica
                                      deeper-blue
                                      manoj-dark
                                      misterioso
                                      modus-vivendi
                                      *default*
                                      dichromacy
                                      modus-operandi))
  :bind
  (("C-<" . theme-looper-enable-previous-theme)
   ("C->" . theme-looper-enable-next-theme)
   ("C-|" . theme-looper-select-theme)
   ("C-\\" . theme-looper-enable-random-theme)))

(put 'narrow-to-region 'disabled nil)

(provide 'init)
;;; init.el ends here
