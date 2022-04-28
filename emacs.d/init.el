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
;; Configuration setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the package manager
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;; Use use-package to install/manage packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; Add custom Emacs Lisp directory to the load path.
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (normal-top-level-add-subdirs-to-load-path))

;; Set large file size limits
(setq large-file-warning-threshold 16000000)

;; Run the Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General editing configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode t)
(setq-default fill-column 80
              indent-tabs-mode nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Automatically update buffers from disk.
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Save last place in a file.
(save-place-mode 1)

;; Enable mini-buffer history.
(setq history-length 8)
(savehist-mode 1)

;; Integrate with system clipboard and selection.
(setq select-enable-primary t
      select-enable-clipboard t)

;; Use xclip for copy/paste if we are running in a terminal.

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

;; Avoid garbage collection while in the mini-buffer.
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold 16000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show an Emacs dashboard on startup
(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  :custom
  ;; (dashboard-startup-banner . 'logo)
  (dashboard-items '((recents . 5)
                     (projects . 3)
                     (bookmarks . 3)
                     (agenda . 3))))

;; Expand regions intelligently
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; Copy/paste from terminal emacs in X11
(use-package xclip
  :config
  (if (not (display-graphic-p))
    (xclip-mode 1)))

;; Selection framework
(use-package selectrum
  :custom
  (selectrum-mode t))

(use-package selectrum-prescient
  :after selectrum
  :custom
  (prescient-persist-mode t)
  (selectrum-prescient-mode t))

(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

;; initialize the path
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; which key
(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-idle-display 0.5))

;; windmove
(use-package windmove
  :config
  (windmove-default-keybindings)
  :custom
  (windmove-wrap-around t))

;; Snippets
(use-package yasnippet
  :config
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (yas-global-mode t)
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("C-c y" . yas-expand)))

(use-package yasnippet-snippets)

;; Completion
(use-package company
  :diminish company-mode
  :custom (company-backends
           '(company-capf company-files
                          (company-dabbrev-code company-gtags company-etags company-keywords company-dabbrev)))
  :init (global-company-mode))

;; The silver searcher integration
(use-package ag)

;; Fuzzy finder integration
(use-package fzf)

;; Smooth scrolling
(use-package smooth-scrolling
  :custom (smooth-scroll-margin 8)
  :config (smooth-scrolling-mode))

;; Swiper
(use-package swiper
  :config (global-set-key "\C-s" 'swiper))

;; Projectile mode for project management
(use-package projectile
  :config (projectile-mode 1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

;; Magit mode
(use-package magit
  :custom (vc-handled-backends nil)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch)))

;; Ligatures
(use-package ligature
  :load-path "elisp/ligature.el/"
  :ensure nil
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
;; Languages and file format packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Snippets
(use-package java-snippets
  :defer t)

;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; Python
(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server nil
              lsp-python-ms-python-executable "/usr/bin/python3")
  :hook (python-mode . (lambda () (require 'lsp-python-ms))))

;; Java
(use-package lsp-java
  :defer t
  :hook (java-mode-hook . lsp))

;; Org mode
(use-package ox-gfm)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org
  :config
  (defun jb/org-insert-jira-link (start end)
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
  (setq org-publish-project-alist
        '(("grow-org"
           :base-directory "~/Org/grow/"
           :base-extension "org"
           :publishing-directory "~/Public/"
           :recursive t
           :publishing-function org-html-publish-to-html)
          ("grow-static"
           :base-directory "~/Org/grow/"
           :base-extension "css\\|png\\|svg\\|ico"
           :publishing-directory "~/Public/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("gantry"
           :components ("grow-org" "grow-static"))))
  :custom
  (org-directory (expand-file-name  "~/Org"))
  (org-agenda-files '("~/Org/" "~/Org/grow/"))
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
         ("C-c j" . jb/org-insert-jira-link)
         ("C-c l" . org-store-link))
  :hook ((org-mode . (lambda () (require 'ox-gfm nil t)))))

;; Rust
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  ;;(add-hook 'rustic-mode-hook 'jb/rustic-mode-hook)
)

(defun jb/rustic-mode-hook ()
  ;; Make C-c C-c C-r work without having to confirm, but don't try to save rust
  ;; buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; Go
(use-package go-mode
  :defer t
  :config
  (defun jb/lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook ((go-mode . jb/lsp-go-install-save-hooks)
         (go-mode . (lambda () (setq-default tab-width 4))))
  :mode ((("\\.go\\'" . go-mode))))

;; Markdown editing
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom (markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Elixir
(use-package elixir-mode
  :commands (elixir-mode)
  :defer t
  :hook ((elixir-mode . (lambda () (add-to-list 'exec-path "/opt/elixir-ls/")))))

;; YAML editing
(use-package yaml-mode
  :mode
  ("\\.yaml\\'" "\\.yml\\'")
  :commands
  (yaml-mode))

;; Paredit
(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (racket-mode . enable-paredit-mode)
         (racket-repl-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

;; Lisp interaction mode - bind eval-print-last-sexp
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "C-<return>") 'eval-print-last-sexp)))

;; Common Lisp - SLY
(use-package sly
  :custom
  (inferior-lisp-program (expand-file-name "~/.asdf/shims/sbcl"))
  (sly-lisp-implementations '((sbcl ("~/.asdf/shims/sbcl") :coding-system utf-8-unix)
                              (ecl ("/usr/bin/ecl"))))
  :bind (:map sly-prefix-map ("M-h" . sly-documentation-lookup)))

;; Racket - racket mode
(use-package racket-mode
  :config (require 'lsp-racket)
  :bind (("C-\\" . racket-insert-lambda)))

(use-package geiser
  :commands (geiser run-geiser geiser-repl)
  :defer t)

;; Clojure - Clojure mode and CIDER
(use-package clojure-mode
  :commands
  (clojure-mode)
  :defer t)

(use-package cider
  :commands
  (cider)
  :defer t)

;; Lua
(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :custom
  (lsp-clients-lua-language-server-bin "/opt/lua-language-server/bin/Linux/lua-language-server")
  (lsp-clients-lua-language-server-main-location "/opt/lua-language-server/main.lua"))

;; Typescript
(use-package typescript-mode
  :mode (("\\.ts[x]?\\'" . typescript-mode)))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")
                                       ("tsx" . "\\.ts[x]?\\'"))))

(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  ;; Rustic
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;; Racket
  (add-to-list 'lsp-client-packages 'lsp-racket)
  :custom (lsp-rust-server 'rust-analyzer)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-ui-mode)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         ; (lua-mode . lsp-deferred)
         (racket-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (python-mode . lsp-deferred)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;;  Emacs Debug Adapter Protocol
(use-package dap-mode
  :commands
  (dap-mode dap-debug)
  :defer t
  :after (lsp-mode)
  :functions dap-hydra/nil
  ;; :bind (:map lsp-mode-map
  ;;        ("<f5>" . dap-debug)
  ;;        ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

;; Themes and theme switching
(use-package dracula-theme
  :defer t)

(use-package theme-looper
  ;; :config
  ;; (theme-looper-set-favorite-themes '(*default*
  ;;                                     dichromacy
  ;;                                     dracula
  ;;                                     misterioso
  ;;                                     deeper-blue))
  :bind
  (("C-<" . theme-looper-enable-previous-theme)
   ("C->" . theme-looper-enable-next-theme)
   ("C-|" . theme-looper-select-theme)))

(put 'narrow-to-region 'disabled nil)

(provide 'init)
;;; init.el ends here
