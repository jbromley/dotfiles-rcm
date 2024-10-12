;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: J. Bromley
;; URL:
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This emacs configuration project is a customizable base that provides better
;; Emacs defaults and optimized startup, intended to serve as a solid foundation
;; for your vanilla Emacs configuration.

;;; Code:

;;; package.el
(when (bound-and-true-p package-initialize-and-refresh)
  ;; Initialize and refresh package contents again if needed
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Ensure use-package is available at compile time
  (eval-when-compile
    (require 'use-package)))

;; Activate some utility Emacs functions
(use-package emacs
  :init
  ;; Window navigation
  (windmove-default-keybindings 'shift)

  ;; (add-to-list 'default-frame-alist '(alpha-background . 50))
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))
  ;; (setq-default line-spacing 0.05)

  :config
  (setq-default left-fringe-width  8)
  (setq-default right-fringe-width 8)

  ;; Do not show an arrow at the top/bottom in the fringe and empty lines
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)

  ;; Continue wrapped lines at whitespace rather than breaking in the
  ;; middle of a word.
  (setq-default word-wrap t)

  ;; Disable wrapping by default due to its performance cost.
  (setq-default truncate-lines t)

  ;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
  ;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
  (setq-default indent-tabs-mode nil
                tab-always-indent nil
                tab-width 4)
  
  ; (setq-default fill-column 100)

  (setq truncate-string-ellipsis "…")

  (when (memq 'context-menu ui-features)
    (when (and (display-graphic-p) (fboundp 'context-menu-mode))
      (add-hook 'after-init-hook #'context-menu-mode)))
  
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose t)
  (auto-save-default t)                 ;; Auto-save backup files
  (auto-save-include-big-deletions t)
  (auto-save-list-file-prefix
   (expand-file-name "autosave/" user-emacs-directory))
  (auto-window-vscroll nil)
  (backup-by-copying t)                 ;; Backup by copying rather renaming
  (backup-by-copying-when-linked t)
  (backup-directory-alist
   `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  ; (blink-cursor-mode t)                 ;; Blink cursor
  (blink-matching-paren 'jump)          ;; Jump to matching paren when on screen
  (column-number-mode t)                ;; Show column number in mode line
  ; (delete-selection-mode t)           ;; Select text and delete it by typing.
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048)
  (comment-empty-lines t)               ;; Comment empty lines to avoid empty lines in comment block
  (comment-multi-line t)                ;; Continue comments on Enter
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (confirm-kill-emacs 'y-or-n-p)
  (confirm-nonexistent-file-or-buffer nil)
  (create-lockfiles nil)                ;; Do not create lock files
  (delete-by-moving-to-trash (not noninteractive))
  (delete-old-versions t)               ;; Delete excess backup versions silently
  (delete-pair-blink-delay 0.03)        ;; Delay after showing paired character to delete
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (dired-recursive-deletes 'top)
  (dired-recursive-copies  'always)
  (display-line-numbers-type t)         ;; Absolute line numbers
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  (display-time-default-load-average t) ;; Show loading time statistics
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  ; (electric-indent-mode nil)          ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)                ;; Turns on automatic parens pairing
  (enable-recursive-minibuffers t)
  (fast-but-imprecise-scrolling t)
  (find-file-suppress-same-file-warnings t) ;; Don't warn two files are the same
  (find-file-visit-truename t)
  (frame-resize-pixelwise t)
  ; (global-auto-revert-mode t)           ;; Automatically reload file and show changes if the file has changed
  (global-auto-revert-non-file-buffers t)
  ; (global-display-line-numbers-mode t)  ;; Display line numbers
  (global-hl-line-mode t)               ;; Highlight the current line
  (global-text-scale-adjust-resizes-frames nil)
  (global-visual-line-mode t)           ;; Enable truncated lines
  (history-length 300)                  ;; Maximum history list length
  (hscroll-margin 2)
  (hscroll-step 1)                      ;; Horizontal scroll step
  (kept-new-versions 5)                 ;; How many backup versions to keep
  (kept-old-versions 5)
  (kill-buffer-delete-auto-save-files t) ;; Delete auto-save files when killing buffer
  (kill-do-not-save-duplicates t)       ;; Don't duplicate entries in kill ring
  (lazy-highlight-initial-delay 0)      ;; Delay before lazily highlighting matches
  (make-backup-files nil)
  (minibuffer-prompt-properties         ;; Keep cursor out of read-only minibuffer areas
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 1)
  (mouse-wheel-progressive-speed nil)   ;; Disable progressive speed when scrolling
  (mouse-yank-at-point t)
  (pixel-scroll-precision-mode t)       ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil) ;; Disable momentum scrolling for pixel precision.
  (python-indent-guess-indent-offset-verbose nil) ;; Don't complain when can't guess indentation
  (recentf-auto-cleanup 'mode)
  (recentf-max-saved-items 300)         ;; default is 20
  (recentf-mode t)                      ;; Enable recent file mode
  (require-final-newline t)
  (resize-mini-windows 'grow-only)
  (revert-without-query (list "."))     ;; Do not prompt to revert these files
  (ring-bell-function #'ignore)         ;; Do nothing when bell is to ring
  (savehist-save-minibuffer-history t)  ;; Save all recorded mini-buffer histories
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  ; (save-place-limit 600)
  (scroll-conservatively 10)
  (scroll-error-top-bottom t)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (sentence-end-double-space nil)       ;; Sentence ends with ". "
  (sh-indent-after-continuation 'always)
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (split-height-threshold 80)
  (split-width-threshold 160)
  (switch-to-buffer-obey-display-actions t)
  (tramp-auto-save-directory
   (expand-file-name "tramp-autosave/" user-emacs-directory))
  (tramp-backup-directory-alist backup-directory-alist)
  (treesit-font-lock-level 4)           ;; Use advanced font locking for Treesit mode.
  (truncate-partial-width-windows nil)  ;; Respect value of truncate-lines for narrow windows
  (uniquify-buffer-name-style 'forward)
  (vc-follow-symlinks t)                ;; Follow symlinks without asking
  (vc-make-backup-files nil)            ;; Do not backup version controlled files
  (version-control t)                   ;; Use version numbers for backup files
  (visible-bell nil)                    ;; No visual bell
  (whitespace-line-column 120)          ;; whitespace-mode marker after 120 columns
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  (window-resize-pixelwise nil)
  (x-stretch-cursor nil)

  :bind
  (;([escape] . keyboard-escape-quit)
   ("C-+" . text-scale-increase)
   ("C--" . text-scale-decrease)
   ("C-M-z" . zap-up-to-char))

  :hook
  ((after-init . global-auto-revert-mode)
   (after-init . recentf-mode)
   (after-init . savehist-mode)
   (after-init . save-place-mode)
   (after-init . window-divider-mode)
   (after-init . (lambda ()
                   (message (format "Loaded %s packages in %s."
                                    (number-to-string (length package-activated-list))
                                    (emacs-init-time)))))
   (minibuffer-setup . cursor-intangible-mode)
   (prog-mode . display-line-numbers-mode)))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?"))

(use-package dracula-theme
  :defer t
  :config
                                        ;(load-theme 'dracula t)
  )

(use-package almost-mono-themes
  :defer t)

(use-package tok-theme
  :defer t)

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
  (doom-modeline-persp-icon t)) ;; Adds folder icon next to persp name

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
                                       "?=" "?." "??" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)" ; Removed ";;"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Terminal inside Emacs
(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))

;; Vertico, consult, embark and others
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c i" . consult-info)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c t" . consult-theme)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")

  :custom
  (consult-fd-args '((if (executable-find "fd" 'remote) "fd" "fdfind") "--full-path --color=never")))

(use-package consult-ag)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)          ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)           ;; Enable auto completion
  (corfu-auto-prefix 2)    ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t) ;; Enable popup information
  (corfu-popupinfo-delay 0.5) ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s) ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;;; Project management
(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/Code/")))
;; Use Bookmarks for smaller, not standard projects


;;; Programming language mode configurations

;; Integrate Emacs with tools installed with mise.
(use-package mise
      :hook (after-init . global-mise-mode))

;; Remap modes to treesitter modes where possible
(setq treesit-language-source-alist
      '((c . ("https://github.com/tree-sitter/tree-sitter-c" nil nil nil nil))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" nil nil nil nil))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake" nil nil nil nil))
        (css . ("https://github.com/tree-sitter/tree-sitter-css" nil nil nil nil))
        (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir" nil nil nil nil))
        (erlang . ("https://github.com/WhatsApp/tree-sitter-erlang/" nil nil nil nil))
        (heex . ("https://github.com/phoenixframework/tree-sitter-heex" nil nil nil nil))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" nil nil nil nil))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" nil nil nil nil))
        (julia . ("https://github.com/tree-sitter/tree-sitter-julia" nil nil nil nil))
        (lua . ("https://github.com/tjdevries/tree-sitter-lua" nil nil nil nil))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" nil nil nil nil))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" nil nil nil nil))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" nil nil nil nil)) 
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" nil nil nil nil))))

(dolist (lang-src treesit-language-source-alist)
  (let ((lang (car lang-src)))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))


(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (css-mode . css-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (heex-mode . heex-ts-mode)
        (html-mode . html-ts-mode)
        (json-mode . json-ts-mode)
        (julia-mode . julia-ts-mode)
        (lua-mode . lua-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; Language modes
(use-package elixir-mode
  :mode "\\.ex\\'")
(use-package julia-ts-mode
  :mode "\\.jl\\'"
  :hook ((julia-mode julia-ts-mode). eglot-jl-init))
(use-package lua-mode
  :mode "\\.lua\\'")
(use-package racket-mode
  :mode "\\.rkt\\'")
(use-package yaml-ts-mode
  :mode "\\.\\(yaml\\|yml\\)\\'")
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"
        sly-lisp-implementations '((sbcl ("sbcl" "--core" "/home/jay/.local/lib/sbcl-sly.core")))))

;; (use-package geiser-racket)

;; Paredit for Lisp-like languages
(use-package paredit
  :hook ((racket-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . (lambda () (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp)))))

;; In Lisp interaction mode, remap C-j to eval and print.
;; (add-hook 'lisp-interaction-mode-hook
;;           #'(lambda () (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp)))

;; Use eglot for LSPs
(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :custom
  (eglot-report-progress nil "Prevent minibuffer spam")
  (eglot-autoshutdown t "Shutdown unused servers")
  (eglot-report-progress nil "Disable lsp server logs (Don't show lsp messages at the bottom")
                                        ; (eglot-events-buffer-config (:size 0 :format full) "No event buffers (Lsp server logs)")

  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  ;; Additional language servers
  (add-to-list 'eglot-server-programs
               `(racket-mode . ("racket" "-l" "racket-langserver")))
  :bind
  (:map prog-mode-map
   ("C-c ]" . flymake-goto-next-error)
   ("C-c [" . flymake-goto-prev-error)
   ("C-c d" . flymake-show-buffer-diagnostics)))

;; Hideshow mode for folding
(use-package emacs
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))))

;;; Version control with magit
(use-package magit
  :commands magit-status)

;; Review and revert chunks
(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

;;; Org mode
(use-package org
  :ensure nil
  :custom
  (org-directory "~/Org")
  (org-agenda-files '("~/Org"))
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
  (org-hierarchical-todo-statistics nil)
  (org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)")))
  (org-todo-keyword-faces '(("STARTED" . "cyan") ("CANCELED" . "red")))
  
  :hook
  (org-mode . org-indent-mode) ;; Indent text
  (org-mode . (lambda ()
                (set-fill-column 80)
                (auto-fill-mode)))
  ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
  ;; Otherwise, org-tempo is broken when you try to <s TAB...
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

;; (use-package org-superstar
;;   :after org
;;   :hook (org-mode . org-superstar-mode))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphases-markers t)
  (org-pretty-entities nil)
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?-)
  (org-ellipsis "...")

  :init
  (with-eval-after-load 'org (global-org-modern-mode))

  :config
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

(use-package org-tempo
  :ensure nil
  :after org)

;;; Which key
(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-idle-delay 0.5)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

;;; pomm
(use-package pomm
  :commands (pomm pomm-third-time))

(provide 'init)

;;; init.el ends here
