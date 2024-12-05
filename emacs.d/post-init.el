;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Activate some utility Emacs functions
(use-package emacs
  :custom
  ; (delete-selection-mode t)   ;; Select text and delete it by typing.
  ; (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)        ;; Turns on automatic parens pairing
  (blink-cursor-mode t)         ;; Blink cursor
  (dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed
  (recentf-mode t) ;; Enable recent file mode
  (global-visual-line-mode t)           ;; Enable truncated lines
  (display-line-numbers-type t)         ;; Absolute line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers
  (mouse-wheel-progressive-speed nil)   ;; Disable progressive speed when scrolling
  (scroll-conservatively 10)            ;; Smooth scrolling
  (scroll-margin 2)
  (setq-default indent-tabs-mode nil)

  :bind
  (;([escape] . keyboard-escape-quit)
   ("C-+" . text-scale-increase)
   ("C--" . text-scale-decrease)
   ("C-z" . zap-to-char)
   ("C-M-z" . zap-up-to-char))

  :hook
  ((after-init . global-auto-revert-mode)
   (after-init . recentf-mode)
   (after-init . savehist-mode)
   (after-init . save-place-mode)
   ; (after-init . fido-vertical-mode)
   (prog-mode . (lambda () (hs-minor-mode t)))))

;; ;; Window navigation
(windmove-default-keybindings 'shift)

;; ;; Appearance
;; (add-to-list 'default-frame-alist '(alpha-background . 50))
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))
;; (setq-default line-spacing 0.05)

(use-package dracula-theme)

(use-package ef-themes
  :defer t)

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
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
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

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

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
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml/src" nil nil))
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
        (ocaml-mode . ocaml-ts-mode)
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

;; OCaml setup 
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

;; Paredit for Lisp-like languages
(use-package paredit
  :hook ((racket-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . (lambda () (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp)))))

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
               ;; `(ocaml-ts-mode . ("ocamllsp" "--stdio"))
               `(racket-mode . ("racket" "-l" "racket-langserver")))
  :bind
  (:map prog-mode-map
   ("C-c ]" . flymake-goto-next-error)
   ("C-c [" . flymake-goto-prev-error)
   ("C-c d" . flymake-show-buffer-diagnostics)))

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

;;; Markdown mode
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

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

;;; hardtime
;; (use-package hardtime
;;   :init
;;   (unless (package-installed-p 'hardtime)
;;     (package-vc-install
;;      '(hardtime
;;        :vc-backend Git
;;        :url "https://github.com/ichernyshovvv/hardtime.el"
;;        :branch "master")))
;;   :config
;;   (hardtime-mode))

;;; chatgpt-shell

(use-package chatgpt-shell
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pick-first-password :host "api.openai.com")))))
