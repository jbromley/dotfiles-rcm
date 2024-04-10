;;; development.el - configure development tools for Emacs Bedrock -*- lexical-binding: t; -*-

;;; Commentary:

;; Contents:
;;
;;  - Built-in config for developers
;;  - Version Control
;;  - Common file types
;;  - Eglot, the built-in LSP client for Emacs

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Built-in config for developers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Set the languages we will use for tree-sitter
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown") 
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
	  (cmake-mode . cmake-ts-mode)
	  (c-mode . c-ts-mode)
	  (cpp-mode . cpp-ts-mode)
	  (elixir-mode . elixir-ts-mode)
	  ;; (erlang-mode . erlang-ts-mode)
	  (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
	  (rust-mode . rust-ts-mode)
	  (toml-mode . toml-ts-mode)
	  (yaml-mode . yaml-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Version Control
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Set exec-path
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If mise is in use, add all binary directories installed.
(require 'seq)

(let* ((mise-dir-regexp (expand-file-name "~/.local/share/mise/installs/[a-z0-9]+/[a-z0-9\\.\\-]+"))
       (mise-dirs (seq-filter (lambda (f) (not (file-symlink-p f)))
			     (file-expand-wildcards mise-dir-regexp t t))))
  (dolist (dir mise-dirs)
    (add-to-list 'exec-path (concat dir "/bin"))))

(when (eq system-type 'gnu/linux)
  (add-to-list 'exec-path "/opt/elixir-ls"))

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "Applications/Racket v8.11.1/bin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Common file types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; (use-package elixir-mode
;;   :ensure t)

(use-package elixir-ts-mode
  :ensure t)

(use-package erlang
  :ensure t
  :init (require 'erlang-start))

(use-package gleam-ts-mode
  :ensure nil 
  :load-path "~/.emacs.d/packages/gleam-mode/"
  :commands (gleam-ts-mode)
  :bind ("C-c g f" . gleam-format)
  :init (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(gleam-ts-mode . ("gleam" "lsp")))))

(use-package racket-mode
  :ensure t)

;; (use-package rust-mode
;;   :ensure t)

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :config (setq rustic-lsp-client 'eglot)
  :custom (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package sly
  :init
  (setq sly-lisp-implementations
	'((sbcl ("sbcl" "--core" "/opt/sbcl/sbcl-sly.core") :coding-system utf-8-unix)))
  :ensure t)

(use-package ligature
  :ensure t
  ;; :load-path "~/.emacs.d/packages/ligature.el"
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package vterm
  :ensure t)
  ;; :load-path "~/.emacs.d/packages/emacs-libvterm")

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Eglot, the built-in LSP client for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  :hook
  (((c-mode c++-mode python-mode elixir-mode gleam-ts-mode racket-mode) . eglot-ensure))
  :custom
  (eglot-send-changes-idle-time 0.1)
  :bind-keymap
  ("C-c e" . eglot-mode-map)
  :bind
  (:map eglot-mode-map ("C-c e f" . eglot-find-declaration))
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  ; (add-to-list 'eglot-server-programs '(elixir-mode . ("/opt/elixir-ls/language_server.sh"))))
  )

(provide 'development)
