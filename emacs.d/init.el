;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: J. Bromley
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The minimal-emacs.d project is a customizable base that provides better Emacs
;; defaults and optimized startup, intended to serve as a solid foundation for
;; your vanilla Emacs configuration.

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

;; Ensure the 'use-package' package is installed and loaded

;;; Minibuffer
;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Misc

;; switch-to-buffer runs pop-to-buffer-same-window instead
(setq switch-to-buffer-obey-display-actions t)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(setq whitespace-line-column nil)  ; whitespace-mode

;; Reduce the default value of 9 to simplify the font-lock keyword,
;; aiming to improve performance. This package helps differentiate
;; nested delimiter pairs, particularly in languages with heavy use of
;; parentheses.
(setq rainbow-delimiters-max-face-count 5)

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error)

(setq truncate-string-ellipsis "…")

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))

;;; Files

;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward)

(setq mouse-yank-at-point t)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider` does not.
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

(add-hook 'after-init-hook #'window-divider-mode)

;;; Backup files

;; Avoid generating backups or lockfiles to prevent creating world-readable
;; copies of files.
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files

;;; Auto save
;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default t)

;; Do not auto-disable auto-save after deleting large chunks of
;; text. The purpose of auto-save is to provide a failsafe, and
;; disabling it contradicts this objective.
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)

;;; recentf
;; `recentf' is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(setq recentf-max-saved-items 300 ; default is 20
      recentf-auto-cleanup 'mode)

;;; saveplace
;; `save-place-mode` enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-limit 600)

;;; savehist
;; `savehist` is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(setq history-length 300
      savehist-save-minibuffer-history t)  ;; Default

;;; Frames and windows

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

;;; Scrolling
;; Enables faster scrolling through unfontified regions. This may result in
;; brief periods of inaccurate syntax highlighting immediately after scrolling,
;; which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keeps screen position if the scroll command moved it vertically out of the
;; window.
(setq scroll-preserve-screen-position t)

;;; Mouse

;; Emacs 29
(when (memq 'context-menu ui-features)
  (when (and (display-graphic-p) (fboundp 'context-menu-mode))
    (add-hook 'after-init-hook #'context-menu-mode)))

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends excessive time recentering the screen when the cursor
      ;; moves more than N lines past the window edges (where N is the value of
      ;; `scroll-conservatively`). This can be particularly slow in larger files
      ;; during extensive scrolling. If `scroll-conservatively` is set above
      ;; 100, the window is never automatically recentered. The default value of
      ;; 0 triggers recentering too aggressively. Setting it to 10 reduces
      ;; excessive recentering and only recenters the window when scrolling
      ;; significantly off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by preventing automatic adjustments to
      ;; `window-vscroll' for unusually long lines. Setting
      ;; `auto-window-vscroll' it to nil also resolves the issue of random
      ;; half-screen jumps during scrolling.
      auto-window-vscroll nil
      ;; Mouse
      mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 1)

;;; Cursor
;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
;; Additionally, it can cause freezing, especially on macOS, for users with
;; customized and colored cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;;; Annoyances

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; This controls how long Emacs will blink to show the deleted pairs with
;; `delete-pair'. A longer delay can be annoying as it causes a noticeable pause
;; after each deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

;;; Indent and formatting
(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enable indentation and completion using the TAB key
(setq-default tab-always-indent nil)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines, which is useful for writing
;; longer comments or docstrings that span multiple lines.
(setq comment-multi-line t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Disable the obsolete practice of end-of-line spacing from the
;; typewriter era.
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or
;; more non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

;;; Mode line

;; Setting `display-time-default-load-average' to nil makes Emacs omit the load
;; average information from the mode line.
(setq display-time-default-load-average nil)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;;; Filetype

;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)

(setq dired-clean-confirm-killing-deleted-buffers nil
      dired-recursive-deletes 'top
      dired-recursive-copies  'always
      dired-create-destination-dirs 'ask)

;;; Font / Text scale

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

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
   ("C-M-z" . zap-up-to-char))

  :hook
  ((after-init . global-auto-revert-mode)
   (after-init . recentf-mode)
   (after-init . savehist-mode)
   (after-init . save-place-mode)))

;; Window navigation
(windmove-default-keybindings 'shift)

;; Appearance
;; (add-to-list 'default-frame-alist '(alpha-background . 50))
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))
;; (setq-default line-spacing 0.05)

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package almost-mono-themes)

(use-package tok-theme)

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
