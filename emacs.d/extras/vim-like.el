;;; vim-like.el - configure Vim key bindings for Emacs Bedrock -*- lexical-binding: t; -*-

;;; Commentary:

;; Contents:
;;
;;  - Core Packages

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Core Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil: vi emulation
(use-package evil
  :ensure t

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)

  ;; Enable this if you want C-u to scroll up, more like pure Vim
  ;(setq evil-want-C-u-scroll t)

  :config
  (evil-mode)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs))

(provide 'vim-like)
