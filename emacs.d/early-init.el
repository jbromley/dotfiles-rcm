;; early-init.el --- Basic settings for quick startup and convenience -*- no-byte-compile: t; lexical-binding: t; -*-
;; Author: Jay Bromley
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The emacs.d project is a customizable base that provides better Emacs
;; defaults and optimized startup, intended to serve as a solid foundation for
;; your vanilla Emacs configuration.

;;; Code:

;;; Variables
(defvar ui-features '()
  "List of user interface features to disable in minimal Emacs setup.

This variable holds a list Emacs UI features that can be enabled:
- `context-menu`: Enables the context menu in graphical environments.
- `tool-bar`: Enables the tool bar in graphical environments.
- `menu-bar`: Enables the menu bar in graphical environments.
- `dialogs`: Enables both file dialogs and dialog boxes.
- `tooltips`: Enables tooltips.

Each feature in the list corresponds to a specific UI component that can be
turned on.")

(defvar frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")

(defvar emacs-debug nil
  "Non-nil to enable debug.")

(defvar emacs-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

(defvar package-initialize-and-refresh t
  "Whether to automatically initialize and refresh packages.
When set to non-nil, Emacs will automatically call `package-initialize' and
`package-refresh-contents' to set up and update the package system.")

;; Garbage collection - none during startup, then adjust.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold emacs-gc-cons-threshold)))

;; Startup speed, annoyance suppression
(setq gc-cons-threshold most-positive-fixnum
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent
      ;; Silence stupid startup message
      inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(setq default-frame-alist '(;(fullscreen . maximized)
                            ;; Turn off scroll bars.
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)))

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq emacs-var-dir (expand-file-name "var/" user-emacs-directory)
      package-user-dir (expand-file-name "elpa" emacs-var-dir)
      user-emacs-directory emacs-var-dir)

;; Save customizations in their own file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Language environment
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Some features that are not represented as packages can be found in
;; `features', but this can be inconsistent. The following enforce consistency:
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features) ; no alternative
    (push 'harfbuzz features))
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))

;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 512 1024))  ; 512kb

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; Determine the state of bundled libraries using calc-loaddefs.el.
     ;; If compressed, retain the gzip handler in `file-name-handler-alist`.
     ;; If compiled or neither, omit the gzip handler during startup for
     ;; improved startup and package load time.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Ensure the new value persists through any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist
                                file-name-handler-alist)
    ;; Remember the old value to reset it as needed.
    (add-hook 'emacs-startup-hook
              (lambda ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite to preserve any changes made
                 ;; since startup.
                 (delete-dups (append file-name-handler-alist old-value))))
              101))

  (unless noninteractive
    (unless emacs-debug
      ;; Suppress redisplay and redraw during startup to avoid delays and
      ;; prevent flashing an unstyled Emacs frame.
      (setq-default inhibit-redisplay t
                    inhibit-message t)

      ;; Reset the above variables to prevent Emacs from appearing frozen or
      ;; visually corrupted after startup or if a startup error occurs.
      (defun minimal-emacs--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      inhibit-message nil)
        (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibited-vars-h))
      (add-hook 'post-command-hook #'minimal-emacs--reset-inhibited-vars-h -100)

      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (setq mode-line-format nil)))

      (put 'mode-line-format 'initial-value
           (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)

      (defun minimal-emacs--startup-load-user-init-file (fn &rest args)
        "Advice for startup--load-user-init-file to reset mode-line-format."
        (unwind-protect
            (progn
              ;; Start up as normal
              (apply fn args))
          ;; If we don't undo inhibit-{message, redisplay} and there's an
          ;; error, we'll see nothing but a blank Emacs frame.
          (setq-default inhibit-message nil)
          (unless (default-toplevel-value 'mode-line-format)
            (setq-default mode-line-format
                          (get 'mode-line-format 'initial-value)))))

      (advice-add 'startup--load-user-init-file :around
                  #'minimal-emacs--startup-load-user-init-file))

    ;; Without this, Emacs will try to resize itself to a specific column size
    (setq frame-inhibit-implied-resize t)

    ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
    ;; No second pass of case-insensitive search over auto-mode-alist.
    (setq auto-mode-case-fold nil)

    ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
    ;; dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for a modest performance boost.
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)

    ;; Give up some bidirectional functionality for slightly faster re-display.
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information about GNU Emacs..." message at startup
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Suppress the vanilla startup screen completely. We've disabled it with
    ;; `inhibit-startup-screen', but it would still initialize anyway.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch buffer in
    ;; `fundamental-mode'
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless emacs-debug
      ;; Unset command line options irrelevant to the current OS. These options
      ;; are still processed by `command-line-1` but have no effect.
      (unless (eq system-type 'darwin)
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil)))))

;;; Native compilation and Byte compilation

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-jit-compilation t
          native-comp-deferred-compilation t  ; Obsolete since Emacs 29.1
          package-native-compile t)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors (or emacs-debug 'silent)
      native-comp-warning-on-missing-source emacs-debug)

(setq debug-on-error emacs-debug
      jka-compr-verbose emacs-debug)

(setq byte-compile-warnings emacs-debug
      byte-compile-verbose emacs-debug)

;;; UI elements

(setq frame-title-format frame-title-format
      icon-title-format frame-title-format)

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(unless (memq 'menu-bar ui-features)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil)))

(unless (daemonp)
  (unless noninteractive
    (when (fboundp 'tool-bar-setup)
      ;; Temporarily override the tool-bar-setup function to prevent it from
      ;; running during the initial stages of startup
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file
          (:after (&rest _) minimal-emacs-setup-toolbar)
        (advice-remove #'tool-bar-setup #'ignore)
        (when tool-bar-mode
          (tool-bar-setup))))))
(unless (memq 'tool-bar ui-features)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(unless (memq 'tooltips ui-features)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(unless (memq 'dialogs ui-features)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil))

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;;; package.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ;("stable" . 70)
                                                      ("melpa"  . 0)))

(provide 'early-init)

;;; early-init.el ends here
