;;; ocaml.el --- Set up Emacs for OCaml -*- lexical-binding: t -*-

;; Author: J. Bromley
;; Package-Requires: (tuareg merlin ocp-indent)
;; Keywords: ocaml, tuareg, merlin, indent
;; URL: https://opam.ocaml.org/packages/user-setup/

;;; Commentary:

;; This package installs and configures Tuareg, Merlin, and ocp-indent to
;; facilitate OCaml programming in Emacs.

;; Base configuration for OPAM
(defun opam-shell-command-to-string (command)
  "Similar to shell-command-to-string, but returns nil unless the process
  returned 0, and ignores stderr (shell-command-to-string ignores return value)"
  (let* ((return-value 0)
         (return-string
          (with-output-to-string
            (setq return-value
                  (with-current-buffer standard-output
                    (process-file shell-file-name nil '(t nil) nil
                                  shell-command-switch command))))))
    (if (= return-value 0) return-string nil)))

(defun opam-update-env (switch)
  "Update the environment to follow current OPAM switch configuration"
  (interactive
   (list
    (let ((default
            (car (split-string (opam-shell-command-to-string "opam switch show --safe")))))
      (completing-read
       (concat "opam switch (" default "): ")
       (split-string (opam-shell-command-to-string "opam switch list -s --safe") "\n")
       nil t nil nil default))))
  (let* ((switch-arg (if (= 0 (length switch)) "" (concat "--switch " switch)))
         (command (concat "opam env --safe --sexp " switch-arg))
         (env (opam-shell-command-to-string command)))
    (when (and env (not (string= env "")))
      (dolist (var (car (read-from-string env)))
        (setenv (car var) (cadr var))
        (when (string= (car var) "PATH")
          (setq exec-path (split-string (cadr var) path-separator)))))))

(opam-update-env nil)

(defvar opam-share
  (let ((reply (opam-shell-command-to-string "opam var share --safe")))
    (when reply (substring reply 0 -1))))

;(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

(use-package company)

(use-package tuareg
  :custom
  (tuareg-match-patterns-aligned t)
  :hook
  ((tuareg-mode . (lambda ()
                    (company-mode)
                    (defalias 'auto-complete 'company-complete)))
   (tuareg-mode . (lambda ()
                    (setq-local comment-style 'multi-line
                                commend-continue "   ")))))

(use-package ocp-indent
  :autoload (ocp-setup-indent ocp-indent-caml-mode-setup)
  :hook
  ((tuareg-mode . ocp-setup-indent)
   (caml-mode . ocp-indent-caml-mode-setup)))

(use-package ocp-index
  :load-path (lambda () (concat opam-share "/emacs/site-lisp"))
  :autoload ocp-index-mode
  :hook
  ((tuareg-mode . ocp-index-mode)
   (caml-mode . ocp-index-mode)))

(use-package merlin
  :config
  (set-face-background 'merlin-type-face "skyblue")
  :bind
  (:map merlin-mode-map
        ("C-c <up>" . merlin-type-enclosing-go-up)
        ("C-c <down>" . merlin-type-enclosing-go-down))
  :hook
  ((tuareg-mode . merlin-mode)
   (caml-mode . merlin-mode)
   (merlin-mode . company-mode)))

(use-package utop
  :autoload (utop utop-minor-mode)
  :hook (tuareg-mode . utop-minor-mode))

(use-package ocamlformat
  :bind
  (:map tuareg-mode-map
        ("C-c C-f" . ocamlformat)))

(use-package dune)
