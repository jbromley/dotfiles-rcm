;; ocaml-setup.el --- <description> -*-coding: utf-8 -*-

;; Copyright (C) 2024 J. Bromley <jbromley@gmail.com>

;; Author:      J. Bromley <jbromley@gmail.com>
;; Maintainer:  J. Bromley <jbromley@gmail.com>
;; Created:     <2024-12-13>
;; Version:     0.1
;; Keywords:    languages,text,tools
;; URL:         http://github.com/jbromley/dotfiles-rcm

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information.

;;; Depends:

;; tuareg,merlin,ocp-index,ocp-indent,utop

;;; Install:

;; Put this file along your Emacs-Lisp `load-path' and add following
;; into your ~/.emacs startup file.
;;
;;      <at standards TAB position explain what lisp code is needed>
;;      (autoload 'example-install "example" "" t)
;;      (autoload 'example-mode    "example" "" t)

;;; Commentary:

;; OPAM provides a user-setup package that will add the appropriate code
;; to one's Emacs initialization files to be able to load tuareg, merlin,
;; ocp-index, ocp-indent, and utop, but it does not autoload these, so
;; Emacs startup is slowed down. This package uses use-package to be able
;; to defer loading until these packages are needed.

;;; Change Log:

;; See git.

;;; Code:

;; See:
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Library-Headers
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Autoload
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Customization

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

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

(use-package tuareg
  :defer t
  :commands (tuareg-mode tuareg-run-ocaml)
  :init
  (add-to-list 'load-path (concat opam-share "/tuareg"))
  (load "tuareg-site-file")
  (require 'auto-complete nil t)
  :config
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . tuareg-mode))
  (add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))
  (add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))
  (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmxs" ".cmt" ".cmti" ".cmi" ".annot"))
    (add-to-list 'completion-ignored-extensions ext))
  :hook
  (tuareg-mode . (lambda ()
                   (company-mode)
                   (defalias 'auto-complete 'company-complete))))

(use-package ocp-indent
  :defer t
  :autoload (ocp-setup-indent ocp-indent-caml-mode-setup)
  :hook
  (tuareg-mode . ocp-setup-indent)
  (caml-mode . ocp-setup-indent))

(use-package ocp-index
  :defer t
  :commands ocp-index-mode
  :hook
  (tuareg-mode . ocp-index-mode)
  (caml-mode . ocp-index-mode))

(use-package merlin
  :defer t
  :init
  (defcustom ocp-index-use-auto-complete nil
    "Use auto-complete with ocp-index (disabled by default by opam-user-setup because merlin is in use)"
    :group 'ocp_index)
  (defcustom merlin-ac-setup 'easy
    "Use auto-complete with merlin (enabled by default by opam-user-setup)"
    :group 'merlin-ac)
  :hook
  (tuareg-mode . merlin)
  (caml-mode . merlin))

(use-package utop
  :defer t
  :commands (utop utop-minor-mode)
  :hook
  (tuareg-mode . utop-minor-mode)
  (caml-mode . utop-minor-mode))

(use-package ocamlformat
  :defer t
  :commands (ocamlformat)
  :hook
  (tuareg-mode . (lambda ()
                   (define-key tuareg-mode-map (kbd "C-c C-f") #'ocamlformat))))
(provide 'ocaml-setup)

;;; example.el ends here.
