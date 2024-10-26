;;; post-early-init.el --- early setup -*- no-byte-compile: t; lexical-binding: t; -*-

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("melpa"  . 99)
                                                      ("gnu"    . 90)
                                                      ("nongnu" . 80)))
