;;; early-init.el --- pre-startup Emacs settings

;;; Commentary:
;;; Code:

;; Increase garbage collection threshold.
(setq gc-cons-threshold 16000000)

;; Remove graphical items we do not use.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; Set default frame size.
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))

(provide 'early-init)
;;; early-init.el ends here
