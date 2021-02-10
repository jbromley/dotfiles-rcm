;;;; early-init.el

;;; Remove graphical items we do not use.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))
