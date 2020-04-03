;;;; early-init.el

;;; On MacOS set a default frame size.
(if (string-equal system-tyle "darwin")
    (progn
      (add-to-list 'default-frame-alist '(width . 164))
      (add-to-list 'default-frame-alist '(height . 80))))

;;; Remove graphical items we do not use.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
