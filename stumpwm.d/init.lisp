;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; Load modules
(setf *module-dir* #p"/home/jay/.stumpwm.d/contrib/")
(init-load-path *module-dir*)
(load-module "ttf-fonts")
(load-module "notify")
(load-module "stumptray")

;; Change appearance.
(setq *message-window-gravity* :top
      *message-window-padding 2
      *input-window-gravity* :top
      *window-border-style* :tight
      *normal-border-width* 2)

;; Message bar and window decorations
(set-msg-border-width 2)
(set-font (make-instance 'xft:font :family "Menlo" :subfamily "Regular" :size 10))


;; Modeline
(setf *screen-mode-line-format* "%n | %W ^> %d | %T"
      *time-modeline-string* "%a, %b %e, %I:%M %P")

;; Other
(setf *mouse-focus-policy* :sloppy)

;; change the prefix key to something else
(set-prefix-key (kbd "C-t"))

;; Prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; Use multimedia controls for Spotify and volume
(define-key *top-map* (kbd "XF86AudioPlay") "exec /home/jay/.local/bin/spcli play")
(define-key *top-map* (kbd "XF86AudioNext") "exec /home/jay/.local/bin/spcli next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec /home/jay/.local/bin/spcli prev")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec /home/jay/.local/bin/pavol down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec /home/jay/.local/bin/pavol up")
(define-key *top-map* (kbd "XF86AudioMute") "exec /home/jay/.local/bin/pavol mute")

;; Key mappings
(define-key *root-map* (kbd "c") "exec urxvtc")
(define-key *root-map* (kbd "C-c") "exec urxvtc -e tmux")
(define-key *root-map* (kbd "d") "exec evince")
(define-key *root-map* (kbd "b") "exec firefox")
(define-key *root-map* (kbd "B") "colon1 exec firefox http://www.")
(define-key *root-map* (kbd "C-s") "colon1 exec urxvtc -e ssh ")
(define-key *root-map* (kbd "C-l") "exec i3lock -c 14041e -d -e")
(define-key *root-map* (kbd "C-m") "exec rofi -show run")
(define-key *root-map* (kbd "C-w") "exec rofi -show window")

;; Group creation and navigation
(define-key *root-map* (kbd "C-Right") "gnext")
(define-key *root-map* (kbd "C-Left") "gprev")

;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
	       (substitute #\+ #\Space search)
	       (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "ddg" "firefox http://www.duckduckgo.com/?q=")

;;; Define window placement policy.

;; Clear rules.
(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
;; (define-frame-preference "Default"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
;;   (1 t nil :class "XTerm"))

(mode-line)
