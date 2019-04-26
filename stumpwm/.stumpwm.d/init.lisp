;; -*- lisp -*-

(in-package :stumpwm)

;; Start swank
(require :swank)
(swank-loader:init)
(swank:create-server :port 4444
                     :style swank:*communication-style*
                     :dont-close t)

;; Basics
(setf *startup-message* NIL
      *suppress-abort-messages* t
      *shell-program* (getenv "SHELL"))

(defvar *message-filters* '("Group dumped")
  "Don't show these messages.")

(defun message (fmt &rest args)
  "Overwritten message function to allow filters"
  (let ((msg-string (apply 'format nil fmt args)))
    (unless (member msg-string *message-filters* :test #'string=)
      (echo-string (current-screen) msg-string))))

;; Modules
(load-module "ttf-fonts")
(load-module "amixer")
(load-module "swm-gaps")
(load-module "kbd-layouts")
(load-module "command-history")

(setf kbd-layouts:*caps-lock-behavior* :ctrl)
(kbd-layouts:keyboard-layout-list "us" "in -variant hin-wx")

(setf swm-gaps:*inner-gaps-size* 10
      swm-gaps:*outer-gaps-size* 15)

;; Looks
(set-font (make-instance 'xft:font
			 :family "FantasqueSansMono Nerd Font"
			 :subfamily "Regular"
			 :size 12))

(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :thin
      *message-window-padding* 10
      *maxsize-border-width* 5
      *normal-border-width* 5
      *transient-border-width* 2
      stumpwm::*float-window-border* 2
      stumpwm::*float-window-title-height* 5
      *mouse-focus-policy* :click)

(set-normal-gravity :center)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

(set-bg-color "#222222")
(set-fg-color "#fafafa")
(set-border-color "#222222")
(set-focus-color "#a47dd0")
(set-unfocus-color "#fafafa")
(set-win-bg-color "#fafafa")
(set-float-focus-color "#fafafa")
(set-float-unfocus-color "#fafafa")

(setf *colors* (list "#1c1e1f"      ; 0 black
                     "#ff6a6a"      ; 1 red
                     "#66cd00"      ; 2 green
                     "#ffd700"      ; 3 yellow
                     "#4f94cd"      ; 4 blue
                     "#c6e2ff"      ; 5 magenta
                     "#00cdcd"      ; 6 cyan
                     "#ffffff"))    ; 7 white


(defun shift-windows-forward (frames win)
  "Exchange windows through cycling frames."
  (when frames
    (let ((frame (car frames)))
      (shift-windows-forward (cdr frames)
                             (stumpwm::frame-window frame))
      (when win
        (stumpwm::pull-window win frame)))))

(defun window-cls-present-p (win-cls &optional all-groups)
  "Tell if a window (by class) is present"
  (let ((windows (group-windows (if all-groups (current-screen) (current-group)))))
    (member win-cls (mapcar #'window-class windows) :test #'string-equal)))

(defun run-or-raise-prefer-group (cmd win-cls)
  "If there are windows in the same class, cycle in those. Otherwise call
run-or-raise with group search t."
  (if (window-cls-present-p win-cls)
      (run-or-raise cmd `(:class ,win-cls) nil T)
      (run-or-raise cmd `(:class ,win-cls) T T)))

;; Commands
(defcommand run-or-raise-firefox () ()
  (run-or-raise-prefer-group "firefox" "Firefox"))

(defcommand raise-brightness () ()
  (run-shell-command "light -A 5"))

(defcommand lower-brightness () ()
  (run-shell-command "light -U 5"))

(amixer::defvolcontrol amixer-Master-5- "Master" "5%-")
(amixer::defvolcontrol amixer-Master-5+ "Master" "5%+")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "raise-brightness")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "lower-brightness")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-5-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-5+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle")

(defcommand urxvt () ()
  (run-shell-command "urxvt"))

(define-key *root-map* (kbd "C-f") "run-or-raise-firefox")
(define-key *root-map* (kbd "c") "urxvt")
(define-key *root-map* (kbd "C-c") "urxvt")

(defcommand emacs () ()
  (run-shell-command "emacsclient -create-frame --alternate-editor=\"\""))

(define-key *root-map* (kbd "C-e") "emacs")
(define-key *root-map* (kbd "e") "emacs")


;; Init

(run-shell-command "setxkbmap -option compose:rctrl")
(run-shell-command "sh ~/.fehbg")
(run-shell-command "xset b off")
(run-shell-command "polybar screen --reload")
(run-shell-command "redshift")
(run-shell-command "compton")
(run-shell-command "mpd")
;; autolock system
(run-shell-command "emacs --daemon")
(run-shell-command "xautolock -corners +--- -time 3 -locker \"$HOME/cfg/locker/locker\" -killer \"set dpms force off\"")


