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

(run-commands "toggle-gaps") ; activate gaps

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

;; Commands
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

(defun current-frame-windows ()
  "return a list of the windows in the active frame"
  (let* ((g (current-group))
	 (f (tile-group-current-frame g)))
    (frame-windows g f)))

(defun run-or-raise-in-frame (cmd class)
  "Raise the first window of the class @class if it is part of the
windows in the current frame. If not run the shell command @cmd"
  (let* ((winlist (current-frame-windows))
	 (win (find-if (lambda (w)
			 (apply 'window-matches-properties-p w
				`(:class ,class))) winlist)))
    (if win
	(focus-all win)
	(run-shell-command cmd))))

(defcommand run-or-raise-firefox () ()
  (run-or-raise-in-frame "firefox" "Firefox"))

(defcommand run-or-raise-emacs () ()
  (run-or-raise-in-frame "emacsclient -create-frame
  --alternate-editor=\"\"" "Emacs"))

(defcommand run-or-raise-urxvt () ()
  (run-or-raise-in-frame "urxvt" "URxvt"))

(define-key *root-map* (kbd "C-f") "run-or-raise-firefox")

(define-key *root-map* (kbd "C-e") "run-or-raise-emacs")

(define-key *root-map* (kbd "C-c") "run-or-raise-urxvt")
(define-key *root-map* (kbd "c") "run-or-raise-urxvt")
 
;; Init
(run-shell-command "xsetroot -cursor_name left_ptr") ; set normal cursor
(run-shell-command "setxkbmap -option compose:rctrl") ; set compose key
(run-shell-command "sh ~/.fehbg") ; wallpaper setting
(run-shell-command "xset b off") ; disable beep
(run-shell-command "polybar screen --reload")
(run-shell-command "redshift")
(run-shell-command "compton")
(run-shell-command "mpd")
;; autolock system
(run-shell-command "emacs --daemon") ; start emacs --daemon
(run-shell-command "xautolock -corners +--- -time 3 -locker \"$HOME/cfg/locker/locker\" -killer \"set dpms force off\"") ; start autolock

;; change pull behaviour, i want to place windows for a reason at a
;; position so C-t C-n and C-t C-p should cycle through the windows in
;; the frame and pull nothing
(define-key *root-map* (kbd "n") "next-in-frame")
(define-key *root-map* (kbd "p") "prev-in-frame")




;; NOT FUNCTIONING
;; NEEDS FURTHER HACKING
;; LOOK AT tile-group 
;; (defun opposite-direction (direction)
;;   (cond ((string= direction "up") "down")
;; 	((string= direction "down") "up")
;; 	((string= direction "right") "left")
;; 	((string= direction "left") "right")))


;; (defcommand (exchange-windows tile-group) (dir) ()
;;   ""
;;     (run-commands (concatenate 'string "move-window" " " dir))
;;     (if (> (length (current-frame-windows)) 1)
;; 	(let ((opp-dir (opposite-direction dir)))
;; 	  (run-commands "next-in-frame"
;; 			(concatenate 'string "move-window" " "
;; 				     opp-dir)
;; 			(concatenate 'string "move-focus" " "
;; 				     dir)))))
 
;; (define-key *root-map* (kbd "S-Up") "exchange-windows up")
;; (define-key *root-map* (kbd "S-Down") "exchange-windows down")
;; (define-key *root-map* (kbd "S-Right") "exchange-windows right")
;; (define-key *root-map* (kbd "S-Left") "exchange-windows left")
