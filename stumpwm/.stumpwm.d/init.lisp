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
      *mouse-focus-policy* :sloppy)

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

;; Gaps
(setf swm-gaps:*inner-gaps-size* 15
      swm-gaps:*outer-gaps-size* 5)

(run-commands "toggle-gaps") ; activate gaps


;; (defmacro def-shell-key (map key command)
;;   `(define-key ,map (kbd ,key) (run-shell-command ,command)))
;; Bindings

;; launcher~
(define-key *root-map* (kbd "F1") "run-shell-command rofi_run -r")

;; core

(define-key *root-map* (kbd "M-w") "run-shell-command exo-open --launch WebBrowser")
(define-key *root-map* (kbd "M-f") "run-shell-command exo-open --launch FileManager")
(define-key *root-map* (kbd "M-t") "run-shell-command exo-open --launch TerminalEmulator")
(define-key *root-map* (kbd "Return") "run-shell-command exo-open --launch TerminalEmulator")

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

(defcommand run-or-raise-firefox () ()
   (run-or-raise-prefer-group "firefox" "Firefox"))

(defcommand run-or-raise-emacs () ()
  (run-or-raise-prefer-group "emacsclient -create-frame
  --alternate-editor=\"\"" "Emacs"))

(defcommand run-or-raise-urxvt () ()
  (run-or-raise-prefer-group "urxvt" "URxvt"))

(define-key *root-map* (kbd "C-f") "run-or-raise-firefox")

(define-key *root-map* (kbd "C-e") "run-or-raise-emacs")

(define-key *root-map* (kbd "C-c") "run-or-raise-urxvt")
(define-key *root-map* (kbd "c") "run-or-raise-urxvt")

  
;; logout script
(define-key *root-map* (kbd "M-k") "run-shell-command rofi_run -l")


;; Screenshot
;; (define-key *root-map* (kbd "Print")
;;   "run-shell-command scrot '%S.png' -e 'mv $f $$(xdg-user-dir
;;   PICTURES)/ArchLabs-%S-$wx$h.png ; feh $$(xdg-user-dir
;;   PICTURES)/ArchLabs-%S-$wx$h.png")

;; audio
(define-key *top-map* (kbd "XF86AudioPlay")
  "run-shell-command playerctl play-pause")
(define-key *top-map* (kbd "XF86AudioNext")
  "run-shell-command playerctl next")
(define-key *top-map* (kbd "XF86AudioPrev")
  "run-shell-command playerctl previous")
(define-key *top-map* (kbd "XF86AudioStop")
  "run-shell-command playerctl stop")
(define-key *top-map* (kbd "XF86AudioMute")
  "run-shell-command pamixer -t")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")
  "run-shell-command pamixer -i 5")
(define-key *top-map* (kbd "XF86AudioLowerVolume")
  "run-shell-command pamixer -d 5")

;; backlight define-key
(define-key *top-map* (kbd "XF86MonBrightnessUp")
  "run-shell-command xbacklight -inc 10")
(define-key *top-map* (kbd "XF86MonBrightnessDown")
  "run-shell-command xbacklight -dec 10")

;; Autostart

; run with reload
(run-shell-command "emacs --daemon") ; start emacs --daemon
(run-shell-command "polybar stumpwm-bar --reload")
(run-shell-command "nitrogen --restore")
(run-shell-command "dunst")
(run-shell-command "xrdb -load ~/.Xresources")
(run-shell-command "xsetroot -cursor_name left_ptr") ; set normal cursor
(run-shell-command "setxkbmap -option compose:rctrl") ; set compose key
(run-shell-command "redshift")
(run-shell-command "xautolock -corners +--- -time 3 -locker \"$HOME/cfg/locker/locker\" -killer \"set dpms force off\"") ; start autolock
; If you have a numpad you may want to enable this
; (run-shell-command "numlockx on")

; run once
(run-shell-command "xfsettingsd")
(run-shell-command "compton")


; needed for super to launch rofi through ksuperkey, see ~/.xprofile
(define-key *root-map* (kbd "M-r") "run-shell-command rofi_run -r")
