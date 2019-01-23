;========================================================
; SETUP org babel languages
;========================================================
(org-babel-do-load-languages
 'org-babel-load-languages '((ruby . t) (shell . t)))

;========================================================
; SETUP ORG MODE TMUX
;========================================================
(load "ob-tmux")
(require 'ob-tmux)
(setq org-babel-default-header-args:tmux
  '((:results . "silent")		;
    (:session . "default")	; The default tmux session to send code to
    (:socket  . nil)              ; The default tmux socket to communicate with
    ;; You can use "xterm" and "gnome-terminal".
    ;; On mac, you can use "iterm" as well.
    (:terminal . "iterm")))

;; The tmux sessions are prefixed with the following string.
;; You can customize this if you like.
(setq org-babel-tmux-session-prefix "ob-")

;; Finally, if your tmux is not in your $PATH for whatever reason, you
;; may set the path to the tmux binary as follows:
(setq org-babel-tmux-location "/usr/local/bin/tmux")
