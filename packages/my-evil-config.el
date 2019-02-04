;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-leader 
             :config
             (require 'evil-leader)
             (global-evil-leader-mode)
             (evil-leader/set-leader "SPC")
             (evil-leader/set-key
               ;File
               "ff" 'helm-find-files
               "fy" 's/show-buffer-file-path
               "fY" 's/show-buffer-file-name
               "ft" 'neotree-project-dir-toggle
               "fes" 'sync-config
               "fw" 'save-buffer
               "fx" 'evil-quit
               ;Projects
               "pf" 'helm-projectile-find-file
               "pg" 'find-my-tag
               "pG" 'projectile-regenerate-tags
               "pD" 'projectile-dired
               "pr" 'projectile-recentf
               "pI" 'projectile-invalidate-cache
               "pp" 'projectile-switch-project
               ;Search
               "saf" 'helm-do-ag
               "saa" 'helm-do-ag-this-file
               "sf" 'helm-do-ag
               "sp" 'helm-do-ag-project-root
               ;Toggle
               "tn" 'display-line-numbers-mode
               "ti" 'indent-guide-toggle
               "ts" 'flycheck-toggle
               "t C-c" 'toggle-camelcase-motion
               "thp" 'highlight-parentheses-mode
               ;Buffer
               "bb" 'helm-mini
               ;Zoom(Folding)
               "zc" 'toggle-selective-display
               ;Applications
               "ait" 'emacs-init-time
               ;Git
               "gb" 'magit-blame
               "gm" 'magit-dispatch-popup
               "gt" 'git-timemachine
               "gs" 'magit-status
               "gfh" 'magit-log-buffer-file
               "gfb" 'magit-log-all-branches
               "gll" 'git-link
               ;Jump
               "jj" 'evil-avy-goto-char
               "jJ" 'evil-avy-goto-char-2
               "jl" 'evil-avy-goto-line
               "jw" 'evil-avy-goto-word-or-subword-1
               ;Error
               "ef" 'force-run-flycheck
               "el" 'flycheck-list-errors
               "en" 'next-error
               "eN" 'previous-error
               "ep" 'previous-error
               "ec" 'flycheck-clear
               ;Layouts
               "lL" 'persp-load-state-from-file
               "ls" 'persp-save-state-to-file
               ;Windows
               "w/" 'split-window-right
               "w-" 'split-window-below
               "wv" 'split-window-right
               "ws" 'split-window-below
               "wd" 'delete-window
               "wm" 'toggle-maximize-buffer
               "wj" 'evil-window-down
               "wk" 'evil-window-up
               "wh" 'evil-window-left
               "wl" 'evil-window-right
               ;Text
               "xU" 'upcase-region
               "xu" 'downcase-region
               "xs" 'toggle-camelcase-underscores
               "xdw" 'delete-trailing-whitespace)
             )

(use-package evil
             :config
             (require 'evil)
             (evil-mode t)
             (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
             (use-package evil-surround
                          :config
                          (progn
                            (global-evil-surround-mode 1)
                            (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
                            (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete))))
             )

;; evil-nerd-commenter
(use-package evil-nerd-commenter
             :init
             (with-eval-after-load "evil"
                                   (define-key evil-visual-state-map "gc" 'evilnc-comment-or-uncomment-lines)
                                   ))

;; from https://gist.github.com/3402786
(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun sync-config ()
  (interactive)
  (load-file user-init-file)
  )

(defun find-my-tag ()
  (interactive)
  (if (not(fboundp 'my-find-tag-and-load-config))
    (progn
      (load "my-tags-config")
      (my-find-tag-and-load-config))
    (my-find-tag-and-load-config))
  )
