;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :config
  (require 'general)
  (general-evil-setup t)
  (general-override-mode)
  (defmacro w/map-main-leader-key (leader-key)
    (list 'general-define-key
          ':states ''(normal visual insert emacs)
          ':prefix leader-key
          ':keymaps ''override
          ':non-normal-prefix "C-m"
          ;helm M-x
          "SPC" ''w/helm-M-x
          ;File
          "f" '(list ':ignore t ':which-key "Files")
          "ff" ''helm-find-files
          "fy" ''s/show-buffer-file-path
          "fY" ''s/show-buffer-file-name
          "ft" ''neotree-project-dir-toggle
          "fw" ''save-buffer
          "fx" ''evil-quit
          "fe" '(list ':ignore t ':which-key "emacs")
          "fes" ''sync-config
          ;Projects
          "p" '(list ':ignore t ':which-key "projects")
          "pf" ''helm-projectile-find-file
          "pg" ''find-my-tag
          "pG" ''projectile-regenerate-tags
          "pD" ''w/projectile-dired
          "pr" ''projectile-recentf
          "pI" ''projectile-invalidate-cache
          "pp" ''w/projectile-switch-project
          ;Search
          "s" '(list ':ignore t ':which-key "search/symbol")
          "sf" ''helm-do-ag
          "sp" ''helm-do-ag-project-root
          "sa" '(list ':ignore t ':which-key "ag")
          "saf" ''helm-do-ag
          "saa" ''helm-do-ag-this-file
          ;Toggle
          "t" '(list ':ignore t ':which-key "toggles")
          "tn" ''display-line-numbers-mode
          "ti" ''indent-guide-toggle
          "ts" ''flycheck-toggle
          "t C-c" ''toggle-camelcase-motion
          "th" '(list ':ignore t ':which-key "highlight")
          "thp" ''highlight-parentheses-mode
          ;Buffer
          "b" '(list ':ignore t ':which-key "buffers")
          "bb" ''w/helm-mini
          ;Zoom(Folding)
          "z" '(list ':ignore t ':which-key "zoom")
          "zc" ''toggle-selective-display
          ;Applications
          "a" '(list ':ignore t ':which-key "applications")
          "ai" '(list ':ignore t ':which-key "System info")
          "ait" ''emacs-init-time
          "as" '(list ':ignore t ':which-key "Shell")
          "asn" ''create-shell
          ;Git
          "g" '(list ':ignore t ':which-key "Git")
          "gb" ''magit-blame
          "gm" ''magit-dispatch-popup
          "gt" ''git-timemachine
          "gs" ''magit-status
          "gf" '(list ':ignore t ':which-key "file")
          "gfh" ''magit-log-buffer-file
          "gfb" ''magit-log-all-branches
          "gl" '(list ':ignore t ':which-key "links")
          "gll" ''git-link
          ;Jump
          "j" '(list ':ignore t ':which-key "jump")
          "jj" ''evil-avy-goto-char
          "jJ" ''evil-avy-goto-char-2
          "jl" ''evil-avy-goto-line
          "jw" ''evil-avy-goto-word-or-subword-1
          ;Error
          "e" '(list ':ignore t ':which-key "errors")
          "ef" ''force-run-flycheck
          "el" ''flycheck-list-errors
          "en" ''next-error
          "eN" ''previous-error
          "ep" ''previous-error
          "ec" ''flycheck-clear
          ;Layouts
          "l" '(list ':ignore t ':which-key "layouts")
          "lL" ''persp-load-state-from-file
          "ls" ''persp-save-state-to-file
          ;Regiters
          "r" '(list ':ignore t ':which-key "registers")
          "rj" ''jump-to-register
          "rw" ''window-configuration-to-register
          ;Windows
          "w" '(list ':ignore t ':which-key "windows")
          "w/" ''split-window-right
          "w-" ''split-window-below
          "wv" ''split-window-right
          "ws" ''split-window-below
          "wd" ''delete-window
          "wm" ''toggle-maximize-buffer
          "wj" ''evil-window-down
          "wk" ''evil-window-up
          "wh" ''evil-window-left
          "wl" ''evil-window-right
          ;Text
          "x" '(list ':ignore t ':which-key "text")
          "xU" ''upcase-region
          "xu" ''downcase-region
          "xs" ''toggle-camelcase-underscores
          "xd" '(list ':ignore t ':which-key "delete")
          "xdw" ''delete-trailing-whitespace
          ))

  (w/map-main-leader-key "SPC")
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
