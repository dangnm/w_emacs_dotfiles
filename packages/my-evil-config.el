;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
             :config
             (require 'evil)
             (evil-mode t)
             )

(use-package evil-leader 
             :config
             (require 'evil-leader)
             (global-evil-leader-mode)
             (evil-leader/set-leader "SPC")
             (evil-leader/set-key
               ;File
               "ff" 'helm-find-files
               "fy" 's/show-buffer-file-name
               "ft" 'neotree-project-dir-toggle
               ;Projects
               "pf" 'helm-projectile-find-file
               "pg" 'find-my-tag
               "pG" 'projectile-regenerate-tags
               "pD" 'projectile-dired
               "pr" 'projectile-recentf
               ;Search
               "saf" 'helm-do-ag
               ;Toggle
               "tn" 'display-line-numbers-mode
               "ti" 'indent-guide-toggle
               "ts" 'flycheck-toggle
               ;Buffer
               "bb" 'helm-mini
               ;Zoom(Folding)
               "zc" 'toggle-selective-display
               ;Applications
               "ait" 'emacs-init-time
               ;Git
               "gb" 'magit-blame
               ;Error
               "ef" 'force-run-flycheck
               "w" 'save-buffer)
             )

(defun find-my-tag ()
  (interactive)
  (if (not(fboundp 'my-find-tag-and-load-config))
    (progn
      (load "my-tags-config")
      (my-find-tag-and-load-config))
    (my-find-tag-and-load-config))
  )
