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
               "ff" 'helm-find-files
               "fy" 's/show-buffer-file-name
               "ft" 'neotree-project-dir-toggle
               "pf" 'helm-projectile-find-file
               "saf" 'helm-do-ag
               "tn" 'display-line-numbers-mode
               "bb" 'helm-mini
               "zc" 'toggle-selective-display
               "ait" 'emacs-init-time
               "w" 'save-buffer)
             )
