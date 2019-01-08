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
               ;Jump
               "jj" 'evil-avy-goto-char
               "jJ" 'evil-avy-goto-char-2
               "jl" 'evil-avy-goto-line
               "jw" 'evil-avy-goto-word-or-subword-1
               ;Error
               "ef" 'force-run-flycheck
               "w" 'save-buffer)
             )

(use-package evil
             :config
             (require 'evil)
             (evil-mode t)
             )

;; evil-nerd-commenter
(use-package evil-nerd-commenter
             :init
             (with-eval-after-load "evil"
                                   (define-key evil-visual-state-map "gc" 'evilnc-comment-or-uncomment-lines)
                                   ))

(defun sync-config ()
  (interactive)
  (load-file user-init-file)
  )

(defun find-my-tag ()
  (if (not(fboundp 'my-find-tag-and-load-config))
    (progn
      (load "my-tags-config")
      (my-find-tag-and-load-config))
    (my-find-tag-and-load-config))
  )
