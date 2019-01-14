;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-projectile
             :commands (helm-mode helm-projectile-find-file projectile-switch-project)
             :config
             (run-with-timer 3 nil
                             (lambda ()
                               (message "Loading helm configs...")))
             (require 'helm-projectile)
             (helm-projectile-on)
             (projectile-mode +1)
             (setq projectile-enable-caching t)
             (global-set-key (kbd "<escape>")      'keyboard-escape-quit)
             (load "my-helm-clear-projectile-cache-config")
             (add-hook 'helm-after-initialize-hook
                       (lambda()
                         (define-key helm-map (kbd "C-j") 'helm-next-line)
                         (define-key helm-map (kbd "C-k") 'helm-previous-line)))
             )
