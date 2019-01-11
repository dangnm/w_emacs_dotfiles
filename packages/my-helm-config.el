;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-projectile
             :commands (helm-mode helm-projectile-find-file)
             :config
             (run-with-timer 3 nil
                             (lambda ()
                               (message "Loading helm configs...")))
             (require 'helm-projectile)
             (helm-projectile-on)
             (projectile-mode +1)
             (setq projectile-enable-caching t)
             (global-set-key (kbd "<escape>")      'keyboard-escape-quit)
             (defun clear-projectile-cache-first-time ()
               (if (not(get 'clear-projectile-first-time-flag 'state))
                 (progn
                   (projectile-invalidate-cache nil)
                   (put 'clear-projectile-first-time-flag 'state t)))
               )
             (add-hook 'helm-after-initialize-hook
                       (lambda()
                         (clear-projectile-cache-first-time)
                         (define-key helm-map (kbd "C-j") 'helm-next-line)
                         (define-key helm-map (kbd "C-k") 'helm-previous-line)))
             )
