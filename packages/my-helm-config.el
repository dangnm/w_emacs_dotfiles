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
             (global-set-key (kbd "<escape>")      'keyboard-escape-quit)
             (add-hook 'helm-after-initialize-hook
                       (lambda()
                         (define-key helm-map (kbd "C-j") 'helm-next-line)
                         (define-key helm-map (kbd "C-k") 'helm-previous-line)))
             )
