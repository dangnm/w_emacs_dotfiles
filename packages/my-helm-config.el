;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-projectile
             :config
             (require 'helm-projectile)
             (helm-projectile-on)
             (projectile-mode +1)
             (global-set-key (kbd "<escape>")      'keyboard-escape-quit)
             (add-hook 'helm-after-initialize-hook
                       (lambda()
                         (define-key helm-map (kbd "C-j") 'helm-next-line)
                         (define-key helm-map (kbd "C-k") 'helm-previous-line)))
             )
