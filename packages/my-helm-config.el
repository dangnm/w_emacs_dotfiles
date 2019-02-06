;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-projectile
             :commands (helm-projectile-configs-load helm-mode helm-projectile-find-file projectile-switch-project)
             :config
             (defun helm-projectile-configs-load ()
               t)
             (run-with-timer 3 nil
                             (lambda ()
                               (message "Loading helm configs...")))
             (require 'helm-projectile)
             (helm-projectile-on)
             (projectile-mode +1)
             (setq projectile-enable-caching t)
             (add-to-list 'exec-path "/usr/local/bin/") ;;Path for running ag ...
             (global-set-key (kbd "<escape>")      'keyboard-escape-quit)
             (load "my-helm-clear-projectile-cache-config")
             ;;Using ESC (ctrl+G) to quit helm command
             (with-eval-after-load "helm-command"
                                   (define-key helm-M-x-map (kbd "ESC") 'helm-keyboard-quit)
                                   )

             (add-hook 'helm-after-initialize-hook
                       (lambda()
                         (define-key helm-map (kbd "C-j") 'helm-next-line)
                         (define-key helm-map (kbd "C-k") 'helm-previous-line)))
             )

(defun w/projectile-switch-project ()
  (interactive)
  (helm-projectile-configs-load)
  (helm-projectile-switch-project)
  )

(defun w/projectile-dired ()
  (interactive)
  (helm-projectile-configs-load)
  (projectile-dired)
  )

(defun w/helm-M-x ()
  (interactive)
  (helm-projectile-configs-load)
  (call-interactively 'helm-M-x)
  )

(defun w/helm-mini ()
  (interactive)
  (helm-projectile-configs-load)
  (call-interactively 'helm-mini)
  )
