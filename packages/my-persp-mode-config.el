;; perspectives for emacs
(use-package persp-mode
             :ensure t
             :diminish persp-mode
             :init
             (setq wg-morph-on nil ;; switch off animation
                   persp-add-buffer-on-after-change-major-mode t
                   persp-auto-resume-time -1
                   persp-autokill-buffer-on-remove 'kill-weak
                   persp-save-dir (expand-file-name "~/.emacs.d/.cache/layouts/"))
             (add-hook 'after-init-hook (lambda () (persp-mode 1)))
             )
