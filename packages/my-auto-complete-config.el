;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-complete
             :commands auto-complete-mode
             :init 
             (progn
               (auto-complete-mode t))
             :bind (("C-n" . ac-next)
                    ("C-p" . ac-previous))
             :config
               (use-package auto-complete-config)
               (ac-set-trigger-key "TAB")
               (ac-config-default)
               (setq ac-delay 0.02)
               (setq ac-use-menu-map t)
               (setq ac-menu-height 50)
               (setq ac-use-quick-help nil) 
               (setq ac-ignore-case nil)
               (setq ac-dwim  t)
               (setq ac-fuzzy-enable t)
               ;;Auto-completion complete key mapping
               (with-eval-after-load "evil"
                                     (define-key evil-insert-state-map (kbd "C-f") 'auto-complete)
                                     )
               (add-hook 'company-mode-hook
                         (lambda()
                           (define-key company-active-map (kbd "C-f") 'company-complete-selection)))
             )
