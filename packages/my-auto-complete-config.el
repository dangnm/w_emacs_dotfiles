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
               (ac-config-default)
               (setq ac-delay 0.02)
               (setq ac-use-menu-map t)
               (setq ac-use-quick-help nil) 
               (setq ac-ignore-case nil)
               (setq ac-dwim  t)
               (setq ac-fuzzy-enable t)
               (setq ac-ignore-case t
                     ac-use-fuzzy t
                     ac-auto-start 0
                     ac-auto-show-menu 2.1
                     ac-expand-on-auto-complete nil)
               ;;Auto-completion complete key mapping
               (define-key ac-completing-map (kbd "C-l") 'ac-complete)
               ;;Trigger auto complete menu
               (with-eval-after-load "evil"
                                     (define-key evil-insert-state-map (kbd "C-f") 'auto-complete)
                                     )
             )
