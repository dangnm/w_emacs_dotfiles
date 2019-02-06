(use-package rbenv
  :ensure t
  :defer t
  :init (setq rbenv-show-active-ruby-in-modeline nil)
  :config (progn
            (global-rbenv-mode)
            (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding)
            ))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  :config
  (eval-after-load 'inf-ruby
    '(rbenv-use-corresponding))
  (eval-after-load 'inf-ruby
    '(define-key inf-ruby-mode-map
       (kbd "C-k") 'comint-previous-input))
  (eval-after-load 'inf-ruby
    '(define-key inf-ruby-mode-map
       (kbd "C-j") 'comint-next-input))
  (setq inf-ruby-console-environment "development")
  )
