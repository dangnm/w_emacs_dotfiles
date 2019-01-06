;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit - GIT tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
             :commands (magit-blame-mode)
             :config
             (evil-make-overriding-map magit-blame-mode-map 'normal)
             (add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)
             (evil-define-key 'normal magit-blame-mode-map (kbd "q") 'magit-blame-quit)
             )
