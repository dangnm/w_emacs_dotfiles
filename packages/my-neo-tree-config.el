;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neo tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package neotree
             :commands (neotree-mode)
             :config
             (require 'neotree)
             (message "Loading neotree configs...")
             (with-eval-after-load 'neotree
                                   (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
                                   (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
                                   (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
                                   (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
                                   (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
                                   (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
                                   (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
                                   (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
                                   (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
                                   (evil-define-key 'normal neotree-mode-map (kbd "m") 'neotree-rename-node)
                                   (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
                                   (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node))
             )

(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
  or the current buffer directory."
  (interactive)
  (let ((project-dir
          (ignore-errors
            ;;; Pick one: projectile or find-file-in-project
            ; (projectile-project-root)
            (ffip-project-root)
            ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
      (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
          (neotree-dir project-dir))
        (if file-name
          (neotree-find file-name))))))
