;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit - GIT tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
             :commands (magit-blame-mode magit-blame)
             :config
             (evil-make-overriding-map magit-blame-mode-map 'normal)
             (add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)
             (evil-define-key 'normal magit-blame-mode-map (kbd "q") 'magit-blame-quit)

             (require 'evil-magit)
             ;Full screen git status
             (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
             (define-key magit-mode-map "\C-j" 'magit-section-forward)
             (define-key magit-mode-map "\C-k" 'magit-section-backward)
             (define-key magit-mode-map "\M-j" 'magit-section-forward-sibling)
             (define-key magit-mode-map "\M-k" 'magit-section-backward-sibling)
             )

(use-package git-timemachine
             :commands (git-timemachine)
             :config

             (defun git-timemachine-blame ()
              "Call magit-blame on current revision."
              (interactive)
              (if (fboundp 'magit-blame)
               (let ((magit-buffer-revision (car git-timemachine-revision)))
                (magit-blame))
               (message "You need to install magit for blame capabilities")))

             (defun git-timemachine-find-revision-by-id (revision-id)
               (require 'cl)
               (message revision-id)
               (cl-loop for v in (git-timemachine--revisions)
                        until (cl-search revision-id (nth 0 v))
                        finally return v
                        )
               )
             (defun git-timemachine-go-to-revision-id (revision-id)
               (interactive "sEnter revision id: ")
               (git-timemachine-show-revision (git-timemachine-find-revision-by-id revision-id))
               )
             (evil-define-key 'normal git-timemachine-mode-map (kbd "G") 'git-timemachine-go-to-revision-id)
             )

(use-package git-gutter
             :commands (global-git-gutter-mode git-gutter-mode)
             :config
             (progn
               (set-face-background 'git-gutter:deleted "#990A1B")
               (set-face-foreground 'git-gutter:modified "#00736F")
               (set-face-foreground 'git-gutter:added "#546E00"))
             )
;Init git gutter when saving
(add-hook 'after-save-hook 'git-gutter-mode)

(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))
