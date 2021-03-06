(defun init-git-full-commit-config-variables ()
  (if (not(fboundp 'save-sexp-save-setq))
    (load "save-sexp")
    )
  (if (and (file-exists-p "~/.emacs.d/w-config-variables.el") 
           (not(boundp 'projectile-git-last-commit-full-ids)))
    (load-file "~/.emacs.d/w-config-variables.el")
    )
  (if (not(boundp 'projectile-git-last-commit-full-ids))
    (setq projectile-git-last-commit-full-ids (make-hash-table :test 'equal)) 
    )
  )
(defun get-last-git-commit-id ()
  (replace-regexp-in-string  "\n\+$" "" (shell-command-to-string "git show -s --format=%H 2>/dev/null"))
  )
(defun save-git-full-commit-id-to-file ()
  (init-git-full-commit-config-variables)
  (let ((last-git-commit-id (get-last-git-commit-id)))
    (if (not(string= "" last-git-commit-id))
      (progn
        (setf (gethash (ffip-project-root) projectile-git-last-commit-full-ids) last-git-commit-id) 
        (save-sexp-save-setq "~/.emacs.d/w-config-variables.el" 'projectile-git-last-commit-full-ids)
        )
      )
    )
  )
(defun check-git-full-commit-id-changed ()
  (init-git-full-commit-config-variables)
  (let ((last-git-commit-id (get-last-git-commit-id)))
    (cond (
           (string= (gethash (ffip-project-root) projectile-git-last-commit-full-ids) last-git-commit-id)
           nil
           )
          (
           (string= (gethash (ffip-project-root) projectile-git-last-commit-full-ids) nil)
           nil
           )
          (t (not nil))
          )
    )
  )
(defun clear-projectile-cache-first-time ()
  (if (not(get 'clear-projectile-first-time-flag 'state))
    (progn
      (projectile-invalidate-cache nil)
      (put 'clear-projectile-first-time-flag 'state t)))
  )
(defun clear-projectile-cache-when-git-commit-id-changed ()
  (if (check-git-full-commit-id-changed)
    (progn
      (save-git-full-commit-id-to-file)
      (projectile-invalidate-cache nil)
      (put 'clear-projectile-first-time-flag 'state t)))
  )

(add-hook 'helm-after-initialize-hook
          (lambda()
            (setq my-saved-project-directory (projectile-project-root))
            (async-start
              `(lambda ()
                ,(async-inject-variables "\\`my-saved-project-directory\\'")
                (setq w/is-running-an-async-job 't) 
                (load-file "~/.emacs.d/init.el")
                (if my-saved-project-directory
                    (progn
                      (require 'helm-projectile)
                      (dired my-saved-project-directory)
                      (load "my-helm-clear-projectile-cache-config")
                      (projectile-mode +1)
                      (if (string= "" (get-last-git-commit-id))
                          (clear-projectile-cache-first-time)
                        (clear-projectile-cache-when-git-commit-id-changed)
                        )
                      (projectile-mode -1)
                      )
                  )
                (format "Async: %s" my-saved-launch-directory)
                )
              )
              'ignore
            ))
