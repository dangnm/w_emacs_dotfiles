(defun loading-org-mode-dependencies ()
  (interactive)
  (load "my-org-mode-dependencies-config")
  )
(add-hook 'org-mode-hook 'loading-org-mode-dependencies)
