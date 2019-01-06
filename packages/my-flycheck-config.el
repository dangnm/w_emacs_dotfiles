;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FlyCheck - Syntax error checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
             :ensure t
             :init (global-flycheck-mode'-1)
             :config
             ;;Issue: flycheck syntax checking makes editing files really slow 
             (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
             (setq flycheck-idle-change-delay 60) ;; Set delay based on what suits you the best
             )

(defun flycheck-toggle ()
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'flycheck-toggle-flag 'state)
    (progn
      (run-with-timer 1 nil
                      (lambda ()
                        (message "Disabled syntax checking")))
      (global-flycheck-mode'-1)
      (put 'flycheck-toggle-flag 'state nil))
    (progn
      (run-with-timer 1 nil
                      (lambda ()
                        (message "Enabled syntax checking")))
      (global-flycheck-mode)
      (put 'flycheck-toggle-flag 'state t))
    ))

(defun force-my-flycheck-enable-first-time ()
  (if (and (not(get 'flycheck-toggle-flag 'state)) (not(get 'flycheck-toggle-first-time-flag 'state)))
    (progn
      (global-flycheck-mode)
      (put 'flycheck-toggle-flag 'state t)
      (put 'flycheck-toggle-first-time-flag 'state t)))
  )

(defun force-run-flycheck ()
  (interactive)
  (progn
    (force-my-flycheck-enable-first-time)
    (flycheck-buffer))
  )

;Only init flycheck when saving
(add-hook 'after-save-hook 'force-my-flycheck-enable-first-time)
