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
             (add-to-list 'display-buffer-alist
                          `(,(rx bos "*Flycheck errors*" eos)
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side            . bottom)
                           (reusable-frames . 0)
                           (window-height   . 0.33)))
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
      (use-package evil-evilified-state
                   :load-path evil-evilified-state-path)

      (evilified-state-evilify-map flycheck-error-list-mode-map
                                   :mode flycheck-error-list-mode
                                   :bindings
                                   "RET" 'flycheck-error-list-goto-error
                                   "j" 'flycheck-error-list-next-error
                                   "k" 'flycheck-error-list-previous-error)
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
