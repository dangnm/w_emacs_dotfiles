;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent guide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package indent-guide
             :commands (indent-guide-global-mode)
             :config
             (run-with-timer 3 nil
                             (lambda ()
                               (message "Loading indent-guide configs...")))
             (require 'indent-guide)
             )

(defun indent-guide-toggle ()
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'indent-guide-toggle-flag 'state)
    (progn
      (run-with-timer 1 nil
                      (lambda ()
                        (message "Disabled indent guide")))
      (indent-guide-global-mode -1)
      (put 'indent-guide-toggle-flag 'state nil))
    (progn
      (run-with-timer 1 nil
                      (lambda ()
                        (message "Enabled indent guide")))
      (indent-guide-global-mode)
      (put 'indent-guide-toggle-flag 'state t))
    ))

