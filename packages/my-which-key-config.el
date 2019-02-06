;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
             :commands (which-key-configs-load)
             :config
             (defun which-key-configs-load ()
               t)
             (run-with-timer 3 nil
                             (lambda ()
                               (message "Loading which-key configs...")))
             (require 'which-key)
             (require 'tramp)
             (which-key-mode)
             (which-key-setup-side-window-bottom)
             )

(add-hook 'emacs-startup-hook 'which-key-configs-load)
