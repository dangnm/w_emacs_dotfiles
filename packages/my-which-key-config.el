;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
             :config
             (run-with-timer 3 nil
                             (lambda ()
                               (message "Loading which-key configs...")))
             (require 'which-key)
             (which-key-mode)
             (which-key-setup-side-window-bottom)
             (which-key-add-key-based-replacements
               "SPC f" "files"
               "SPC s" "search/symbol"
               "SPC sa" "ag"
               "SPC p" "projects"
               "SPC t" "toggles"
               "SPC b" "buffers"
               "SPC z" "zoom"
               "SPC a" "applications"
               "SPC ai" "System info"
               "SPC g" "Git"
               "SPC e" "Errors"
               )
             )

