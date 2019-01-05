(require 'package)

;========================================================
; SETUP PACKAGES
;========================================================
; List the packages you want
(setq package-list '(evil
                      evil-leader 
                      which-key 
                      helm-ag
                      helm-projectile
                      autothemer
                      spaceline ;Bottom statusline
                      osx-clipboard
                      ))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; Activate all the packages (in particular autoloads)
(package-initialize)

; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;========================================================
; SETUP EVIL
;========================================================
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)

;========================================================
; SETUP EVIL LEADER
;========================================================
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "ff" 'helm-find-files
  "fy" 's/show-buffer-file-name
  "pf" 'helm-projectile-find-file
  "saf" 'helm-do-ag
  "tn" 'display-line-numbers-mode
  "bb" 'helm-mini
  "zc" 'toggle-selective-display
  "w" 'save-buffer)

;========================================================
; SETUP WHICH KEY
;========================================================
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
  )

;========================================================
; SETUP HELM
;========================================================
(require 'helm-projectile)
(helm-projectile-on)
(projectile-mode +1)
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
(add-hook 'helm-after-initialize-hook
          (lambda()
            (define-key helm-map (kbd "C-j") 'helm-next-line)
            (define-key helm-map (kbd "C-k") 'helm-previous-line)))

;========================================================
; SETUP THEME
;========================================================
(add-to-list 'load-path "~/w_emacs_dotfiles/themes")
(add-to-list 'custom-theme-load-path "~/w_emacs_dotfiles/themes")
;(load-theme 'monokai t)
(load-theme 'gruvbox-dark-medium t)

;========================================================
; SETUP SPACELINE
;========================================================
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;========================================================
; SETUP CLIPBOARD
;========================================================
(osx-clipboard-mode +1)

;========================================================
; SETUP EDITOR
;========================================================
(menu-bar-mode -1) 
(global-display-line-numbers-mode)


;========================================================
; HELPERS
;========================================================
(defun s/show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

; Folding
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
    (or column
        (unless selective-display
          (1+ (current-column))))))

;========================================================
; OTHER CONFIGS
;========================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-leader 
                                      evil 
                                      which-key 
                                      helm-ag 
                                      helm-projectile
                                      autothemer
                                      ))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
