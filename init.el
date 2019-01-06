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
                      neotree ;Tree explorer
                      find-file-in-project ;neotree project path support
                      magit ;git tools
                      osx-clipboard
                      use-package
                      ))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; Activate all the packages (in particular autoloads)
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/w_emacs_dotfiles/packages")
  (require 'use-package))

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
(bind-keys :prefix-map my-leader-map :prefix "SPC")
(load "my-evil-config")

;========================================================
; SETUP WHICH KEY
;========================================================
(load "my-which-key-config")

;========================================================
; SETUP HELM
;========================================================
(load "my-helm-config")

;========================================================
; SETUP MAGIT
;========================================================
(load "my-magit-config")

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
; SETUP NEO TREE
;========================================================
(load "my-neo-tree-config")

;========================================================
; SETUP CLIPBOARD
;========================================================
(osx-clipboard-mode +1)

;========================================================
; SETUP EDITOR
;========================================================
(menu-bar-mode -1) 
(global-display-line-numbers-mode)
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

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
