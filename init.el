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
  "ft" 'neotree-project-dir-toggle
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
; SETUP NEO TREE
;========================================================
;; Difine key bind for neotree
(require 'neotree)
(with-eval-after-load 'neotree
                      (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
                      (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
                      (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
                      (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
                      (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
                      (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
                      (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
                      (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
                      (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
                      (evil-define-key 'normal neotree-mode-map (kbd "m") 'neotree-rename-node)
                      (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
                      (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node))


(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
  or the current buffer directory."
  (interactive)
  (let ((project-dir
          (ignore-errors
            ;;; Pick one: projectile or find-file-in-project
            ; (projectile-project-root)
            (ffip-project-root)
            ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
      (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
          (neotree-dir project-dir))
        (if file-name
          (neotree-find file-name))))))

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
