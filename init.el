(require 'package)

;========================================================
; SETUP PACKAGES
;========================================================
; List the packages you want
(setq package-list '(evil
                      evil-leader 
                      bind-map ;keymap available across different “leader keys”
                      which-key 
                      helm-ag
                      helm-projectile
                      autothemer
                      spaceline ;Bottom statusline
                      neotree ;Tree explorer
                      find-file-in-project ;neotree project path support
                      magit ;git tools
                      magit-popup ;git tools
                      git-link ;copy github link
                      git-timemachine ;git tools
                      indent-guide ;indent guide
                      flycheck ;syntax error checking
                      auto-complete
                      evil-nerd-commenter ;comment code
                      evil-surround ;surroundings: parentheses, brackets, quotes, XML tags, and more
                      avy ;jumping to visible text using a char-based decision tree
                      async ;asynchronous processing in Emacs
                      osx-clipboard
                      use-package
                      ))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; Activate all the packages (in particular autoloads)
(package-initialize)

;Init file paths
(defun s/dotfiles-folder-path ()
  (let ((path1 (replace-regexp-in-string  "\n\+$" "" (shell-command-to-string "readlink ~/.emacs.d 2>/dev/null")))
        (path2 (replace-regexp-in-string  "\n\+$" "" (shell-command-to-string "dirname $(readlink ~/.emacs.d/init.el) 2>/dev/null"))))
    (if (string= "" path1)
      path2
      path1
      )
    )
  )
(setq w-dotfiles-folder-path (s/dotfiles-folder-path))
(setq evil-evilified-state-path (format "%s/packages/evil-evilified-state.el" w-dotfiles-folder-path))

;; This is only needed once, near the top of the file
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path (format "%s/packages" w-dotfiles-folder-path))
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
; SETUP FLYCHECK
;========================================================
(load "my-flycheck-config")

;========================================================
; SETUP THEME
;========================================================
(add-to-list 'load-path (format "%s/themes" w-dotfiles-folder-path))
(add-to-list 'custom-theme-load-path (format "%s/themes" w-dotfiles-folder-path))
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
; SETUP INDENT GUIDE
;========================================================
(load "my-indent-guide-config")

;========================================================
; SETUP AUTO COMPLETE
;========================================================
(load "my-auto-complete-config")

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
;; Indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default js-indent-level 2)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
;;Disable lock file
;;Emacs automatically creates a temporary symlink in the same directory as the file being edited
(setq create-lockfiles nil)
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
	'(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
	'(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
;; support downcase upcase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; hide toolbar in emacs GUI
(tool-bar-mode -1) 

;========================================================
; HELPERS
;========================================================
(defun s/show-buffer-file-path ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun s/show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (file-name-nondirectory(buffer-file-name))))
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

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun toggle-camelcase-motion ()
  (interactive)
  (if (get 'camelcase-motion-toggle-flag 'state)
    (progn
      (message "Disabled camelcase motion")
      (subword-mode'-1)
      (put 'camelcase-motion-toggle-flag'state nil))
    (progn
      (message "Enabled camelcase motion")
      (subword-mode)
      (put 'camelcase-motion-toggle-flag 'state t))
    )
  )
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
