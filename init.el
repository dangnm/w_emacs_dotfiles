(require 'package)

;========================================================
; SETUP PACKAGES
;========================================================
(setq my-saved-launch-directory default-directory)
;; set the dir where all elpa packages go
(setq relative-config-dir "~/.emacs.d/")
(setq package-user-dir (concat relative-config-dir "welpa"))

; List the packages you want
(setq package-list '(
                      ;========================================================
                      ; Package: GENERAL
                      ;========================================================
                      evil ;Vi mode
                      general ;evil leader map
                      bind-map ;keymap available across different “leader keys”
                      which-key ;key leader guide 
                      async ;asynchronous processing in Emacs
                      use-package ;package manager
                      ;========================================================
                      ; Package: Search
                      ;========================================================
                      helm-ag
                      helm-projectile
                      neotree ;Tree explorer
                      find-file-in-project ;neotree project path support
                      ;========================================================
                      ; Package: UI
                      ;========================================================
                      autothemer ;theme
                      spaceline ;Bottom statusline
                      indent-guide ;indent guide
                      highlight-parentheses; UI: highlight
                      ;========================================================
                      ; Package: GIT
                      ;========================================================
                      magit ;git tools
                      magit-popup ;git tools
                      evil-magit ;git tools with vi mode
                      git-gutter ;indicating inserted, modified or deleted lines
                      git-link ;copy github link
                      git-timemachine ;git tools
                      ;========================================================
                      ; Package: Edit tools
                      ;========================================================
                      flycheck ;syntax error checking
                      auto-complete
                      evil-nerd-commenter ;comment code
                      evil-surround ;surroundings: parentheses, brackets, quotes, XML tags, and more
                      avy ;jumping to visible text using a char-based decision tree
                      ;========================================================
                      ; Package: Ruby tools
                      ;========================================================
                      haml-mode ;Haml for ruby development
                      ruby-test-mode ; ruby development
                      rbenv ; ruby env
                      inf-ruby ; ruby irb
                      ;========================================================
                      ; Package: Workspace tools
                      ;========================================================
                      persp-mode ;perspectives for emacs, save/recover sessions
                      osx-clipboard
                      org
                      ))

; Add Melpa as the default Emacs Package repository
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

; Activate all the packages (in particular autoloads)
(package-initialize)

;Init file paths
(defun w/dotfiles-folder-path ()
  (let ((path1 (replace-regexp-in-string  "\n\+$" "" (shell-command-to-string "dirname $(readlink ~/.emacs.d/init.el) 2>/dev/null"))))
    (if (not(string= "" path1))
      path1
      (replace-regexp-in-string  "\n\+$" "" (shell-command-to-string "readlink ~/.emacs.d 2>/dev/null"))
      )
    )
  )
(setq w-dotfiles-folder-path (w/dotfiles-folder-path))
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
; SETUP PERSP MODE - save/recover sessions
;========================================================
(load "my-persp-mode-config")

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
; SETUP ORG MODE
;========================================================
(load "my-org-mode-config")

;========================================================
; SETUP RUBY
;========================================================
(load "my-ruby-config")

;========================================================
; SETUP CLIPBOARD
;========================================================
(osx-clipboard-mode +1)

;========================================================
; SETUP SHELL
;========================================================
(add-hook 'comint-mode-hook
          (lambda ()
            (toggle-truncate-lines -1) ;;Enable auto line wrapping
            (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
            (evil-define-key 'normal comint-mode-map (kbd "C-d") 'evil-scroll-down)
            ))

;========================================================
; HELPERS
;========================================================
(defun create-shell ()
  "creates a shell with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun create-shell-with-name (name)
  (shell (concat "*" name "*")))

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
; SETUP EDITOR
;========================================================
;;Disable auto line wrapping
(set-default 'truncate-lines t)
;; Theme colors for shell
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)
(menu-bar-mode -1) 
;; hide toolbar in emacs GUI
(tool-bar-mode -1) 
(global-display-line-numbers-mode)
(defun w/setup-font-and-window ()
    (progn
      ;;Font size 16pt
      (set-face-attribute 'default nil :font "Source Code Pro-16" )
      (menu-bar-mode -1) 
      ;;Disable scrollbar in UI mode
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
    )
(if (display-graphic-p)
  (w/setup-font-and-window)
  )
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (if (display-graphic-p frame)
    (w/setup-font-and-window)
    )
  (set-frame-parameter frame 'menu-bar-lines 
                             (if (display-graphic-p frame)
                                  1 0)))

;; Hide meubar when opening emacs with emacs client
(add-hook 'after-make-frame-functions 'contextual-menubar)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(package-selected-packages
   (quote
    (evil-leader evil which-key helm-ag helm-projectile autothemer))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
;; support downcase upcase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;========================================================
; OTHER CONFIGS
;========================================================

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
