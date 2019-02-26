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
                      exec-path-from-shell
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
                      evil-mc
                      ;========================================================
                      ; Package: Ruby tools
                      ;========================================================
                      haml-mode ;Haml for ruby development
                      ruby-test-mode ; ruby development
                      rbenv ; ruby env
                      inf-ruby ; ruby irb
                      ;========================================================
                      ; Package: JS tools
                      ;========================================================
                      add-node-modules-path
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
(setq w-dotfiles-pakages-folder-path (format "%s/packages" w-dotfiles-folder-path))
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

;; (setq use-package-verbose 't) ;Show use-package stat

;========================================================
; MAIN CONFIGS
;========================================================
(if (or (not (boundp 'w/is-running-an-async-job)) (not w/is-running-an-async-job))
    (progn
      (require 'org)
      (org-babel-load-file
       (expand-file-name "emacs.org"
                         w-dotfiles-folder-path))
      ))

