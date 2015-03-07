;; Copy/Paste they way I am used to
(global-unset-key (kbd "C-c c"))
(global-set-key (kbd "C-c c") 'kill-ring-save)

;; Paste the way I am used to
(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v") 'yank)

;; Kill selection or line (in case nothing is selected)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'whole-line-or-region-kill-region)

;; List Buffers
(global-unset-key (kbd "C-S-b"))
(global-set-key (kbd "C-S-b") 'helm-buffers-list)

;; Window movements
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") 'windmove-right)
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h") 'windmove-left)
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'windmove-up)
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'windmove-down)

;; Zoom-in/Zoom-out
(global-unset-key (kbd "C-M-z"))
(global-set-key (kbd "C-M-z") 'zoom-window-zoom)

;; Kill current window
(global-unset-key (kbd "M-<end>"))
(global-set-key (kbd "M-<end>") 'delete-window)

;; Kill current buffer
(global-unset-key (kbd "C-<end>"))
(global-set-key (kbd "C-<end>") 'kill-this-buffer)

;; Comment bindings
(global-unset-key (kbd "C-c C-c"))
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-unset-key (kbd "C-c C-u"))
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Run shell mode
(global-unset-key (kbd "C-S-m"))
(global-set-key (kbd "C-S-m") 'shell)

;; Match parens
(global-unset-key (kbd "C-]"))
(global-set-key (kbd "C-]") 'goto-match-paren)

;; ace-jump-mode
(global-unset-key (kbd "M-SPC"))
(define-key global-map (kbd "M-SPC") 'ace-jump-word-mode)
(global-unset-key (kbd "C-M-r"))
(define-key global-map (kbd "C-M-r") 'redraw-display) ; sometimes needed when doing ace jump
;(setq ace-jump-mode-scope 'window)       ; jump within one window only
;(setq ace-jump-mode-gray-background nil) ; disable gray background
(custom-set-faces
 '(ace-jump-face-foreground
   ((t (:inherit ace-jump-face-foreground :height 1.1 :foreground "yellow" :background "black" )))))

;; helm projectile
;; TODO move to its own mode file
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;; Variable customizations

;; Show corresponding paren
(show-paren-mode 1)
;; Don't wrap lines
(setq-default truncate-lines t)
;; keyboard scroll one line at a time
(setq scroll-step 1)
;; suppress splash screen
(setq inhibit-startup-message t)
;; disable scrollbar, menu bar and tool bar
(scroll-bar-mode -1)
(menu-bar-mode 1)
(tool-bar-mode -1)
;; turn off blinking cursor
(blink-cursor-mode -1)
;; turn off the annoying beep
(setq visible-bell nil)
;; turn off line wrapping
(set-default 'truncate-lines t)

;;; Color customization
;; set background colors
(set-background-color "#2F4F4F")
;; color of border of buffer separator
(set-face-background 'fringe "#2F4F4F")
;; color of comments
(set-face-foreground 'font-lock-comment-face "#FA8278")
;; color of keyword
(set-face-foreground 'font-lock-keyword-face "#FF9664")
;; color of background
(set-face-foreground 'default "#FFF8DC")
;; color of srings
(set-face-foreground 'font-lock-string-face "#00ECC8")

;; Kick off required modes
(helm-mode 1)
(desktop-change-dir "~/emacs")
(desktop-save-mode 1)
