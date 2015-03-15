;; Copy/Paste they way I am used to
(global-unset-key (kbd "C-c c"))
(global-set-key (kbd "C-c c") 'kill-ring-save)

;; Paste the way I am used to
(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v") 'yank)

;; Copy word under the cursor
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c") 'sd/copy-word-under-cursor)

;; Kill selection or line (in case nothing is selected)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'whole-line-or-region-kill-region)

;; List Buffers
(global-unset-key (kbd "C-S-b"))
(global-set-key (kbd "C-S-b") 'helm-buffers-list)

;; Window movements
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-<right>"))
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-<right>") 'windmove-right)

(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-<left>"))
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-<left>") 'windmove-left)

(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-<up>"))
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-<up>") 'windmove-up)

(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-<down>"))
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-<down>") 'windmove-down)

;; Zoom-in/Zoom-out
(global-unset-key (kbd "C-M-z"))
(global-set-key (kbd "C-M-z") 'zoom-window-zoom)

;; Kill current window
(global-unset-key (kbd "M-<end>"))
(global-set-key (kbd "M-<end>") 'delete-window)

;; Kill current buffer
(global-unset-key (kbd "C-<end>"))
(global-set-key (kbd "C-<end>") 'kill-this-buffer)

;; Got to other window after split
(global-unset-key (kbd "\C-x2"))
(global-set-key "\C-x2"
		(lambda ()
		  (interactive)
		  (split-window-vertically)
		  (other-window 1)))

(global-unset-key (kbd "\C-x3"))
(global-set-key "\C-x3" (lambda ()
			  (interactive)
			  (split-window-horizontally)
			  (other-window 1)))

;; Comment bindings
(global-unset-key (kbd "C-c C-c"))
(global-set-key (kbd "C-c C-c") 'sd/comment-line-or-region)
(global-unset-key (kbd "C-c C-u"))
(global-set-key (kbd "C-c C-u") 'sd/comment-line-or-region)

;; Text movement
(global-unset-key (kbd "C-S-<up>"))
(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-unset-key (kbd "C-S-<down>"))
(global-set-key (kbd "C-S-<down>") 'move-text-down)

;; Run shell mode
(global-unset-key (kbd "C-S-m"))
(global-set-key (kbd "C-S-m") 'shell)

;; Match parens
(global-unset-key (kbd "C-]"))
(global-set-key (kbd "C-]") 'sd/forward-or-backward-sexp)

;; Duplicate line
(global-unset-key (kbd "C-c C-v"))
(global-set-key (kbd "C-c C-v") 'sd/duplicate-line-or-region)

;; Select vertical region
(global-unset-key (kbd "C-M-<down>"))
(global-set-key (kbd "C-M-<down>") 'rectangle-mark-mode)

;; Enable deleting of selected text by subsequent input
(delete-selection-mode 1)

;; Map F1 to helm-imenu
(global-unset-key (kbd "<f1>"))
(global-set-key (kbd "<f1>") 'helm-imenu)

;; Map F2 to describe-key
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'hl-highlight-thingatpt-local)

;; Map F6 to describe-key
(global-unset-key (kbd "<f6>"))
(global-set-key (kbd "<f6>") 'describe-key)

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

;; Move mode line to top
(setq-default header-line-format mode-line-format)

;; Display shell buffer in the current window
(add-to-list 'display-buffer-alist
      '("^\\*shell\\*$" . (display-buffer-same-window)))

;; Expand region
(global-unset-key (kbd "C-="))
(global-set-key (kbd "C-=") 'er/expand-region)

;; Kick off dired with Ctrl-l
(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l") 
		(lambda ()
		  (interactive)
		  (dired ".") ))

;; Helm swoop
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'helm-swoop)

;; toggle h-cpp
(global-unset-key (kbd "C-<return>"))
(global-set-key (kbd "C-<return>") 'ff-find-other-file)

;; Backup file configuration
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; helm projectile
;; TODO move to its own mode file
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; TODO move to its own hook
(add-hook 'buffer-list-update-hook #'sd/update-header)

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
(menu-bar-mode -1)
(tool-bar-mode -1)
;; turn off blinking cursor
(blink-cursor-mode -1)
;; turn off the annoying beep
(setq visible-bell nil)
;; turn off line wrapping
(set-default 'truncate-lines t)
;; turn on line numbers globaly
(global-linum-mode t)

;; Color customization
;; Set cursor color to white
(set-cursor-color "cyan")
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
;; selection/search background/foreground
(set-face-attribute 'region nil :background "black" :foreground "yellow" )
(set-face-attribute 'isearch nil :background "black" :foreground "yellow" )
(set-face-attribute 'lazy-highlight nil :background "black" :foreground "cyan" )
;; mode line colors
(set-face-attribute 'mode-line nil :background "black" :foreground "yellow" )
(set-face-attribute 'mode-line-inactive nil :background "dimgray" :foreground "white" )
;; (set-face-attribute 'header-line nil :background "black" :foreground "yellow" )

;; hilight current line
(global-hl-line-mode +1)
;; (set-face-background hl-line-face "#005A64")
(set-face-background hl-line-face "#345858")

;; Kick off required modes
(helm-mode 1)
(desktop-change-dir "~/.emacs.d/temp")
(desktop-save-mode 1)
;; commented out causes performace issues when switching buffers
;;(hl-highlight-mode)
