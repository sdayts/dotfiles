(message "Loading file after-init hook")

;; Move mode line to top
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

;; helm options
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-mode-reverse-history           nil ; place helm command history on top
      )

;; stop emacs from opening in "other" window
(setq same-window-regexps nil);;'(("^*")))
(add-to-list 'same-window-buffer-names "*Help*")
(add-to-list 'same-window-buffer-names "*shell*")
(add-to-list 'same-window-buffer-names "*Apropos*")
(add-to-list 'same-window-buffer-names "*Summary*")
(add-to-list 'same-window-buffer-names "*grep*")

;; toggle h-cpp
(global-unset-key (kbd "C-<return>"))
(global-set-key (kbd "C-<return>") 'ff-find-other-file)

;; Kick off dired with Ctrl-l
(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l")
		(lambda ()
		  (interactive)
		  (dired ".") ))

;; avy customizations. weirdly looks like avy does not have standard hooks
(require 'avy)
(global-unset-key (kbd "M-SPC"))
(define-key global-map (kbd "M-SPC") 'avy-goto-word-1)
(setq avy-background t)
(set-face-attribute 'avy-lead-face nil :background "black" :foreground "yellow")
(set-face-attribute 'avy-lead-face-0 nil :background "black" :foreground "yellow")

;; Run shell mode
(global-unset-key (kbd "C-S-m"))
(global-set-key (kbd "C-S-m") 'shell)
;; Display shell buffer in the current window
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

;; Match parens
(global-unset-key (kbd "C-]"))
(global-set-key (kbd "C-]") 'sd/forward-or-backward-sexp)

;; Duplicate line
(global-unset-key (kbd "C-c C-v"))
(global-set-key (kbd "C-c C-v") 'sd/duplicate-line-or-region)

;; Copy word under the cursor
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c") 'sd/copy-word-under-cursor)

;; Kill selection or line (in case nothing is selected)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'whole-line-or-region-kill-region)

;; Copy/Paste they way I am used to
(global-unset-key (kbd "C-c c"))
(global-set-key (kbd "C-c c") 'kill-ring-save)

;; Paste the way I am used to
(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v") 'yank)

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

;; List Buffers
(global-unset-key (kbd "C-S-b"))
(global-set-key (kbd "C-S-b") 'helm-buffers-list)

;; Map F1 to helm-imenu
(global-unset-key (kbd "<f1>"))
(global-set-key (kbd "<f1>") 'helm-imenu)

;; Map F2 to highlight-symbol
(hl-highlight-mode t)
(global-unset-key [f2])
(global-set-key [f2] 'hl-highlight-thingatpt-local)

;; Map C-x C-r to helm-recentf
(global-unset-key (kbd "C-x C-r"))
(global-set-key (kbd "C-x C-r") 'helm-recentf)

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

;; Select vertical region
(global-unset-key (kbd "C-M-<down>"))
(global-set-key (kbd "C-M-<down>") 'rectangle-mark-mode)

;; Enable deleting of selected text by subsequent input
(delete-selection-mode 1)

;; Map F6 to describe-key
(global-unset-key (kbd "<f6>"))
(global-set-key (kbd "<f6>") 'describe-key)

;; disable scrollbar, menu bar and tool bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Helm swoop
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'helm-occur)

;; hilight current line
(global-hl-line-mode +1)

;; set all bold faces to normal
(mapc
 (lambda (face)
   (when (eq (face-attribute face :weight) 'bold)
     (set-face-attribute face nil :weight 'normal)))
 (face-list))

;; TODO
;; if using theme misterioso:
;; 1. change the current line background
;; 2. disable bold weight
;; 3. install & configure dired-single

;; Kick off required modes
(helm-mode 1)
(ido-vertical-mode 1)
(desktop-save-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(package-selected-packages
   (quote
    (ido-vertical-mode zoom-window yasnippet vlf rtags rainbow-delimiters project-explorer powerline paredit page-break-lines ox-gfm org-bullets nlinum markdown-mode magit iedit ido-ubiquitous highlight-symbol helm-swoop helm-projectile helm-descbinds goto-chg git-timemachine git-gutter-fringe fill-column-indicator expand-region exec-path-from-shell enh-ruby-mode diminish company cmake-mode cider avy auto-complete-c-headers ac-js2)))
 '(protect-buffer-bury-p nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
