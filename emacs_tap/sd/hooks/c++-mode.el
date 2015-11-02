;; window movement
(local-unset-key (kbd "M-j"))
(local-unset-key (kbd "M-<down>"))
(local-set-key (kbd "M-j") 'windmove-down)
(local-set-key (kbd "M-<down>") 'windmove-down)

;; Comment bindings
(local-unset-key (kbd "C-c C-c"))
(local-set-key (kbd "C-c C-c") 'sd/comment-line-or-region)
(local-unset-key (kbd "C-c C-u"))
(local-set-key (kbd "C-c C-u") 'sd/comment-line-or-region)
