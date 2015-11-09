;; Commen line
(local-unset-key (kbd "C-c C-c"))
(local-set-key (kbd "C-c C-c") 'sd/comment-line-or-region)
(local-unset-key (kbd "C-c C-u"))
(local-set-key (kbd "C-c C-u") 'sd/comment-line-or-region)

;; Duplicate line
(local-unset-key (kbd "C-c C-v"))
(local-set-key (kbd "C-c C-v") 'sd/duplicate-line-or-region)
