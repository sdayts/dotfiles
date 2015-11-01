(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local ace-jump-search-filter
                        (lambda ()
                          (get-text-property (point) 'dired-filename)))))
