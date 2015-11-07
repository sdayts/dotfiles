(setq-local ace-jump-search-filter
            (lambda ()
              (get-text-property (point) 'dired-filename)))

;; (setq-local dired-omit-files "\\.o$|\\.tsk$")

;; ignore files with certain extensions
(add-to-list 'dired-omit-extensions ".tsk")
(setq-default dired-omit-mode t)
