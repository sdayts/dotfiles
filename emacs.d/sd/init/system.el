;; Idea from Ryan Davis:
;; http://blog.zenspider.com/blog/2013/06/my-emacs-setup-osx.html

;; load OS specific settings
(sd/load-init-file (concat "sd/system/" (symbol-name system-type)) t)

;; load system specific settings
(sd/load-init-file (concat "sd/system/"
                             (downcase (car (split-string (system-name) "\\."))))
                     t)

;; load minimal early system settings
;; (sd/load-init-file "sd/system/minimal.el")
