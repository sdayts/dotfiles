;; Sergey Dayts's Emacs configuration

(defun sd/load-init-file (path &optional noerror)
  "This loads a file from inside the the .emacs.d directory"
  (let ((file (file-name-sans-extension
               (expand-file-name path user-emacs-directory))))
    (load file noerror)))

;; load configuration settings by type
(sd/load-init-file "sd/init/freshen")
(sd/load-init-file "sd/init/system")
(sd/load-init-file "sd/init/autoloads")
(sd/load-init-file "sd/init/packages")
(sd/load-init-file "sd/init/compile")
(sd/load-init-file "sd/init/autohooks")
(sd/load-init-file "sd/init/settings")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rtags-ac ensime zoom-window yasnippet whole-line-or-region rtags paredit move-text hl-anything helm-swoop helm-projectile expand-region auto-complete-c-headers ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
