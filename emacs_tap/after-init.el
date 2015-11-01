;; Sergey Dayts's Emacs configuration

(message "File name directory: (%s)" (file-name-directory load-file-name))

(defun sd/load-init-file (path &optional noerror)
  "This loads a file from inside the the .emacs.d directory"
  (let ((file (file-name-sans-extension
               (expand-file-name path (file-name-directory load-file-name)))))
    (message "Loading init-file (%s)" file)
    (load file noerror)))

;; load configuration settings by type
(sd/load-init-file "sd/init/functions")
;(sd/load-init-file "sd/init/freshen")
;(sd/load-init-file "sd/init/system")
;(sd/load-init-file "sd/init/autoloads")
;(sd/load-init-file "sd/init/packages")
;(sd/load-init-file "sd/init/compile")
(sd/load-init-file "sd/init/autohooks")
;(sd/load-init-file "sd/init/settings")
