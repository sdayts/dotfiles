(defun sd/load-settings ()
  "Loads all Lisp files in the settings subdirectory of the init directory."
  (dolist (file (directory-files (concat user-emacs-directory "sd/settings")
                                 nil
                                 "\\.el$"))
    (sd/load-init-file (concat "sd/settings/" file))))

(sd/load-settings)
