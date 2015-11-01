(defun sd/load-functions ()
  "Loads all Lisp files in the functions subdirectory"

  (dolist (file (directory-files (concat (file-name-directory (or load-file-name buffer-file-name)) "../functions/") t ".+\\.el$"))
    (let ((library (file-name-sans-extension file)))
      (message "%s" file)
      (load library nil t))))

(sd/load-functions)
