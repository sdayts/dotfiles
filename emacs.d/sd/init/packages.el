;; Idea from Ryan Davis:
;; http://blog.zenspider.com/blog/2013/06/my-emacs-setup-packages.html

(require 'package)

(dolist (repo '(
;;	("elpa"      . "http://tromey.com/elpa/")
;;      ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives repo))

(defun sd/package-refresh-and-install (name)
  "Ensure we have a fresh package list, then install."
  (package-refresh-contents)
  (package-install name))

(defun sd/package-install-unless-installed (name)
  "Install a package by name unless it is already installed."
  (or (package-installed-p name) (sd/package-refresh-and-install name)))

(defun sd/package-details-for (name)
  "Safely pull out package details across Emacs versions."
  (let ((v (cdr (assoc name package-archive-contents))))
    (and v (if (consp v)
               (car v) ; for Emacs 24.4+
             v))))

(defun sd/package-version-for (package)
  "Get the version of a loaded package."
  (package-desc-vers (sd/package-details-for package)))

(defun sd/package-delete-by-name (package)
  "Remove a package by name."
  (package-delete (symbol-name package)
                  (package-version-join (sd/package-version-for package))))

(defun sd/package-delete-unless-listed (packages)
  "Remove packages not explicitly declared."
  (let ((packages-and-dependencies (sd/packages-requirements packages)))
    (dolist (package (mapcar 'car package-alist))
      (unless (memq package packages-and-dependencies)
        (sd/package-delete-by-name package)))))

(defun sd/packages-requirements (packages)
  "List of dependencies for packages."
  (delete-dups (apply 'append (mapcar 'sd/package-requirements packages))))

(defun sd/package-requirements (package)
  "List of recursive dependencies for a package."
  (let ((package-info (sd/package-details-for package)))
     (cond ((null package-info) (list package))
           (t
            (sd/flatten
             (cons package
                   (mapcar 'sd/package-requirements
                           (mapcar 'car (package-desc-reqs package-info)))))))))

(defun sd/package-install-and-remove-to-match-list (&rest packages)
  "Sync packages so the installed list matches the passed list."
  (package-initialize)
  (condition-case nil ;; added to handle no-network situations
      (mapc 'sd/package-install-unless-installed packages)
    (error (message "Couldn't install package. No network connection?")))
  (sd/package-delete-unless-listed packages))

(sd/package-install-and-remove-to-match-list
;; 'ag
;; 'coffee-mode
;; 'color-theme-sanityinc-tomorrow
;; 'dash
;; 'dart-mode
;;;; 'fill-column-indicator
;; 'haml-mode
;; 'idomenu
;; 'inf-ruby
;; 'magit
;; 'markdown-mode
;; 'multiple-cursors
;; 'paredit
;; 'rainbow-mode
;; 'rhtml-mode
;; 'ruby-compilation
;; 'rust-mode
;; 'scss-mode
;; 'smartparens
;; 'undo-tree
;; 'window-number
;; 'yagist
 ;; 'yaml-mode
 'ace-jump-mode
 'expand-region
 'helm
 'helm-projectile
 'helm-swoop
 ;; 'highlight-symbol
 'hl-anything
 'move-text
 'paredit
 'projectile
 'whole-line-or-region
 'yasnippet
 'zoom-window
 )

;; vendored packages
;; (sd/load-init-file "sd/vendor/rcodetools")
;; (sd/load-init-file "sd/vendor/scad-mode")
