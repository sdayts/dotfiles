;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "sd/functions/files" "sd/functions/files.el"
;;;;;;  (21745 52571 223630 686000))
;;; Generated autoloads from sd/functions/files.el

(autoload 'sd/read-file-to-string "sd/functions/files" "\
Reads the contents of path into a string.

\(fn PATH)" nil nil)

(autoload 'sd/find-subpath-in-path "sd/functions/files" "\
Walks up the passed path hunting for subpath at each level.

\(fn SUBPATH PATH)" nil nil)

(autoload 'sd/find-in-path "sd/functions/files" "\
Walks up the current path hunting for subpath at each level.

\(fn SUBPATH)" nil nil)

;;;***

;;;### (autoloads nil "sd/functions/lists" "sd/functions/lists.el"
;;;;;;  (21745 52590 257143 128000))
;;; Generated autoloads from sd/functions/lists.el

(autoload 'sd/flatten "sd/functions/lists" "\
Flatten a list.

\(fn X)" nil nil)

;;;***

;;;### (autoloads nil "sd/functions/regexen" "sd/functions/regexen.el"
;;;;;;  (21745 52615 417717 418000))
;;; Generated autoloads from sd/functions/regexen.el

(autoload 'sd/regex-replace "sd/functions/regexen" "\
Replace a regular expression in the passed string, if it occurs.

\(fn STR REGEX REPLACEMENT &optional FIXEDCASE LITERAL)" nil nil)

(autoload 'sd/regex-replace-all "sd/functions/regexen" "\
Replace a regular expression everywhere it occurs in the passed string.

\(fn STR REGEX REPLACEMENT &optional FIXEDCASE LITERAL)" nil nil)

;;;***

;;;### (autoloads nil "sd/functions/strings" "sd/functions/strings.el"
;;;;;;  (21745 52629 248630 226000))
;;; Generated autoloads from sd/functions/strings.el

(autoload 'sd/string-trim "sd/functions/strings" "\
Trim whitespace from both ends of the passed string.

\(fn STR)" nil nil)

(autoload 'sd/camelize "sd/functions/strings" "\
Forces a string into CamelCase.

\(fn STR)" nil nil)

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
