;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "sd/functions/files" "../../../.emacs.d/sd/functions/files.el"
;;;;;;  "676f8a911cfdb4ec11c1bc8bcfa55c2b")
;;; Generated autoloads from ../../../.emacs.d/sd/functions/files.el

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

;;;### (autoloads nil "sd/functions/lists" "../../../.emacs.d/sd/functions/lists.el"
;;;;;;  "9aa583ced79adbf4f6fd83377a49cfe1")
;;; Generated autoloads from ../../../.emacs.d/sd/functions/lists.el

(autoload 'sd/flatten "sd/functions/lists" "\
Flatten a list.

\(fn X)" nil nil)

;;;***

;;;### (autoloads nil "sd/functions/regexen" "../../../.emacs.d/sd/functions/regexen.el"
;;;;;;  "bc762e862431a0c81a5f8644c110f22a")
;;; Generated autoloads from ../../../.emacs.d/sd/functions/regexen.el

(autoload 'sd/regex-replace "sd/functions/regexen" "\
Replace a regular expression in the passed string, if it occurs.

\(fn STR REGEX REPLACEMENT &optional FIXEDCASE LITERAL)" nil nil)

(autoload 'sd/regex-replace-all "sd/functions/regexen" "\
Replace a regular expression everywhere it occurs in the passed string.

\(fn STR REGEX REPLACEMENT &optional FIXEDCASE LITERAL)" nil nil)

;;;***

;;;### (autoloads nil "sd/functions/strings" "../../../.emacs.d/sd/functions/strings.el"
;;;;;;  "e10c89b366876d1d06d3b845c5010eb6")
;;; Generated autoloads from ../../../.emacs.d/sd/functions/strings.el

(autoload 'sd/string-trim "sd/functions/strings" "\
Trim whitespace from both ends of the passed string.

\(fn STR)" nil nil)

(autoload 'sd/camelize "sd/functions/strings" "\
Forces a string into CamelCase.

\(fn STR)" nil nil)

;;;***

;;;### (autoloads nil "sd/functions/utils" "../../../.emacs.d/sd/functions/utils.el"
;;;;;;  "a8ae8584b49facbfd1d904adaf8bc6b5")
;;; Generated autoloads from ../../../.emacs.d/sd/functions/utils.el

(autoload 'sd/forward-or-backward-sexp "sd/functions/utils" "\
Go to the matching parenthesis character if one is adjacent to point.

\(fn &optional ARG)" t nil)

(autoload 'sd/duplicate-line-or-region "sd/functions/utils" "\
Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated.

\(fn ARG)" t nil)

(autoload 'sd/copy-word-under-cursor "sd/functions/utils" "\
Copy the word under the cursor to the kill ring.

\(fn)" t nil)

(autoload 'sd/update-header "sd/functions/utils" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/sd/commands/commands.el"
;;;;;;  "../../../.emacs.d/sd/functions/files.el" "../../../.emacs.d/sd/functions/lists.el"
;;;;;;  "../../../.emacs.d/sd/functions/regexen.el" "../../../.emacs.d/sd/functions/strings.el"
;;;;;;  "../../../.emacs.d/sd/functions/utils.el") (21756 54778 413941
;;;;;;  225000))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
