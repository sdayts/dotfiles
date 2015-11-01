;;;###autoload
(defun sd/string-trim (str)
  "Trim whitespace from both ends of the passed string."
  (sd/regex-replace (sd/regex-replace str "[ \t]+\\'" "" t t)
                      "\\`[ \t]+" "" t t))

;;;###autoload
(defun sd/camelize (str)
  "Forces a string into CamelCase."
  (mapconcat (lambda (s)
               (if (string-match "[aeiouy]" s)
                   (capitalize s)
                 (upcase s)))
             (split-string str "[^A-Za-z0-9]")
             ""))
