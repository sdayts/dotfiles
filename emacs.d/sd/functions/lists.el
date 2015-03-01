;;;###autoload
(defun sd/flatten (x)
  "Flatten a list."
  (cond ((null x) nil)
        ((listp x) (append (sd/flatten (car x)) (sd/flatten (cdr x))))
        (t (list x))))
