;;;###autoload
(defun sd/forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;;;###autoload
(defun sd/duplicate-line-or-region(arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defvar sd/copy-word-under-cursor-regex "[^[:word:]_]"
  "Regular expression to use when copying with `copy-word-under-cursor'.
Can be customized for each major mode.")

;;;###autoload
(defun sd/copy-word-under-cursor ()
  "Copy the word under the cursor to the kill ring."
  (interactive)
  (save-excursion
    (save-excursion (re-search-backward sd/copy-word-under-cursor-regex))
    (let ((beg (+ (match-beginning 0) 1))
          (end (re-search-forward sd/copy-word-under-cursor-regex)))
      (copy-region-as-kill beg (- end 1)))))


;;;###autoload
(defun sd/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (save-excursion
    (if (use-region-p)
	(comment-or-uncomment-region
	 (region-beginning) (region-end))
      (let ((range
	     (list (line-beginning-position)
		   (goto-char (line-end-position n)))))
	(comment-or-uncomment-region
	 (apply #'min range)
	 (apply #'max range)))
      (forward-line 1)
      (back-to-indentation))))
