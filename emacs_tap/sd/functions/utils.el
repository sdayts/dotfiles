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
(defun sd/update-header()
  (mapc
   (lambda (window)
     (with-current-buffer (window-buffer window)
       ;; don't mess with buffers that don't have a header line
       (when header-line-format
         (let ((original-format (get 'header-line-format 'original))
               (inactive-face 'mode-line-inactive)
	       (active-face 'mode-line)
	       ) ; change this to your favorite inactive header line face
           ;; if we didn't save original format yet, do it now
           (when (not original-format)
             (put 'header-line-format 'original header-line-format)
             (setq original-format header-line-format))
           ;; check if this window is selected, set faces accordingly
           (if (eq window (selected-window))
               (setq header-line-format `(:propertize ,original-format face ,active-face))
             (setq header-line-format `(:propertize ,original-format face ,inactive-face)))))))
   (window-list)))


(defun sd/comment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )

;; (setq-default mode-line-format nil)
;; (delete-rectangle)
;; rectangle-mark-mode-hook
;; rectangle-mark-mode-map

(defun sd/test()
  (print "In the focus-out-hook"
	 ;; (interactive)
	 ;; (split-window-vertically)
	 ;; (other-window 1)
	 ))
