;;; color-theme-sd.el --- A low contrast color theme.
;;;
;;; Credits:
;;; Jani Nurminen created the original theme for vim on such this port
;;; is based.
;;; Inspired by the emacs theme by Bozhidar Batsov:
;;; https://github.com/bbatsov/sd-emacs

(require 'org)
(require 'init-prefs)

;;; Color palette.
;;; `+N' suffixes indicate a color is lighter.
;;; `-N' suffixes indicate a color is darker.

(defconst sd-colors
  '((sd-fg+1     . "#FFFFEF")
    (sd-fg       . "#FFF8DC")
    (sd-fg-1     . "#656555")
    (sd-bg-2     . "#000000")
    (sd-bg-1     . "#2B2B2B")
    (sd-bg-05    . "#383838")
    (sd-bg       . "#2F4F4F")
    (sd-bg+05    . "#494949")
    (sd-bg+1     . "#4F4F4F")
    (sd-bg+2     . "#5F5F5F")
    (sd-bg+3     . "#6F6F6F")
    (sd-red+1    . "#DCA3A3")
    (sd-red      . "#CC9393")
    (sd-red-1    . "#BC8383")
    (sd-red-2    . "#AC7373")
    (sd-red-3    . "#9C6363")
    (sd-red-4    . "#8C5353")
    (sd-orange   . "#DFAF8F")
    (sd-yellow   . "yellow")
    (sd-yellow-1 . "#E0CF9F")
    (sd-yellow-2 . "#D0BF8F")
    (sd-green-1  . "#5F7F5F")
    (sd-green    . "#7F9F7F")
    (sd-green+1  . "#8FB28F")
    (sd-green+2  . "#9FC59F")
    (sd-green+3  . "#AFD8AF")
    (sd-green+4  . "#BFEBBF")
    (sd-cyan     . "#93E0E3")
    (sd-blue+1   . "#94BFF3")
    (sd-blue     . "#8CD0D3")
    (sd-blue-1   . "#7CB8BB")
    (sd-blue-2   . "#6CA0A3")
    (sd-blue-3   . "#5C888B")
    (sd-blue-4   . "#4C7073")
    (sd-blue-5   . "#366060")
    (sd-magenta  . "#DC8CC3")
    (sd-cursor-bg . "cyan")
    (sd-dark-red . "#FA8278")
    (sd-peach    . "FF9664")
    (sd-black    . "black")
    (sd-aqua     . "#00ECC8")
    (sd-bright-red . "#ED0909")))


;; ;; mode line colors
;; (set-face-attribute 'mode-line nil :background "black" :foreground "yellow" )

;; ;; Set cursor color
;; (set-cursor-color "cyan")
;; ;; set background colors
;; (set-background-color "#2F4F4F")
;; ;; color of comments
;; (set-face-foreground 'font-lock-comment-face "#FA8278")
;; ;; color of keyword
;; (set-face-foreground 'font-lock-keyword-face "#FF9664")
;; ;; color of background
;; (set-face-foreground 'default "#FFF8DC")
;; ;; color of srings
;; (set-face-foreground 'font-lock-string-face "#00ECC8")
;; ;; selection/search background/foreground
;; (set-face-attribute 'region nil :background "black" :foreground "yellow" )
;; (set-face-attribute 'isearch nil :background "yellow" :foreground "black" )
;; (set-face-attribute 'lazy-highlight nil :background "black" :foreground "cyan" )


;;; Theme definition

(defmacro with-sd-colors (&rest body)
  "Execute `BODY' in a scope with variables bound to the sd colors."
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   sd-colors))
     ,@body))

(defmacro sd-face-specs ()
  "Return a backquote which defines a list of face specs.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; basic coloring
     (button ((t (:underline t))))
     (link ((t (:foreground ,sd-yellow :underline t :weight bold))))
     (link-visited ((t (:foreground ,sd-yellow-2 :underline t :weight normal))))
     (default ((t (:foreground ,sd-fg :background ,sd-bg))))
     (cursor ((t (:foreground ,sd-fg :background ,sd-cursor-bg))))
     (escape-glyph ((t (:foreground ,sd-yellow :bold t))))
     (fringe ((t (:foreground ,sd-fg :background ,sd-bg+1))))
     (header-line ((t (:foreground ,sd-yellow :background ,sd-bg-1
                            :box (:line-width -1 :style released-button)))))
     (highlight ((t (:background ,sd-bg-05))))
     (success ((t (:foreground ,sd-green :weight bold))))
     (warning ((t (:foreground ,sd-orange :weight bold))))
     (shadow ((t (:background ,sd-bg+1))))

     ;; compilation
     (compilation-column-face ((t (:foreground ,sd-yellow))))
     (compilation-enter-directory-face ((t (:foreground ,sd-green))))
     (compilation-error-face ((t (:foreground ,sd-red-1 :weight bold :underline t))))
     (compilation-face ((t (:foreground ,sd-fg))))
     (compilation-info-face ((t (:foreground ,sd-blue))))
     (compilation-info ((t (:foreground ,sd-green+4 :underline t))))
     (compilation-leave-directory-face ((t (:foreground ,sd-green))))
     (compilation-line-face ((t (:foreground ,sd-yellow))))
     (compilation-line-number ((t (:foreground ,sd-yellow))))
     (compilation-message-face ((t (:foreground ,sd-blue))))
     (compilation-warning-face ((t (:foreground ,sd-orange :weight bold :underline t))))
     (compilation-mode-line-exit ((t (:foreground ,sd-green+2 :weight bold))))
     (compilation-mode-line-fail ((t (:foreground ,sd-red :weight bold))))
     (compilation-mode-line-run ((t (:foreground ,sd-yellow :weight bold))))

     ;; completions
     (completions-annotations ((t (:foreground ,sd-fg-1))))

     ;; grep
     (grep-context-face ((t (:foreground ,sd-fg))))
     (grep-error-face ((t (:foreground ,sd-red-1 :weight bold :underline t))))
     (grep-hit-face ((t (:foreground ,sd-blue))))
     (grep-match-face ((t (:foreground ,sd-orange :weight bold))))
     (match ((t (:background ,sd-bg-1 :foreground ,sd-orange :weight bold))))

     ;; isearch
     (isearch ((t (:foreground ,sd-black :weight bold :background ,sd-yellow))))
     (isearch-fail ((t (:foreground ,sd-fg :background ,sd-bright-red))))
     (lazy-highlight ((t (:foreground ,sd-yellow :weight bold :background ,sd-black))))

     ;; Hi-Lock
     (hi-yellow ((t (:foreground ,sd-yellow-2 :background ,sd-bg :inverse-video t))))
     (hi-pink ((t (:foreground ,sd-magenta :background ,sd-bg :inverse-video t))))
     (hi-green ((t (:foreground ,sd-green :background ,sd-bg :inverse-video t))))
     (hi-blue ((t (:foreground ,sd-cyan :background ,sd-bg :inverse-video t))))

     ;; UI
     (menu ((t (:foreground ,sd-fg :background ,sd-bg))))
     (minibuffer-prompt ((t (:foreground ,sd-yellow))))
     (mode-line
      ((,class (:foreground ,sd-green+1 :background ,sd-bg-1
                :box (:line-width -1 :style released-button)))
       (t :inverse-video t)))
     (mode-line-buffer-id ((t (:foreground ,sd-yellow :weight bold))))
     (mode-line-inactive
      ((t (:foreground ,sd-green-1 :background ,sd-bg-1 ; for powerline (previously sd-bg-05)
                            :box (:line-width -1 :style released-button)))))
     (region ((,class (:background ,sd-black :foreground ,sd-yellow))
              (t :inverse-video t)))
     (secondary-selection ((t (:background ,sd-black :foreground ,sd-yellow))))
     (trailing-whitespace ((t (:background ,sd-red))))
     (vertical-border ((t (:foreground ,sd-fg))))

     ;; font lock
     (font-lock-builtin-face ((t (:foreground ,sd-fg :weight bold))))
     (font-lock-comment-face ((t (:foreground ,sd-dark-red :italic nil))))
     (font-lock-comment-delimiter-face ((t (:foreground ,sd-green-1))))
     (font-lock-constant-face ((t (:foreground ,sd-green+4))))
     (font-lock-doc-face ((t (:foreground ,sd-green+2))))
     (font-lock-function-name-face ((t (:foreground ,sd-cyan))))
     (font-lock-keyword-face ((t (:foreground ,sd-peach :weight normal))))
     (font-lock-negation-char-face ((t (:foreground ,sd-yellow :weight bold))))
     (font-lock-preprocessor-face ((t (:foreground ,sd-blue+1))))
     (font-lock-regexp-grouping-construct ((t (:foreground ,sd-yellow :weight bold))))
     (font-lock-regexp-grouping-backslash ((t (:foreground ,sd-green :weight bold))))
     (font-lock-string-face ((t (:foreground ,sd-aqua))))
     (font-lock-type-face ((t (:foreground ,sd-blue-1))))
     (font-lock-variable-name-face ((t (:foreground ,sd-orange))))
     (font-lock-warning-face ((t (:foreground ,sd-yellow-2 :weight bold))))

     (c-annotation-face ((t (:inherit font-lock-constant-face))))

     ;; Powerline
     (exordium-powerline-active1 ((t (:background ,sd-bg-1))))
     (exordium-powerline-active2 ((t (:background ,sd-bg-05))))
     (exordium-powerline-active3 ((t (:background ,sd-yellow :foreground ,sd-bg))))
     (exordium-powerline-active4 ((t (:background ,sd-red :foreground ,sd-bg))))
     (exordium-powerline-active5 ((t (:background ,sd-green :foreground ,sd-bg))))
     (exordium-powerline-inactive1 ((t (:background ,sd-bg-1))))
     (exordium-powerline-inactive2 ((t (:background ,sd-bg-05))))
     (exordium-powerline-inactive3 ((t (:background ,sd-bg :foreground ,sd-yellow))))
     (exordium-project-name ((t (:foreground ,sd-yellow))))

     (powerline-active1 ((t (:background ,sd-bg-05 :inherit mode-line))))
     (powerline-active2 ((t (:background ,sd-bg+2 :inherit mode-line))))
     (powerline-inactive1 ((t (:background ,sd-bg+1 :inherit mode-line-inactive))))
     (powerline-inactive2 ((t (:background ,sd-bg+3 :inherit mode-line-inactive))))

     ;; auctex
     (font-latex-bold-face ((t (:inherit bold))))
     (font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
     (font-latex-sectioning-5-face ((t (:foreground ,sd-red :weight bold ))))
     (font-latex-sedate-face ((t (:foreground ,sd-yellow))))
     (font-latex-italic-face ((t (:foreground ,sd-cyan :slant italic))))
     (font-latex-string-face ((t (:inherit ,font-lock-string-face))))
     (font-latex-math-face ((t (:foreground ,sd-orange))))

     ;; auto-complete
     (ac-candidate-face ((t (:background ,sd-bg+3 :foreground ,sd-bg-2))))
     (ac-selection-face ((t (:background ,sd-blue-4 :foreground ,sd-fg))))
     (popup-tip-face ((t (:background ,sd-yellow-2 :foreground ,sd-bg-2))))
     (popup-scroll-bar-foreground-face ((t (:background ,sd-blue-5))))
     (popup-scroll-bar-background-face ((t (:background ,sd-bg-1))))
     (popup-isearch-match ((t (:background ,sd-bg :foreground ,sd-fg))))

     ;; diff
     (diff-added ((,class (:foreground ,sd-green+4 :background nil))
                  (t (:foreground ,sd-green-1 :background nil))))
     (diff-changed ((t (:foreground ,sd-yellow))))
     (diff-removed ((,class (:foreground ,sd-red :background nil))
                    (t (:foreground ,sd-red-3 :background nil))))
     (diff-refine-added ((t (:inherit diff-added :weight bold))))
     (diff-refine-change ((t (:inherit diff-changed :weight bold))))
     (diff-refine-removed ((t (:inherit diff-removed :weight bold))))
     (diff-header ((,class (:background ,sd-bg+2))
                   (t (:background ,sd-fg :foreground ,sd-bg))))
     (diff-file-header
      ((,class (:background ,sd-bg+2 :foreground ,sd-fg :bold t))
       (t (:background ,sd-fg :foreground ,sd-bg :bold t))))


     ;; dired+
     (diredp-display-msg ((t (:foreground ,sd-blue))))
     (diredp-compressed-file-suffix ((t (:foreground ,sd-orange))))
     (diredp-date-time ((t (:foreground ,sd-magenta))))
     (diredp-deletion ((t (:foreground ,sd-yellow))))
     (diredp-deletion-file-name ((t (:foreground ,sd-red))))
     (diredp-dir-heading ((t (:foreground ,sd-blue :background ,sd-bg-1))))
     (diredp-dir-priv ((t (:foreground ,sd-cyan))))
     (diredp-exec-priv ((t (:foreground ,sd-red))))
     (diredp-executable-tag ((t (:foreground ,sd-green+1))))
     (diredp-file-name ((t (:foreground ,sd-blue))))
     (diredp-file-suffix ((t (:foreground ,sd-green))))
     (diredp-flag-mark ((t (:foreground ,sd-yellow))))
     (diredp-flag-mark-line ((t (:foreground ,sd-orange))))
     (diredp-ignored-file-name ((t (:foreground ,sd-red))))
     (diredp-link-priv ((t (:foreground ,sd-yellow))))
     (diredp-mode-line-flagged ((t (:foreground ,sd-yellow))))
     (diredp-mode-line-marked ((t (:foreground ,sd-orange))))
     (diredp-no-priv ((t (:foreground ,sd-fg))))
     (diredp-number ((t (:foreground ,sd-green+1))))
     (diredp-other-priv ((t (:foreground ,sd-yellow-1))))
     (diredp-rare-priv ((t (:foreground ,sd-red-1))))
     (diredp-read-priv ((t (:foreground ,sd-green-1))))
     (diredp-symlink ((t (:foreground ,sd-yellow))))
     (diredp-write-priv ((t (:foreground ,sd-magenta))))

     ;; ediff
     (ediff-current-diff-A ((t (:foreground ,sd-fg :background ,sd-red-4))))
     (ediff-current-diff-Ancestor ((t (:foreground ,sd-fg :background ,sd-red-4))))
     (ediff-current-diff-B ((t (:foreground ,sd-fg :background ,sd-green-1))))
     (ediff-current-diff-C ((t (:foreground ,sd-fg :background ,sd-blue-5))))
     (ediff-even-diff-A ((t (:background ,sd-bg+1))))
     (ediff-even-diff-Ancestor ((t (:background ,sd-bg+1))))
     (ediff-even-diff-B ((t (:background ,sd-bg+1))))
     (ediff-even-diff-C ((t (:background ,sd-bg+1))))
     (ediff-fine-diff-A ((t (:foreground ,sd-fg :background ,sd-red-2 :weight bold))))
     (ediff-fine-diff-Ancestor ((t (:foreground ,sd-fg :background ,sd-red-2 weight bold))))
     (ediff-fine-diff-B ((t (:foreground ,sd-fg :background ,sd-green :weight bold))))
     (ediff-fine-diff-C ((t (:foreground ,sd-fg :background ,sd-blue-3 :weight bold ))))
     (ediff-odd-diff-A ((t (:background ,sd-bg+2))))
     (ediff-odd-diff-Ancestor ((t (:background ,sd-bg+2))))
     (ediff-odd-diff-B ((t (:background ,sd-bg+2))))
     (ediff-odd-diff-C ((t (:background ,sd-bg+2))))

     ;; eshell
     (eshell-prompt ((t (:foreground ,sd-yellow :weight bold))))
     (eshell-ls-archive ((t (:foreground ,sd-red-1 :weight bold))))
     (eshell-ls-backup ((t (:inherit font-lock-comment-face))))
     (eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
     (eshell-ls-directory ((t (:foreground ,sd-blue+1 :weight bold))))
     (eshell-ls-executable ((t (:foreground ,sd-red+1 :weight bold))))
     (eshell-ls-unreadable ((t (:foreground ,sd-fg))))
     (eshell-ls-missing ((t (:inherit font-lock-warning-face))))
     (eshell-ls-product ((t (:inherit font-lock-doc-face))))
     (eshell-ls-special ((t (:foreground ,sd-yellow :weight bold))))
     (eshell-ls-symlink ((t (:foreground ,sd-cyan :weight bold))))

     ;; flycheck
     (flycheck-error
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-red-1) :inherit unspecified))
       (t (:foreground ,sd-red-1 :weight bold :underline t))))
     (flycheck-warning
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-yellow) :inherit unspecified))
       (t (:foreground ,sd-yellow :weight bold :underline t))))
     (flycheck-info
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-cyan) :inherit unspecified))
       (t (:foreground ,sd-cyan :weight bold :underline t))))
     (flycheck-fringe-error ((t (:foreground ,sd-red-1 :weight bold))))
     (flycheck-fringe-warning ((t (:foreground ,sd-yellow :weight bold))))
     (flycheck-fringe-info ((t (:foreground ,sd-cyan :weight bold))))

     ;; flymake
     (flymake-errline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-red)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,sd-red-1 :weight bold :underline t))))
     (flymake-warnline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-orange)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,sd-orange :weight bold :underline t))))
     (flymake-infoline
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-green)
                    :inherit unspecified :foreground unspecified :background unspecified))
       (t (:foreground ,sd-green-1 :weight bold :underline t))))

     ;; Rtags
     (rtags-errline ((t ,(if exordium-theme-use-loud-rtags-faces
                                  `(:background ,sd-red :foreground ,sd-bg)
                                `(:underline (:color ,sd-red :style wave))))))
     (rtags-warnline ((t ,(if exordium-theme-use-loud-rtags-faces
                                   `(:background ,sd-orange :foreground ,sd-bg)
                                 `(:underline (:color ,sd-orange :style wave))))))
     (rtags-fixitline ((t ,(if exordium-theme-use-loud-rtags-faces
                                    `(:background ,sd-green :foreground ,sd-bg)
                                  `(:underline (:color ,sd-green :style wave))))))

     ;; flyspell
     (flyspell-duplicate
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-orange) :inherit unspecified))
       (t (:foreground ,sd-orange :weight bold :underline t))))
     (flyspell-incorrect
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-red) :inherit unspecified))
       (t (:foreground ,sd-red-1 :weight bold :underline t))))

     ;; full-ack
     (ack-separator ((t (:foreground ,sd-fg))))
     (ack-file ((t (:foreground ,sd-blue))))
     (ack-line ((t (:foreground ,sd-yellow))))
     (ack-match ((t (:foreground ,sd-orange :background ,sd-bg-1 :weight bold))))

     ;; git-commit
     (git-commit-comment-action  ((,class (:foreground ,sd-green+1 :weight bold))))
     (git-commit-comment-branch  ((,class (:foreground ,sd-blue+1  :weight bold))))
     (git-commit-comment-heading ((,class (:foreground ,sd-yellow  :weight bold))))

     ;; git-gutter
     (git-gutter:added ((t (:foreground ,sd-green :weight bold :inverse-video t))))
     (git-gutter:deleted ((t (:foreground ,sd-red :weight bold :inverse-video t))))
     (git-gutter:modified ((t (:foreground ,sd-magenta :weight bold :inverse-video t))))
     (git-gutter:unchanged ((t (:foreground ,sd-fg :weight bold :inverse-video t))))

     ;; git-gutter-fr
     (git-gutter-fr:added ((t (:foreground ,sd-green  :weight bold))))
     (git-gutter-fr:deleted ((t (:foreground ,sd-red :weight bold))))
     (git-gutter-fr:modified ((t (:foreground ,sd-magenta :weight bold))))

     ;; git-rebase
     (git-rebase-hash ((t (:foreground ,sd-orange))))

     ;; helm
     (helm-header
      ((t (:foreground ,sd-green :background ,sd-bg
                :underline nil :box nil))))
     (helm-source-header
      ((t (:foreground ,sd-yellow :background ,sd-bg-1
                :underline nil :weight bold
                :box (:line-width -1 :style released-button)))))
     (helm-selection ((t (:background ,sd-bg+1 :underline nil))))
     (helm-selection-line ((t (:background ,sd-bg+1))))
     (helm-visible-mark ((t (:foreground ,sd-bg :background ,sd-yellow-2))))
     (helm-candidate-number ((t (:foreground ,sd-green+4 :background ,sd-bg-1))))
     (helm-separator ((t (:foreground ,sd-red :background ,sd-bg))))
     (helm-time-zone-current ((t (:foreground ,sd-green+2 :background ,sd-bg))))
     (helm-time-zone-home ((t (:foreground ,sd-red :background ,sd-bg))))
     (helm-bookmark-addressbook ((t (:foreground ,sd-orange :background ,sd-bg))))
     (helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
     (helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
     (helm-bookmark-gnus ((t (:foreground ,sd-magenta :background ,sd-bg))))
     (helm-bookmark-info ((t (:foreground ,sd-green+2 :background ,sd-bg))))
     (helm-bookmark-man ((t (:foreground ,sd-yellow :background ,sd-bg))))
     (helm-bookmark-w3m ((t (:foreground ,sd-magenta :background ,sd-bg))))
     (helm-buffer-not-saved ((t (:foreground ,sd-red :background ,sd-bg))))
     (helm-buffer-process ((t (:foreground ,sd-cyan :background ,sd-bg))))
     (helm-buffer-saved-out ((t (:foreground ,sd-fg :background ,sd-bg))))
     (helm-buffer-size ((t (:foreground ,sd-fg-1 :background ,sd-bg))))
     (helm-ff-directory ((t (:foreground ,sd-cyan :background ,sd-bg :weight bold))))
     (helm-ff-file ((t (:foreground ,sd-fg :background ,sd-bg :weight normal))))
     (helm-ff-executable ((t (:foreground ,sd-green+2 :background ,sd-bg :weight normal))))
     (helm-ff-invalid-symlink ((t (:foreground ,sd-red :background ,sd-bg :weight bold))))
     (helm-ff-symlink ((t (:foreground ,sd-yellow :background ,sd-bg :weight bold))))
     (helm-ff-prefix ((t (:foreground ,sd-bg :background ,sd-yellow :weight normal))))
     (helm-grep-cmd-line ((t (:foreground ,sd-cyan :background ,sd-bg))))
     (helm-grep-file ((t (:foreground ,sd-fg :background ,sd-bg))))
     (helm-grep-finish ((t (:foreground ,sd-green+2 :background ,sd-bg))))
     (helm-grep-lineno ((t (:foreground ,sd-fg-1 :background ,sd-bg))))
     (helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
     (helm-grep-running ((t (:foreground ,sd-red :background ,sd-bg))))
     (helm-moccur-buffer ((t (:foreground ,sd-cyan :background ,sd-bg))))
     (helm-mu-contacts-address-face ((t (:foreground ,sd-fg-1 :background ,sd-bg))))
     (helm-mu-contacts-name-face ((t (:foreground ,sd-fg :background ,sd-bg))))

     ;; helm-swoop
     (helm-swoop-target-line-face ((t (:foreground ,sd-fg :background ,sd-bg+1))))
     (helm-swoop-target-word-face ((t (:foreground ,sd-yellow :background ,sd-bg+2 :weight bold))))

     ;; hl-line-mode
     (hl-line-face ((,class (:background ,sd-bg-05))
                    (t :weight bold)))
     (hl-line ((,class (:background ,sd-bg-05)) ; old emacsen
               (t :weight bold)))

     ;; hl-sexp
     (hl-sexp-face ((,class (:background ,sd-bg+1))
                    (t :weight bold)))

     ;; IDO
     (ido-first-match ((t (:foreground ,sd-yellow :weight bold))))
     (ido-only-match ((t (:foreground ,sd-orange :weight bold))))
     (ido-subdir ((t (:foreground ,sd-yellow))))
     (ido-indicator ((t (:foreground ,sd-yellow :background ,sd-red-4))))

     ;; iedit-mode
     (iedit-occurrence ((t (:background ,sd-bg+2 :weight bold))))

     ;; js2-mode
     (js2-warning ((t (:underline ,sd-orange))))
     (js2-error ((t (:foreground ,sd-red :weight bold))))
     (js2-jsdoc-tag ((t (:foreground ,sd-green-1))))
     (js2-jsdoc-type ((t (:foreground ,sd-green+2))))
     (js2-jsdoc-value ((t (:foreground ,sd-green+3))))
     (js2-function-param ((t (:foreground ,sd-green+3))))
     (js2-external-variable ((t (:foreground ,sd-orange))))

     ;; linum-mode
     (linum ((t (:foreground ,sd-green+2 :background ,sd-bg))))


;;;;; magit
;;;;;; headings and diffs
     (magit-section-highlight           ((t (:background ,sd-bg+05))))
     (magit-section-heading             ((t (:foreground ,sd-yellow :weight bold))))
     (magit-section-heading-selection   ((t (:foreground ,sd-orange :weight bold))))
     (magit-diff-file-heading           ((t (:weight bold))))
     (magit-diff-file-heading-highlight ((t (:background ,sd-bg+05  :weight bold))))
     (magit-diff-file-heading-selection ((t (:background ,sd-bg+05
                                                          :foreground ,sd-orange :weight bold))))
     (magit-diff-hunk-heading           ((t (:background ,sd-bg+1))))
     (magit-diff-hunk-heading-highlight ((t (:background ,sd-bg+2))))
     (magit-diff-hunk-heading-selection ((t (:background ,sd-bg+2
                                                          :foreground ,sd-orange))))
     (magit-diff-lines-heading          ((t (:background ,sd-orange
                                                          :foreground ,sd-bg+2))))
     (magit-diff-context-highlight      ((t (:background ,sd-bg+05
                                                          :foreground "grey70"))))
     (magit-diffstat-added   ((t (:foreground ,sd-green+4))))
     (magit-diffstat-removed ((t (:foreground ,sd-red))))
;;;;;; popup
     (magit-popup-heading             ((t (:foreground ,sd-yellow  :weight bold))))
     (magit-popup-key                 ((t (:foreground ,sd-green-1 :weight bold))))
     (magit-popup-argument            ((t (:foreground ,sd-green   :weight bold))))
     (magit-popup-disabled-argument   ((t (:foreground ,sd-fg-1    :weight normal))))
     (magit-popup-option-value        ((t (:foreground ,sd-blue-2  :weight bold))))
;;;;;; process
     (magit-process-ok    ((t (:foreground ,sd-green  :weight bold))))
     (magit-process-ng    ((t (:foreground ,sd-red    :weight bold))))
;;;;;; log
     (magit-log-author    ((t (:foreground ,sd-orange))))
     (magit-log-date      ((t (:foreground ,sd-fg-1))))
     (magit-log-graph     ((t (:foreground ,sd-fg+1))))
;;;;;; sequence
     (magit-sequence-pick ((t (:foreground ,sd-yellow-2))))
     (magit-sequence-stop ((t (:foreground ,sd-green))))
     (magit-sequence-part ((t (:foreground ,sd-yellow))))
     (magit-sequence-head ((t (:foreground ,sd-blue))))
     (magit-sequence-drop ((t (:foreground ,sd-red))))
     (magit-sequence-done ((t (:foreground ,sd-fg-1))))
     (magit-sequence-onto ((t (:foreground ,sd-fg-1))))
;;;;;; bisect
     (magit-bisect-good ((t (:foreground ,sd-green))))
     (magit-bisect-skip ((t (:foreground ,sd-yellow))))
     (magit-bisect-bad  ((t (:foreground ,sd-red))))
;;;;;; blame
     (magit-blame-heading ((t (:background ,sd-bg-1 :foreground ,sd-blue-2))))
     (magit-blame-hash    ((t (:background ,sd-bg-1 :foreground ,sd-blue-2))))
     (magit-blame-name    ((t (:background ,sd-bg-1 :foreground ,sd-orange))))
     (magit-blame-date    ((t (:background ,sd-bg-1 :foreground ,sd-orange))))
     (magit-blame-summary ((t (:background ,sd-bg-1 :foreground ,sd-blue-2
                                            :weight bold))))
;;;;;; references etc
     (magit-dimmed         ((t (:foreground ,sd-bg+3))))
     (magit-hash           ((t (:foreground ,sd-bg+3))))
     (magit-tag            ((t (:foreground ,sd-orange :weight bold))))
     (magit-branch-remote  ((t (:foreground ,sd-green  :weight bold))))
     (magit-branch-local   ((t (:foreground ,sd-blue   :weight bold))))
     (magit-branch-current ((t (:foreground ,sd-blue   :weight bold :box t))))
     (magit-head           ((t (:foreground ,sd-blue   :weight bold))))
     (magit-refname        ((t (:background ,sd-bg+2 :foreground ,sd-fg :weight bold))))
     (magit-refname-stash  ((t (:background ,sd-bg+2 :foreground ,sd-fg :weight bold))))
     (magit-refname-wip    ((t (:background ,sd-bg+2 :foreground ,sd-fg :weight bold))))
     (magit-signature-good      ((t (:foreground ,sd-green))))
     (magit-signature-bad       ((t (:foreground ,sd-red))))
     (magit-signature-untrusted ((t (:foreground ,sd-yellow))))
     (magit-cherry-unmatched    ((t (:foreground ,sd-cyan))))
     (magit-cherry-equivalent   ((t (:foreground ,sd-magenta))))
     (magit-reflog-commit       ((t (:foreground ,sd-green))))
     (magit-reflog-amend        ((t (:foreground ,sd-magenta))))
     (magit-reflog-merge        ((t (:foreground ,sd-green))))
     (magit-reflog-checkout     ((t (:foreground ,sd-blue))))
     (magit-reflog-reset        ((t (:foreground ,sd-red))))
     (magit-reflog-rebase       ((t (:foreground ,sd-magenta))))
     (magit-reflog-cherry-pick  ((t (:foreground ,sd-green))))
     (magit-reflog-remote       ((t (:foreground ,sd-cyan))))
     (magit-reflog-other        ((t (:foreground ,sd-cyan))))

     ;; mic-paren
     (paren-face-match ((t (:foreground ,sd-cyan :background ,sd-bg :weight bold))))
     (paren-face-mismatch ((t (:foreground ,sd-bg :background ,sd-magenta :weight bold))))
     (paren-face-no-match ((t (:foreground ,sd-bg :background ,sd-red :weight bold))))

     ;; org-mode
     (org-agenda-date-today
      ((t (:foreground ,sd-fg+1 :slant italic :weight bold))) t)
     (org-agenda-structure
      ((t (:inherit font-lock-comment-face))))
     (org-archived ((t (:foreground ,sd-fg :weight bold))))
     (org-checkbox ((t (:background ,sd-bg+2 :foreground ,sd-fg+1
                             :box (:line-width 1 :style released-button)))))
     (org-date ((t (:foreground ,sd-blue :underline t))))
     (org-deadline-announce ((t (:foreground ,sd-red-1))))
     (org-done ((t (:bold t :weight bold :foreground ,sd-green+3))))
     (org-formula ((t (:foreground ,sd-yellow-2))))
     (org-headline-done ((t (:foreground ,sd-green+3))))
     (org-hide ((t (:foreground ,sd-bg-1))))
     (org-document-title ((t
                           ,(append `(:weight bold :foreground ,sd-green+2)
                                    (if exordium-theme-use-big-org-fonts '(:height 1.44) nil)))))
     (org-level-1 ((t
                    ,(append `(:foreground ,sd-orange)
                             (if exordium-theme-use-big-org-fonts '(:height 1.44) nil)))))
     (org-level-2 ((t (:foreground ,sd-green+4))))
     (org-level-3 ((t (:foreground ,sd-blue-1))))
     (org-level-4 ((t (:foreground ,sd-yellow-2))))
     (org-level-5 ((t (:foreground ,sd-cyan))))
     (org-level-6 ((t (:foreground ,sd-green+2))))
     (org-level-7 ((t (:foreground ,sd-red-4))))
     (org-level-8 ((t (:foreground ,sd-blue-4))))
     (org-link ((t (:foreground ,sd-yellow-2 :underline t))))
     (org-scheduled ((t (:foreground ,sd-green+4))))
     (org-scheduled-previously ((t (:foreground ,sd-red))))
     (org-scheduled-today ((t (:foreground ,sd-blue+1))))
     (org-sexp-date ((t (:foreground ,sd-blue+1 :underline t))))
     (org-special-keyword ((t (:inherit font-lock-comment-face))))
     (org-table ((t (:foreground ,sd-green+2))))
     (org-tag ((t (:bold t :weight bold))))
     (org-time-grid ((t (:foreground ,sd-orange))))
     (org-todo ((t (:bold t :foreground ,sd-red :weight bold))))
     (org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
     (org-warning ((t (:bold t :foreground ,sd-red :weight bold :underline nil))))
     (org-column ((t (:background ,sd-bg-1))))
     (org-column-title ((t (:background ,sd-bg-1 :underline t :weight bold))))
     (org-mode-line-clock ((t (:foreground ,sd-fg :background ,sd-bg-1))))
     (org-mode-line-clock-overrun ((t (:foreground ,sd-bg :background ,sd-red-1))))
     (org-ellipsis ((t (:foreground ,sd-yellow-1 :underline t))))
     (org-footnote ((t (:foreground ,sd-cyan :underline t))))

     ;; outline
     (outline-1 ((t (:foreground ,sd-orange))))
     (outline-2 ((t (:foreground ,sd-green+4))))
     (outline-3 ((t (:foreground ,sd-blue-1))))
     (outline-4 ((t (:foreground ,sd-yellow-2))))
     (outline-5 ((t (:foreground ,sd-cyan))))
     (outline-6 ((t (:foreground ,sd-green+2))))
     (outline-7 ((t (:foreground ,sd-red-4))))
     (outline-8 ((t (:foreground ,sd-blue-4))))

     ;; Clojure
     (clojure-test-failure-face ((t (:foreground ,sd-orange :weight bold :underline t))))
     (clojure-test-error-face ((t (:foreground ,sd-red :weight bold :underline t))))
     (clojure-test-success-face ((t (:foreground ,sd-green+1 :weight bold :underline t))))

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((t (:foreground ,sd-fg))))
     (rainbow-delimiters-depth-2-face ((t (:foreground ,sd-green+4))))
     (rainbow-delimiters-depth-3-face ((t (:foreground ,sd-yellow-2))))
     (rainbow-delimiters-depth-4-face ((t (:foreground ,sd-cyan))))
     (rainbow-delimiters-depth-5-face ((t (:foreground ,sd-green+2))))
     (rainbow-delimiters-depth-6-face ((t (:foreground ,sd-blue+1))))
     (rainbow-delimiters-depth-7-face ((t (:foreground ,sd-yellow-1))))
     (rainbow-delimiters-depth-8-face ((t (:foreground ,sd-green+1))))
     (rainbow-delimiters-depth-9-face ((t (:foreground ,sd-blue-2))))
     (rainbow-delimiters-depth-10-face ((t (:foreground ,sd-orange))))
     (rainbow-delimiters-depth-11-face ((t (:foreground ,sd-green))))
     (rainbow-delimiters-depth-12-face ((t (:foreground ,sd-blue-5))))

     ;; sh-mode
     (sh-hesd-redoc     ((t (:foreground ,sd-yellow :bold t))))
     (sh-quoted-exec ((t (:foreground ,sd-red))))

     ;; show-paren
     (show-paren-mismatch ((t (:foreground ,sd-red+1 :background ,sd-bg+3 :weight bold))))
     (show-paren-match ((t (:background ,sd-bg+3 :weight bold))))

     ;; smartparens
     (sp-show-pair-mismatch-face ((t (:foreground ,sd-red+1 :background ,sd-bg+3 :weight bold))))
     (sp-show-pair-match-face ((t (:background ,sd-bg+3 :weight bold))))

     ;; sml-mode-line
     '(sml-modeline-end-face ((t :inherit default :width condensed)))

     ;; SLIME
     (slime-repl-output-face ((t (:foreground ,sd-red))))
     (slime-repl-inputed-output-face ((t (:foreground ,sd-green))))
     (slime-error-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-red)))
       (t
        (:underline ,sd-red))))
     (slime-warning-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-orange)))
       (t
        (:underline ,sd-orange))))
     (slime-style-warning-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-yellow)))
       (t
        (:underline ,sd-yellow))))
     (slime-note-face
      ((((supports :underline (:style wave)))
        (:underline (:style wave :color ,sd-green)))
       (t
        (:underline ,sd-green))))
     (slime-highlight-face ((t (:inherit highlight))))

     ;; speedbar
     (speedbar-button-face ((t (:foreground ,sd-green+2))))
     (speedbar-directory-face ((t (:foreground ,sd-cyan))))
     (speedbar-file-face ((t (:foreground ,sd-fg))))
     (speedbar-highlight-face ((t (:foreground ,sd-bg :background ,sd-green+2))))
     (speedbar-selected-face ((t (:foreground ,sd-red))))
     (speedbar-separator-face ((t (:foreground ,sd-bg :background ,sd-blue-1))))
     (speedbar-tag-face ((t (:foreground ,sd-yellow))))

     ;; tabbar
     (tabbar-button ((t (:foreground ,sd-fg :background ,sd-bg))))
     (tabbar-selected ((t (:foreground ,sd-fg :background ,sd-bg
                                :box (:line-width -1 :style pressed-button)))))
     (tabbar-unselected ((t (:foreground ,sd-fg :background ,sd-bg+1
                                  :box (:line-width -1 :style released-button)))))

     ;; term
     (term-color-black ((t (:foreground ,sd-bg :background ,sd-bg-1))))
     (term-color-red ((t (:foreground ,sd-red-2 :background ,sd-red-4))))
     (term-color-green ((t (:foreground ,sd-green :background ,sd-green+2))))
     (term-color-yellow ((t (:foreground ,sd-orange :background ,sd-yellow))))
     (term-color-blue ((t (:foreground ,sd-blue-1 :background ,sd-blue-4))))
     (term-color-magenta ((t (:foreground ,sd-magenta :background ,sd-red))))
     (term-color-cyan ((t (:foreground ,sd-cyan :background ,sd-blue))))
     (term-color-white ((t (:foreground ,sd-fg :background ,sd-fg-1))))
     (term-default-fg-color ((t (:inherit term-color-white))))
     (term-default-bg-color ((t (:inherit term-color-black))))

     ;; undo-tree
     (undo-tree-visualizer-active-branch-face ((t (:foreground ,sd-fg+1 :weight bold))))
     (undo-tree-visualizer-current-face ((t (:foreground ,sd-red-1 :weight bold))))
     (undo-tree-visualizer-default-face ((t (:foreground ,sd-fg))))
     (undo-tree-visualizer-register-face ((t (:foreground ,sd-yellow))))
     (undo-tree-visualizer-unmodified-face ((t (:foreground ,sd-cyan))))

     ;; whitespace-mode
     (whitespace-space ((t (:background ,sd-bg+1 :foreground ,sd-bg+1))))
     (whitespace-hspace ((t (:background ,sd-bg+1 :foreground ,sd-bg+1))))
     (whitespace-tab ((t (:background ,sd-red-1))))
     (whitespace-newline ((t (:foreground ,sd-bg+1))))
     (whitespace-trailing ((t (:background ,sd-red))))
     (whitespace-line ((t (:background ,sd-bg :foreground ,sd-magenta))))
     (whitespace-space-before-tab ((t (:background ,sd-orange :foreground ,sd-orange))))
     (whitespace-indentation ((t (:background ,sd-yellow :foreground ,sd-red))))
     (whitespace-empty ((t (:background ,sd-yellow))))
     (whitespace-space-after-tab ((t (:background ,sd-yellow :foreground ,sd-red))))

     ;; which-func-mode
     (which-func ((t (:foreground ,sd-green+4))))

     ;; yascroll
     (yascroll:thumb-text-area ((t (:background ,sd-bg-1))))
     (yascroll:thumb-fringe ((t (:background ,sd-bg-1 :foreground ,sd-bg-1))))
     )))


(defmacro sd-variables ()
  "Return a backquote which defines a list of variables.
It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(
     (ansi-color-names-vector [,sd-bg ,sd-red ,sd-green ,sd-yellow
                                         ,sd-blue ,sd-magenta ,sd-cyan ,sd-fg])
;;;;; fill-column-indicator
     (fci-rule-color ,sd-bg+05)
;;;;; vc-annotate
     (vc-annotate-color-map
      '(( 20. . ,sd-red-1)
        ( 40. . ,sd-red)
        ( 60. . ,sd-orange)
        ( 80. . ,sd-yellow-2)
        (100. . ,sd-yellow-1)
        (120. . ,sd-yellow)
        (140. . ,sd-green-1)
        (160. . ,sd-green)
        (180. . ,sd-green+1)
        (200. . ,sd-green+2)
        (220. . ,sd-green+3)
        (240. . ,sd-green+4)
        (260. . ,sd-cyan)
        (280. . ,sd-blue-2)
        (300. . ,sd-blue-1)
        (320. . ,sd-blue)
        (340. . ,sd-blue+1)
        (360. . ,sd-magenta)))
     (vc-annotate-very-old-color ,sd-magenta)
     (vc-annotate-background ,sd-bg-1)
     )))

(defun define-sd-theme ()
  "Define the sd theme (only one variant for now)"
  (deftheme sd "A low contrast theme")
  (with-sd-colors
   (apply 'custom-theme-set-faces 'sd (sd-face-specs)))
  (with-sd-colors
   (apply 'custom-theme-set-variables 'sd (sd-variables)))
  (provide-theme 'sd))

;;; Extra functions

(defun set-sd-extra-org-statuses ()
  (require 'org)
  (with-sd-colors
   (setq org-todo-keyword-faces
         `(("WORK" . (;:background ,sd-yellow
                      :foreground ,sd-yellow
                      :weight bold :box nil))
           ("WAIT" . (;:background ,sd-orange
                      :foreground ,sd-orange
                      :weight bold :box nil))))))

;;; Debugging functions

(defun set-colors-sd ()
  "Sets the colors to the sd theme"
  (interactive)
  (with-sd-colors
    (apply 'custom-set-faces (sd-face-specs))))

(provide 'color-theme-sd)

;;; color-theme-sd.el ends here
