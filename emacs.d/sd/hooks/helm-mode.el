(setq helm-full-frame                       t ; make helm alway open in a full frame
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-mode-reverse-history           nil ; place helm command history on top
      )

 ;; helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
 
