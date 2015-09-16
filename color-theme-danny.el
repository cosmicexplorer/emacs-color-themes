;;;###autoload
(defun color-theme-danny ()
  "Yet another color theme. I like it a bit."
  (interactive)
  (color-theme-install
   '(color-theme-danny
     ((foreground-color . "green")
      (background-color . "black")
      (background-mode . light))
     (underline ((t (:foreground "yellow" :underline t))))
     (italic ((t (:foreground "#ff1493" :italic t))))
     (bold ((t (:foreground "cyan" :bold t))))
     (bold-italic ((t (:foreground "dark magenta" :bold t :italic t))))
     (font-lock-comment-face
      ((t (:foreground "white" :background "dark magenta"))))
     (font-lock-warning-face
      ((t (:foreground "red" :background "white"))))
     (modeline-buffer-id ((t (:background "orange"
                                          :foreground "dark red"
                                          :underline nil))))
     (modeline-mousable ((t (:background "#00ff00" :foreground "white"))))
     (mode-line
      ((t (:background "#11aa11"
                       :foreground "black"
                       :overline nil
                       :underline nil))))
     (mode-line-inactive
      ((t (:background "#2f2f00"
                       :foreground "black"
                       :underline nil
                       :overline nil
                       :weight light))))
     (org-block-begin-line
      ((t (:underline "#A7A6AA" :foreground "#5555ee"
                      :background "#333333"))))
     (org-block-background
      ((t (:background "#001133"))))
     (org-block-end-line
      ((t (:overline "#A7A6AA" :foreground "#5555ee"
                     :background "#333333"))))
     (ediff-even-diff-A
      ((t (:background "dark blue"))))
     (ediff-even-diff-B
      ((t (:background "dark blue"))))
     (ediff-odd-diff-A
      ((t (:background "dark blue"))))
     (ediff-odd-diff-B
      ((t (:background "dark blue"))))
     (magit-section-highlight
      ((t (:background "#430"))))
     (magit-diff-context-highlight
      ((t (:background "grey23" :foreground "grey76"))))
     (magit-diff-context ((t (:background "grey23" :foreground "grey76"))))
     (magit-diff-removed-highlight
      ((t (:background "#440000" :foreground "red"))))
     (magit-diff-removed ((t (:background "#440000" :foreground "red"))))
     (diff-removed ((t (:background "#440000" :foreground "red"))))
     (magit-diff-added-highlight
      ((t (:background "#004400" :foreground "green"))))
     (magit-diff-added ((t (:background "#004400" :foreground "green"))))
     (diff-added ((t (:background "#004400" :foreground "green"))))
     (magit-diff-hunk-heading
      ((t (:background "#550" :foreground "#dd0"))))
     (diff-hunk-header
      ((t (:background "#dd0" :foreground "black"))))
     (diff-function
      ((t (:background "#550" :foreground "#dd0"))))
     (magit-diff-hunk-heading-highlight
      ((t (:background "#dd0" :foreground "black"))))
     (diff-hunk-header
      ((t (:background "#550" :foreground "#dd0"))))
     (magit-blame-heading
      ((t (:background "#550" :foreground "#dd0"))))
     ;; i don't like this since it blots out normal text backgrounds like
     ;; comments and colors
     (hl-line
      ((t (:background "#002500" :foreground nil)))))))

(provide 'color-theme-danny)
