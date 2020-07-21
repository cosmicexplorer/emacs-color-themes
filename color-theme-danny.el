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
     (font-lock-string-face
      ((t (:foreground "orange" :background "#4b4b4b"))))
     (font-lock-warning-face
      ((t (:foreground "red" :background "white"))))
     (mode-line-buffer-id ((t (:background "orange"
                                          :foreground "dark red"
                                          :underline nil))))
     (mode-line-mousable ((t (:background "#00ff00" :foreground "white"))))
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
     (hl-line
      ((t (:background nil :foreground nil :underline nil))))
     (rainbow-delimiters-depth-1-face
      ((t (:foreground "#ffffff"))))
     (rainbow-delimiters-depth-2-face
      ((t (:foreground "#9922cc"))))
     (rainbow-delimiters-depth-3-face
      ((t (:foreground "#008833"))))
     (rainbow-delimiters-depth-4-face
      ((t (:foreground "#816501"))))
     (rainbow-delimiters-depth-5-face
      ((t (:foreground "#5294ff"))))
     (rainbow-delimiters-depth-6-face
      ((t (:foreground "#824919"))))
     (rainbow-delimiters-depth-7-face
      ((t (:foreground "#2aa"))))
     (rainbow-delimiters-depth-8-face
      ((t (:foreground "#902398"))))
     (rainbow-delimiters-depth-9-face
      ((t (:foreground "#988131"))))
     (highlight-stages-negative-level-face
      ((t (:background "#ffffff"))))
     (highlight-stages-level-1-face
      ((t (:background "#9922cc"))))
     (highlight-stages-level-2-face
      ((t (:background "#008833"))))
     (highlight-stages-level-3-face
      ((t (:background "#816501"))))
     (highlight-stages-higher-level-face
      ((t (:background "#5294ff")))))))

(provide 'color-theme-danny)
