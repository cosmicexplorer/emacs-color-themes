(defgroup danny nil
  "Finally.")

(deftheme danny
  "My theme!")

(defface danny-very-shadowed '((t (:foreground "#454545")))
  "A face for something which is so shadowed as to be almost invisible.

Similar to `shadow', but more."
  :group 'danny)

;; (custom-theme-set-variables)

(custom-theme-set-faces
 'danny

 '(default ((t (:height 100 :background "black" :foreground "green"))))
 '(underline ((t (:inherit default :foreground "yellow" :underline t))))
 '(italic ((t (:inherit default :foreground "#ff1493" :slant italic))))
 '(bold ((t (:inherit default :foreground "cyan" :weight extra-bold))))
 '(bold-italic ((t (:inherit (bold italic) :foreground "dark magenta" :slant italic :weight bold))))
 '(font-lock-comment-face
   ((t (:foreground "white" :background "dark magenta"))))
 '(font-lock-string-face
   ((t (:foreground "orange" :background "#4b4b4b"))))
 '(font-lock-warning-face
   ((t (:foreground "red" :background "white"))))
 '(mode-line-buffer-id ((t (:background "orange"
                                        :foreground "dark red"
                                        :underline nil))))
 '(mode-line-mousable ((t (:background "#00ff00" :foreground "white"))))
 '(mode-line
   ((t (:background "#cd3e89"
                    :foreground "#2f2f77"
                    :overline nil
                    :underline nil))))
 '(mode-line-inactive
   ((t (:background "#2f2f77"
                    :foreground "#cd5c5c"
                    :underline nil
                    :overline nil
                    :weight light))))
 '(minibuffer-line ((t (:background "#37013f" :foreground "dark cyan"))))
 '(org-block-begin-line
   ((t (:underline "#A7A6AA" :foreground "#5555ee"
                   :background "#333333"))))
 '(org-block-background
   ((t (:background "#001133"))))
 '(org-block-end-line
   ((t (:overline "#A7A6AA" :foreground "#5555ee"
                  :background "#333333"))))
 '(ediff-even-diff-A
   ((t (:background "dark blue"))))
 '(ediff-even-diff-B
   ((t (:background "dark blue"))))
 '(ediff-odd-diff-A
   ((t (:background "dark blue"))))
 '(ediff-odd-diff-B
   ((t (:background "dark blue"))))
 '(magit-section-highlight
   ((t (:background "#430"))))
 '(magit-diff-context-highlight
   ((t (:background "grey23" :foreground "grey76"))))
 '(magit-diff-context ((t (:background "grey23" :foreground "grey76"))))
 '(magit-diff-removed-highlight
   ((t (:background "#440000" :foreground "red"))))
 '(magit-diff-removed ((t (:background "#440000" :foreground "red"))))
 '(diff-removed ((t (:background "#440000" :foreground "red"))))
 '(magit-diff-added-highlight
   ((t (:background "#004400" :foreground "green"))))
 '(magit-diff-added ((t (:background "#004400" :foreground "green"))))
 '(diff-added ((t (:background "#004400" :foreground "green"))))
 '(magit-diff-hunk-heading
   ((t (:background "#550" :foreground "#dd0"))))
 '(diff-hunk-header
   ((t (:background "#dd0" :foreground "black"))))
 '(diff-function
   ((t (:background "#550" :foreground "#dd0"))))
 '(magit-diff-hunk-heading-highlight
   ((t (:background "#dd0" :foreground "black"))))
 '(diff-hunk-header
   ((t (:background "#550" :foreground "#dd0"))))
 '(magit-blame-heading
   ((t (:background "#550" :foreground "#dd0"))))
 '(rainbow-delimiters-depth-1-face
   ((t (:foreground "#ffffff"))))
 '(rainbow-delimiters-depth-2-face
   ((t (:foreground "#9922cc"))))
 '(rainbow-delimiters-depth-3-face
   ((t (:foreground "#008833"))))
 '(rainbow-delimiters-depth-4-face
   ((t (:foreground "#816501"))))
 '(rainbow-delimiters-depth-5-face
   ((t (:foreground "#5294ff"))))
 '(rainbow-delimiters-depth-6-face
   ((t (:foreground "#824919"))))
 '(rainbow-delimiters-depth-7-face
   ((t (:foreground "#2aa"))))
 '(rainbow-delimiters-depth-8-face
   ((t (:foreground "#902398"))))
 '(rainbow-delimiters-depth-9-face
   ((t (:foreground "#988131"))))
 '(highlight-stages-negative-level-face
   ((t (:background "#ffffff"))))
 '(highlight-stages-level-1-face
   ((t (:background "#2b1c62"))))
 '(highlight-stages-level-2-face
   ((t (:background "#008833"))))
 '(highlight-stages-level-3-face
   ((t (:background "#816501"))))
 '(highlight-stages-higher-level-face
   ((t (:background "#5294ff"))))
 '(browse-url-button ((t (:inherit link))))
 '(cperl-no-trailing-whitespace-face ((t (:underline nil))))
 '(cursor ((t (:background "cyan"))))
 '(font-latex-script-char-face ((t (:foreground "burlywood"))))
 '(font-lock-comment-face ((t (:extend t :background "dark magenta" :foreground "white"))))
 '(hl-line ((t (:extend t :box (:line-width (3 . 2) :color "sandy brown")))))
 '(org-agenda-property-face ((t (:inherit font-lock-comment-face :extend nil))))
 '(region ((t (:extend t :background "blue3" :foreground "white"))))
 '(window-divider ((t (:foreground "deep sky blue"))))
 '(window-divider-first-pixel ((t (:foreground "chartreuse"))))
 '(window-divider-last-pixel ((t (:foreground "pale turquoise"))))
 '(woman-unknown ((t (:background "#333333" :foreground "#ff0000"))))
 '(hl-sexp-face ((t (:background "#4b3b4b" :extend t))))
 '(trailing-whitespace ((t (:extend t :background "red1"))))
 '(fill-column-indicator ((t (:inherit danny-very-shadowed :inverse-video t))))
 '(info-xref ((t  (:inherit link :background  "#884488" :box (:line-width  (2 . 2)  :color "magenta" :style released-button)))))
 '(Info-quoted ((t (:inherit inform-color :box (:line-width (2 . 2) :color "gray33" :style pressed-button)))))
 '(variable-pitch ((t (:background "#1f0000" :distant-foreground "magenta" :foreground "yellow" :box (:line-width (6 . 2) :color "#cc69b4" :style pressed-button))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'danny)
