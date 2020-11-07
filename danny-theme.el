(defgroup danny nil
  "Finally.")

(defun danny-get-time-zone-and-offset-nicely ()
  (cl-destructuring-bind (utc-offset-seconds local-zone-name) (current-time-zone)
    (cl-assert (% utc-offset-seconds 3600) t
               "The number of seconds should be equal to an hour. We are directly assuming we are not in Sri Lanka for ease of programming.")
    (let* ((utc-offset (/ utc-offset-seconds 3600))
           (parity-sym (cond
                        ((> utc-offset-seconds 0) "+")
                        ((< utc-offset-seconds 0) "-")
                        (t "=")))
           (abs-offset (abs utc-offset)))
      `(
        ,local-zone-name
        ,utc-offset
        ,(format "UTC%s%d" parity-sym abs-offset)
        ))))

(defun danny-get-a-nice-updating-time-display ()
  (cl-destructuring-bind (local-zone-name utc-offset nicely-formatted-utc-offset)
      (danny-get-time-zone-and-offset-nicely)
    (let ((full-time (format-time-string "%H:%M:%S"))
          (unix-time (format-time-string "%s")))
      (format "%s %d[%s] %s" full-time utc-offset local-zone-name unix-time))))

(deftheme danny
  "My theme!")

(defface danny-very-shadowed '((t (:foreground "#454545")))
  "A face for something which is so shadowed as to be almost invisible.

Similar to `shadow', but more."
  :group 'danny)

(custom-theme-set-variables
 'danny

 '(Info-breadcrumbs-depth 17)
 '(Info-history-skip-intermediate-nodes nil)
 '(Info-scroll-prefer-subnodes t)
 '(TeX-newline-function 'reindent-then-newline-and-indent)
 '(abbrev-all-caps t)
 '(abbrev-suggest t)
 '(adaptive-fill-mode t)
 '(ag-default-search-fn 'ag-regexp)
 '(ag-highlight-search t)
 '(async-shell-command-buffer 'new-buffer)
 '(auto-revert-avoid-polling t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-mode-text " LIVE")
 '(auto-revert-tail-mode-text " TAIL")
 '(auto-revert-verbose t)
 '(auto-save-default nil)
 '(auto-save-no-message nil)
 '(backup-by-copying-when-mismatch nil)
 '(backup-by-copying-when-privileged-mismatch nil)
 '(blink-cursor-mode nil)
 '(buffer-offer-save 'always)
 '(cloc-use-3rd-gen nil)
 '(coffee-args-compile '("-c" "--bare" "--no-header"))
 '(TeX-engine 'luatex)
 '(asm-comment-char 35)
 '(book-txt-view-buffer-contents 'fill)
 '(book-txt-view-font-size 12)
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-program "firefox-nightly")
 '(coffee-indent-like-python-mode t)
 '(coffee-switch-to-compile-buffer t)
 '(coffee-tab-width 2)
 '(comint-completion-addsuffix "")
 '(comint-completion-autolist t)
 '(comint-completion-fignore '("#" "~" "%"))
 '(comint-completion-recexact t)
 '(comint-prompt-read-only t)
 '(company-continue-commands
   '(not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs completion-at-point))
 '(company-dabbrev-code-modes
   '(prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-tooltip-align-annotations t)
 '(compilation-scroll-output t)
 '(compile-command "make")
 '(completion-auto-help 'lazy)
 '(completion-cycle-threshold t)
 '(completion-pcm-word-delimiters "-_./:|       ")
 '(completions-format 'horizontal)
 '(cperl-hairy nil)
 '(create-lockfiles nil)
 '(cursor-in-non-selected-windows '(hbar . 12))
 '(cursor-type 'hollow)
 '(dabbrev-case-replace 'case-replace)
 '(debug-on-error t)
 '(default-justification 'full)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(dired-auto-revert-buffer t)
 '(dired-clean-up-buffers-too nil)
 '(dired-guess-shell-alist-user
   '(("\\.zip\\'" "unzip")
     ("\\.tar\\.xz\\'" "tar xpf" "sudo pacman -U")
     ("\\.exe\\'" "wine")
     ("\\.\\(?:jpe?g\\|png\\|gif\\|bmp\\)\\'" "display" "gimp")
     ("\\.\\(?:pdf\\|ps\\)\\'" "evince")
     ("\\.coffee\\'" "coffee")
     ("\\.pl\\'" "perl")
     ("\\.jar\\'" "java -jar")
     ("^[^\\.]+\\'"
      (concat user-emacs-directory "exec-file.sh"))
     ("\\.bash\\'" "bash")
     ("\\.zsh\\'" "zsh")
     ("\\.sh\\'" "sh")
     ("\\.py\\'" "python" "python2" "python3")
     ("\\..+x\\'" "libreoffice")
     ("\\.html\\'" "chromium" "firefox")
     ("\\.vi\\'" "labview")
     ("\\.R\\'" "Rscript")
     ("\\.svg\\'" "inkscape")
     ("\\.\\(?:mp3\\|wav\\|wmv\\)\\'" "cvlc --play-and-exit")
     ("\\.*" "xdg-open")))
 '(dired-kept-versions 2)
 '(dired-listing-switches "-lavFh")
 '(dired-no-confirm t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(display-fill-column-indicator-character ?@)
 '(display-raw-bytes-as-hex t)
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(echo-keystrokes 0.1)
 '(edebug-eval-macro-args t)
 '(edebug-save-windows nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(electric-pair-pairs
   '((40 . 41)
     (123 . 125)
     (34 . 34)
     ((nth 0 electric-quote-chars)
      nth 1 electric-quote-chars)
     ((nth 2 electric-quote-chars)
      nth 3 electric-quote-chars)))
 '(electric-pair-skip-self 'electric-pair-default-skip-self)
 '(electric-pair-text-pairs
   '((34 . 34)
     ((nth 0 electric-quote-chars)
      nth 1 electric-quote-chars)
     ((nth 2 electric-quote-chars)
      nth 3 electric-quote-chars)))
 '(enable-recursive-minibuffers t)
 '(ensime-eldoc-hints 'all)
 '(ensime-startup-notification nil)
 '(ensime-startup-snapshot-notification nil)
 '(eshell-cmpl-suffix-list '(47 58 64))
 '(ess-S-assign "_")
 '(ess-own-style-list
   '((ess-indent-offset . 4)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-line)
     (ess-offset-block . prev-line)
     (ess-offset-continued . straight)
     (ess-align-nested-calls "ifelse")
     (ess-align-arguments-in-calls "function[[:space:]]*(")
     (ess-align-continuations-in-calls . t)
     (ess-align-blocks control-flow)
     (ess-indent-from-lhs arguments fun-decl-opening)
     (ess-indent-from-chain-start . t)
     (ess-indent-with-fancy-comments . t)))
 '(ess-style 'OWN)
 '(eval-expression-print-level nil)
 '(f3-before-args
   '("-not" "(" "-ipath" "*.git/*" "-or" "-ipath" "*.pants.d/*" "-or" "-name" "*.pyc" ")"))
 '(file-precious-flag t)
 '(fill-column 100)
 '(fill-individual-varying-indent t)
 '(fill-nobreak-invisible t)
 '(fill-nobreak-predicate '(fill-single-word-nobreak-p fill-single-char-nobreak-p))
 '(fill-separate-heterogeneous-words-with-space t)
 '(fix-info-rename-buffer-mode t)
 '(git-gutter:update-hooks
   '(after-save-hook after-revert-hook find-file-hook after-change-major-mode-hook text-scale-mode-hook magit-revert-buffer-hook magit-status-refresh-hook magit-run-git-hook))
 '(git-gutter:update-interval 1)
 '(git-gutter:window-width 0)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-linum-mode nil)
 '(global-mark-ring-max 200)
 '(global-pabbrev-mode t)
 '(global-undo-tree-mode t)
 '(goal-column 100)
 '(helm-ag--preview-highlight-matches 'any)
 '(helm-ag--preview-max-matches 500)
 '(helm-ag-do-display-preview nil)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-ag-use-agignore t)
 '(helm-ag-use-emacs-lisp-regexp nil)
 '(helm-completing-read-handlers-alist
   '((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (tmm-menubar)
     (load-file . ido)))
 '(helm-completion-style 'emacs)
 '(helm-ff-fuzzy-matching nil)
 '(helm-follow-mode-persistent t)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-rg-default-directory 'git-root)
 '(helm-swoop-pre-input-function (lambda nil (thing-at-point 'symbol)))
 '(highlight-80+-columns 100)
 '(highlight-nonselected-windows t)
 '(highlight-parentheses-background-colors '("light goldenrod"))
 '(highlight-parentheses-colors '("chocolate" "magenta" "tomato" "yellow"))
 '(highlight-stages-global-mode t)
 '(hl-sexp-background-color nil)
 '(idle-update-delay 0.5)
 '(ido-enable-flex-matching t)
 '(ido-mode 'both nil (ido))
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(line-move-visual nil)
 '(linum-relative-current-symbol ">")
 '(linum-relative-format "%4s")
 '(linum-relative-plusp-offset 0)
 '(lua-indent-level 2)
 '(magit-auto-revert-immediately nil)
 '(magit-auto-revert-mode t)
 '(magit-auto-revert-tracked-only nil)
 '(magit-display-buffer-function 'magit-display-buffer-traditional)
 '(magit-no-confirm '(kill-process stage-all-changes unstage-all-changes))
 '(magit-push-always-verify nil)
 '(magit-remote-add-set-remote\.pushDefault 'ask)
 '(magit-revert-buffers 5 t)
 '(make-backup-files nil)
 '(mark-ring-max 160)
 '(markdown-export-async t)
 '(markdown-gfm-additional-languages nil)
 '(markdown-indent-on-enter nil)
 '(markdown-list-indent-width 2)
 '(markdown-live-preview-delete-export 'delete-on-destroy)
 '(markdown-live-preview-do-sync nil)
 '(max-lisp-eval-depth 160000)
 '(max-specpdl-size 250000)
 '(menu-bar-mode nil)
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-eldef-shorten-default t)
 '(minibuffer-electric-default-mode t)

 )

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
