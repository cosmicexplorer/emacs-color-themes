(require 'cl)
(require 's)
(require 'text-property-search)

(require 'dash)


(defgroup danny nil
  "Finally!")

(defgroup danny-bindings nil
  "Methods relating to how variables and methods can be bound."
  :group 'danny)

(defcustom danny-safe-local-variables
  '(org-todo-keyword-faces)
  "Symbols which are set as `safe-local-variable's in `danny-theme-make-safe-local-variables'."
  :type '(repeat variable)
  :group 'danny-bindings)

(defgroup danny-display nil
  "Affecting display in `danny-theme'."
  :group 'danny
  :group 'display)

(defcustom danny-mode-line-separator "|"
  "The string separating minor mode summaries in the `mode-line-format' in `danny-theme'."
  :type 'string
  :group 'danny-display)


(defun danny--get-time-zone-and-offset-nicely ()
  "Returns (<local zone name>, <utc offset (int)>, <nicely-formatted offset>)."
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

(defun danny--get-a-nice-time-display ()
  (cl-destructuring-bind (local-zone-name utc-offset nicely-formatted-utc-offset)
      (danny--get-time-zone-and-offset-nicely)
    (let ((full-time (format-time-string "%H:%M:%S"))
          (unix-time (format-time-string "%s")))
      (format "%s %d[%s] %s" full-time utc-offset local-zone-name unix-time))))

(defun danny--trim-whitespace (s)
  ;; TODO: use `rx' if it's not too new?
  (replace-regexp-in-string "\\`\\s-+\\|\\s-+\\'" "" s))

(defun danny--is-probably-a-real-file (&optional buffer)
  (and (stringp (buffer-file-name buffer))
       (file-readable-p (buffer-file-name buffer))))

(defun danny--get-buffer-mode-line-text ()
  "Get a string representation of the current buffer."
  (->> (if (danny--is-probably-a-real-file)
           nil
         'danny-help-ish-mode-line)
       (format-mode-line mode-line-buffer-identification)
       (danny--trim-whitespace)))

(defun danny--ensure-single-string-in-list (input)
  (and (listp input)
       (= 1 (length input))
       (stringp (car input))))

;;; TODO: upstream this!!!
(defun danny--split-modes-by-property (property-name input-text)
  "Returns a list of substrings with a string value in INPUT-TEXT for the named PROPERTY-NAME."
  (cl-check-type property-name symbol)
  (cl-check-type input-text string)
  (--> (with-temp-buffer
         (insert input-text)
         (cl-loop initially (goto-char (point-min))
                  for match = (text-property-search-forward
                               property-name
                               nil
                               (lambda (_arg property-value)
                                 (stringp property-value)))
                  while match
                  collect (buffer-substring
                           (prop-match-beginning match)
                           (prop-match-end match))))
       ;; TODO: We assume the first is the  "major". There are occasionally minor modes which expect
       ;; to be formatted right next to the major mode representation.
       (cl-destructuring-bind (major . minor-modes) it
         (cons
          (danny--add-face-text-property 'danny-major-mode-mode-line major)
          (->> minor-modes
               (-map #'s-split-words)
               (-flatten-n 1)
               (--map (danny--add-face-text-property 'danny-minor-mode-mode-line it)))))))

; TODO: upstream this to emacs!
(defun danny--add-face-text-property (face text)
  (add-face-text-property
   0
   (length text)
   face
   nil
   text)
  text)

(defun danny--format-mode-list-from-help-echo ()
  "Apply a specific face to the major vs minor modes that getmixed together in \"lighter\" strings."
  (->> (danny--split-modes-by-property
        'help-echo
        (format-mode-line mode-line-modes))
       (--reduce (concat acc danny-mode-line-separator it))))


(defgroup danny-faces nil
  "Customization of faces in `danny-theme'."
  :group 'danny-display
  :group 'faces)

(defface danny-very-shadowed '((t (:foreground "#454545" :inherit shadow)))
  "A face for something which is so shadowed as to be almost invisible.

Similar to `shadow', but more."
  :group 'danny-faces)

(defgroup danny-mode-line nil
  "Customization specific to the mode line in `danny-theme'."
  :group 'danny-faces)

(defface danny-line-number '((t))
  "Face for the line number in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-column-number '((t))
  "Face for the column number in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-modified-string '((t))
  "Face for the modified string in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-buffer-progress '((t))
  "Face for the buffer progress indicator in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-mode-line-punctuation '((t))
  "Face for any punctuation in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-help-ish-mode-line '((t))
  "Face for any buffer not backed a real file in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-major-mode-mode-line '((t))
  "Face for the major mode string in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-minor-mode-mode-line '((t))
  "Face for the minor mode string in the mode line in `danny-theme'."
  :group 'danny-mode-line)


(deftheme danny
  "My theme!")

(custom-theme-set-variables
 'danny

 '(mode-line-format
   '(
    (:propertize "%l" face danny-line-number)
    (:propertize ":" face danny-mode-line-punctuation)
    (:propertize "%c" face danny-column-number)
    (:propertize "|" face danny-mode-line-punctuation)
    (:propertize "%p" face danny-buffer-progress)
    (:propertize "|" face danny-mode-line-punctuation)
    (:propertize mode-line-modified face danny-modified-string)
    (:eval (danny--get-buffer-mode-line-text))
    (:eval (danny--format-mode-list-from-help-echo))
    ))
 '(after-save-hook
   '(executable-make-buffer-file-executable-if-script-p output-lines-in-buffer helm-swoop--clear-cache rmail-after-save-hook))
 '(archive-visit-single-files t)
 '(before-save-hook
   '(copyright-update time-stamp nuke-whitespace-except-this-line))
 '(bug-reference-bug-regexp
   "\\(?1:[Bb]ug ?#?\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z+-]+/\\|TODO[ (]?#\\|FIXME[ (]?#\\)(?\\(?2:[0-9]+\\(?:#[0-9]+\\)?\\))?\\|\\(?:#\\(?2:[0-9]+\\)\\)")
 '(byte-count-to-string-function (lambda (n) (file-size-human-readable n `si "@" "B")))
 '(char-fold-symmetric t)
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
 '(cursor-in-non-selected-windows 'hollow)
 '(cursor-type 'box)
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
 '(global-company-mode nil)
 '(global-display-fill-column-indicator-mode nil)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-linum-mode nil)
 '(global-mark-ring-max 200)
 '(global-pabbrev-mode t)
 '(global-undo-tree-mode t)
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
 '(minibuffer-line-format '((:eval (danny--get-a-nice-time-display))))
 '(minibuffer-line-mode t)
 '(minibuffer-line-refresh-interval 1)
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
 '(mode-require-final-newline t)
 '(mouse-autoselect-window nil)
 '(multi-isearch-isearch t)
 '(next-line-add-newlines t)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files nil)
 '(org-agenda-property-list ''("CUSTOM_ID"))
 '(org-catch-invisible-edits 'smart)
 '(org-confirm-babel-evaluate nil)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-highlight-latex-and-related '(native latex script entities))
 '(org-link-elisp-confirm-function 'y-or-n-p)
 '(org-link-from-user-regexp nil)
 '(org-link-shell-confirm-function 'y-or-n-p)
 '(org-list-allow-alphabetical t)
 '(org-pretty-tags-mode-lighter "/$$$")
 '(org-src-fontify-natively t)
 '(org-startup-folded t)
 '(org-support-shift-select 'always)
 '(org-use-property-inheritance t)
 '(orgit-log-save-arguments t)
 '(pabbrev-idle-timer-verbose nil)
 '(pabbrev-minimal-expansion-p t)
 '(pabbrev-mode-hook '(pabbrev-mode-set-explicitly))
 '(pcomplete-autolist t)
 '(pcomplete-compare-entry-function 'file-newer-than-file-p)
 '(pcomplete-expand-before-complete t)
 '(pcomplete-file-ignore "\\.Xauthority\\'")
 '(pcomplete-help nil)
 '(pcomplete-recexact t)
 '(pcomplete-termination-string "")
 '(python-indent-def-block-scale 1)
 '(rainbow-ansi-colors t)
 '(rainbow-html-colors t)
 '(rainbow-latex-colors t)
 '(rainbow-r-colors t)
 '(rainbow-x-colors t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(read-quoted-char-radix 10)
 '(require-final-newline t)
 '(rust-indent-offset 2)
 '(safe-local-variable-values
   '((highlight-80+-columns . 100)
     (highlight-stages-mode)
     (highlight-sexp-mode)
     (highlight-80+-mode)
     (TeX-auto-untabify . t)
     (comment-start . //)
     (f3-default-directory . /home/cosmicexplorer/projects/active/ping-pong)
     (f3-before-args "-not" "(" "-ipath" "*.git/*" "-or" "-ipath" "*.pants.d/*" "-or" "-iname" "*.pyc" ")")
     (f3-default-directory . project)
     (c-file-offsets
      (block-close . 0)
      (brace-list-close . 0)
      (brace-list-entry . 0)
      (brace-list-intro . +)
      (case-label . 0)
      (class-close . 0)
      (defun-block-intro . +)
      (defun-close . 0)
      (defun-open . 0)
      (else-clause . 0)
      (inclass . +)
      (label . 0)
      (statement . 0)
      (statement-block-intro . +)
      (statement-case-intro . +)
      (statement-cont . +)
      (substatement . +)
      (topmost-intro . 0))
     (markdown-list-indent-width . 2)
     (markdown-enable-math . t)
     (c-offsets-alist
      (inexpr-class . +)
      (inexpr-statement . +)
      (lambda-intro-cont . +)
      (inlambda . c-lineup-inexpr-block)
      (template-args-cont c-lineup-template-args +)
      (incomposition . +)
      (inmodule . +)
      (innamespace . +)
      (inextern-lang . +)
      (composition-close . 0)
      (module-close . 0)
      (namespace-close . 0)
      (extern-lang-close . 0)
      (composition-open . 0)
      (module-open . 0)
      (namespace-open . 0)
      (extern-lang-open . 0)
      (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
      (objc-method-args-cont . c-lineup-ObjC-method-args)
      (objc-method-intro .
                         [0])
      (friend . 0)
      (cpp-define-intro c-lineup-cpp-define +)
      (cpp-macro-cont . +)
      (cpp-macro .
                 [0])
      (inclass . +)
      (stream-op . c-lineup-streamop)
      (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
      (arglist-cont c-lineup-gcc-asm-reg 0)
      (arglist-intro . +)
      (catch-clause . 0)
      (else-clause . 0)
      (do-while-closure . 0)
      (label . 2)
      (access-label . -)
      (substatement-label . 2)
      (substatement . +)
      (statement-case-open . 0)
      (statement-case-intro . +)
      (statement-block-intro . +)
      (statement-cont . +)
      (statement . 0)
      (brace-entry-open . 0)
      (brace-list-entry . 0)
      (brace-list-intro . +)
      (brace-list-close . 0)
      (brace-list-open . 0)
      (block-close . 0)
      (inher-cont . c-lineup-multi-inher)
      (inher-intro . +)
      (member-init-cont . c-lineup-multi-inher)
      (member-init-intro . +)
      (annotation-var-cont . +)
      (annotation-top-cont . 0)
      (topmost-intro-cont . c-lineup-topmost-intro-cont)
      (topmost-intro . 0)
      (knr-argdecl . 0)
      (func-decl-cont . +)
      (inline-close . 0)
      (inline-open . +)
      (class-close . 0)
      (class-open . 0)
      (defun-block-intro . +)
      (defun-close . 0)
      (defun-open . 0)
      (string . c-lineup-dont-change)
      (arglist-close . c-lineup-arglist)
      (substatement-open . 0)
      (case-label . 0)
      (block-open . 0)
      (c . 1)
      (comment-intro . 0)
      (knr-argdecl-intro . -))
     (c-cleanup-list scope-operator brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces list-close-comma defun-close-semi)
     (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks)
     (c-hanging-colons-alist
      (member-init-intro before)
      (inher-intro)
      (case-label after)
      (label after)
      (access-label after))
     (c-hanging-braces-alist
      (substatement-open after)
      (brace-list-open after)
      (brace-entry-open)
      (defun-open after)
      (class-open after)
      (inline-open after)
      (block-open after)
      (block-close . c-snug-do-while)
      (statement-case-open after)
      (substatement after))
     (c-comment-only-line-offset . 0)
     (c-tab-always-indent . t)
     (f3-before-args "-not" "(" "-ipath" "*.git*" "-or" "-ipath" "*.pants.d*" ")")
     (js2-basic-offset . 4)
     (no-gfm)
     (Syntax . ANSI-Common-Lisp)
     (Base . 10)
     (no-gfm . t)
     (major-mode . sh-mode)
     (TeX-master . "proposal")
     (add-log-time-format lambda nil
                          (progn
                            (setq tz
                                  (getenv "TZ"))
                            (setq time
                                  (format-time-string "%a %b %e %H:%M:%S %Z %Y"
                                                      (current-time)))
                            (set-time-zone-rule tz)
                            time))
     (destroy-whitespace)
     (nil)
     (flycheck-mode)))
 '(save-place-file "saveplace")
 '(save-place-limit nil)
 '(save-place-mode t)
 '(save-place-save-skipped nil)
 '(save-place-version-control 'nospecial)
 '(save-some-buffers-default-predicate
   '(lambda nil
      (not
       (string-match-p "\\`\\*.*\\*\\'"
                       (buffer-name)))))
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-preserve-screen-position 1)
 '(scroll-step 1)
 '(search-default-mode 'char-fold-to-regexp)
 '(sh-basic-offset 2)
 '(shift-select-mode t)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(smart-tab-completion-functions-alist '((lisp-mode . slime-complete-symbol)))
 '(smart-tab-default-functions-alist
   '((org-mode . org-cycle)
     (markdown-mode . toggle-subtree-markdown)))
 '(smart-tab-disabled-major-modes nil)
 '(smart-tab-using-hippie-expand t)
 '(text-quoting-style 'grave)
 '(timer-max-repeats 2)
 '(tool-bar-mode nil)
 '(track-eol t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(undo-outer-limit 5000000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-enable-undo-in-region nil)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(version-control t)
 '(visible-bell t)
 '(window-divider-default-bottom-width 3)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 3)
 '(window-divider-mode nil)
 '(woman-fill-frame nil)
 '(woman-imenu t)
 '(words-include-escapes t)
 '(x-stretch-cursor t)
 '(xmllint-pretty-level 2)
 '(yank-pop-change-selection t)
 '(org-indent-indentation-per-level 0))

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
 '(hl-line ((t (:box (:line-width (3 . 2) :color "sandy brown")))))
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
 '(variable-pitch  ((t (:background  "#1f0000"  :distant-foreground  "magenta" :foreground  "yellow"
                                   :box(:line-width    (6    .    2)   :color    "#cc69b4"    :style
                                                       pressed-button)))))
 '(custom-button ((t (:background "blue" :foreground "cyan" :box (:line-width (2 . 2) :style released-button)))))
 '(helm-rg-active-arg-face ((t (:foreground "red"))))
 '(help-argument-name ((t (:inherit bold-italic))))
 '(helpful-heading ((t (:foreground "light green" :box (:line-width (5 . 5) :color "yellow" :style pressed-button) :weight bold))))
 '(isearch ((t (:background "gainsboro" :foreground "dark  magenta" :box (:line-width (2 . 2) :color
                                                                                     "black"  :style
                                                                                     pressed-button)))))
 '(link ((t (:extend t :foreground "cyan1" :underline t))))
 '(link-visited ((t (:inherit link :extend t :foreground "violet"))))
 '(tutorial-warning-face ((t (:inherit font-lock-warning-face))))
 '(danny-help-ish-mode-line ((t :inverse-video t))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun danny-theme-make-safe-local-variables ()
  (interactive)
  (let ((ignore-callback (lambda (&rest _) t)))
    (--map (put it 'safe-local-variable ignore-callback)
           danny-safe-local-variables)))

;;;###autoload
(defun danny-setup ()
  (interactive)
  (enable-theme 'danny)
  ;; NB: I do not know why this needs to be enabled both now and later.  But otherwise our
  ;; `hl-line' face is not applied.
  (add-hook 'window-setup-hook (z (enable-theme 'danny)) 99)
  (danny-theme-make-safe-local-variables))

(provide-theme 'danny)
