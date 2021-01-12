(require 'cl)
(require 'f)
(require 'rx)
(require 's)
(require 'text-property-search)
(require 'org)

(require 'dash)
(require 'helm-rg)


(defgroup danny nil
  "Finally!")

(defgroup danny-org-todo
  "Org overrides."
  :group 'danny
  :group 'org-todo)

(defcustom danny-org-todo-keyword-sets
  `((github . ("IDEA(i)" "RESEARCH(r)" "WIP(w)" "WAIT-FOR-REVIEW(W)" "CI(c)" "|" "MERGED(m)"))
    (blockable . ("|" "BLOCKED(b)" "CANCELLED(c)"))
    (provenance . ("NOVEL(n)" "|" "PROPRIETARY(P)" "PAPER(p)" "OSS(o)")))
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'danny-org-todo
  "Keywords for org TODO cycling (see `(org)Top').")

(defun -danny-collect-todo-keyword-sets (alist)
  ;; TODO: echo text on this according to the symbol at at the car
  (-map (-lambda ((_ . ))) alist))

(defgroup danny-elisp nil
  "Methods affecting the emacs lisp environment."
  :group 'danny
  :group 'lisp)

(defgroup danny-bindings nil
  "Methods relating to how variables and methods can be bound."
  :group 'danny-elisp)

(defcustom danny-safe-local-variables
  `(,@org-todo-keyword-faces)
  "Symbols which are set as `safe-local-variable's in `danny-theme-make-safe-local-variables'."
  :type '(repeat variable)e
  :group 'danny-bindings)

(defgroup danny-display nil
  "Affecting display in `danny-theme'."
  :group 'danny
  :group 'display)

(defcustom danny-mode-line-separator "|"
  "The string separating minor mode summaries in the `mode-line-format' in `danny-theme'."
  :type 'string
  :group 'danny-display)

(defcustom danny-mode-line-action-alist
  `(((named-group :all bos "ELisp" (* anychar) eos) . all)
    ((: bos (named-group :content (** 0 4 anychar))
            (named-group :rest (* anychar) eos)) .
     (propertize content 'was-truncated (length rest))))
  "An alist of `helm-rg-rx' inputs and corresponding output expressions."
  :type '(alist :key-type sexp :value-type sexp)
  :group 'danny-display)


;;; TODO: replace `danny--buffer-is-real'!
;; (cl-defmacro danny--memoize (zero-arg-func &key var)
;;   "Memoize the execution of a function FUNCTION-OR-SYMBOL using a hash table stored in VAR.
;;
;; 0 args -> computes lazily; returns the same value each time.
;; 1+ args -> [TODO] hashes arguments; returns the same value for equal hashes.
;;
;; If  VAR  is  not  provided,  one  will be  generated  from  the  name  `<symbol>--memoizations'.  If
;; `function-or-symbol' is not a `symbol', VAR *must* be provided or an `error' will be raised."
;;   )


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
  (replace-regexp-in-string
   (rx (| (: bos (+ space))
          (: (+ space) eos)))
   ""
   s))

(defvar-local danny--buffer-is-real 'unset
  "Memoize `danny--is-probably-a-real-file'.")

(defun danny--is-probably-a-real-file (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (pcase-exhaustive danny--buffer-is-real
      (`unset
       (setq danny--buffer-is-real
             (cond
              ((stringp (buffer-file-name buffer))
               (file-readable-p (buffer-file-name buffer)))
              ((derived-mode-p 'dired-mode)
               (f-directory-p default-directory))
              ;; TODO: no clue how the above applies to remote buffers? check info on TRAMP?
              (t nil))))
      (_ danny--buffer-is-real))))

(defun danny--get-buffer-mode-line-text ()
  "Get a string representation of the current buffer."
  (->> (if (danny--is-probably-a-real-file)
           nil
         'danny-help-ish-mode-line)
       (format-mode-line mode-line-buffer-identification)
       (danny--trim-whitespace)))

(defun danny--buffer-percentage ()
  (let ((progress (--> (point)
                       (float it)
                       (/ it (point-max)))))
       (cl-assert (and (>= progress 0.0) (<= progress 1.0)))
       progress))

(defun danny--is-t (val)
  (eq val t))

(cl-deftype danny-bool ()
  "No clue why `bool' and `boolean' don't seem to work on `t' and `nil'"
  `(satisfies (lambda (x)
                (or (eq x t)
                    (eq x nil)))))

(defun danny--within-unit-weight (x &optional force)
  (cl-check-type x number)
  (cl-check-type force danny-bool)
  (if force
      (-> x (min 0) (max 1))
    (if (and (>= x 0) (<= x 1))
        x
      nil)))

(cl-deftype danny-unitless-percentage ()
  `(and number
        (satisfies danny--within-unit-weight)))

;;; TODO: make 2*it*pi/3, ... into an infinite stream and lazy eval!
;; (defun danny--iterate-stream (start op)
;; ())

;;; TODO!!!!
;; (cl-deftype danny-numeric-range (numeric-type &key (min nil) (max nil))
;;   `(and ,numeric-type
;;         ,(if min `(satisfies (lambda (x) (<= x ,min))))
;;         ,(if max `(satisfies (lambda (x) (>= x ,max))))))

(cl-deftype danny-natural ()
  ;; `(danny-numeric-range integer :min 1)
  `(and integer
        (satisfies (lambda (x) (>= x 1)))))

(cl-defun danny--constant-pie-chart-for-circle (num-sections &key
                                                             (total (* 2 pi))
                                                             starts-at-zero
                                                             inclusive)
  (cl-check-type total number)
  (cl-check-type num-sections danny-natural)
  (cl-check-type starts-at-zero boolean)
  (cl-loop with increment = (-> total (float) (/ num-sections))
           for section-index
           from (if starts-at-zero 0 1)
           upto (let ((num-sections (if starts-at-zero (1- num-sections) num-sections)))
                  (if inclusive
                      (1+ num-sections)
                    num-sections))
           collect (* increment section-index)))

(defun danny--calc-diff (x y)
  (-> (- x y) (abs)))

(cl-defun danny--validate-radian (maybe-radian &optional (delta 1e-5))
  (and (>= maybe-radian 0)
       (< delta (danny--calc-diff maybe-radian pi))
       maybe-radian))

(cl-deftype danny-radian (&optional (delta 1e-5))
  `(and number
        (satisfies danny--validate-radian)))

(defconst danny--color-circle-space-rgb 3
  "Declare the number of channels in RGB again, for some reason.")

(defconst danny--period 1.0
  "???")

(defconst danny--scale (/ pi danny--period)
  "???")

(defun danny--make-rgb-steps ()
  (let* ((percentage (danny--buffer-percentage))
         (pi-percentage (* percentage danny--scale)))
    (-->
     (danny--constant-pie-chart-for-circle 2 :total danny--scale :starts-at-zero t :inclusive t)
     (list
      (--map (+ it pi-percentage) it)
      (--map (+ (+ it pi-percentage) danny--scale) it))
     (--map (--map (mod (/ (sin it) danny--scale) 1) it) it))))

(defun danny--get-color-for-buffer-progress ()
  (cl-destructuring-bind (positive negative) (danny--make-rgb-steps)
    (list
     (->> (apply #'color-rgb-to-hex (append positive '(2)))
          (cons 'foreground-color))
     (->> (apply #'color-rgb-to-hex (append negative '(2)))
          (cons 'background-color)))))

(defun danny--get-modified-mark ()
  "Get a list representing the modification state of the buffer, if applicable."
  (->> (concat
        (format-mode-line mode-line-modified)
        (->> (format "%d%%%%" (round (* 100 (danny--buffer-percentage))))))
       (danny--add-face-text-property 'danny-buffer-progress)
       (danny--add-face-text-property (danny--get-color-for-buffer-progress))))

(defun danny--ensure-single-string-in-list (input)
  (and (listp input)
       (= 1 (length input))
       (stringp (car input))))

(defun danny--get-modes-and-apply-properties ()
  "Get a list of strings representing major and minor modes in the current buffer."
  (pcase-exhaustive (format-mode-line mode-line-modes)
    ((helm-rg-rx (: bos "(" (named-group :content (+ anychar)) ") " eos))
     (s-split (rx space) content))))

; TODO: upstream this to emacs!
(defun danny--add-face-text-property (face text &optional append)
  (add-face-text-property
   0
   (length text)
   face
   append
   text)
  text)

(defun danny--set-help-echo (text)
  (add-text-properties
   0
   (length text)
   `(help-echo ,text)
   text)
  text)

(defun danny--try-string-against-actions (input)
  (eval `(pcase-exhaustive input
           ,@(--map `((helm-rg-rx ,(car it)) ,(cdr it)) danny-mode-line-action-alist))))

(defun danny--was-major (major-mode-description)
  (text-property-any
   0 (length major-mode-description)
   'was-major t
   major-mode-description))

(defun danny--how-much-truncated (minor-mode-description)
  (or (get-text-property 0 'was-truncated minor-mode-description)
      0))

(defun danny--format-mode-list-from-help-echo ()
  "Apply a specific face to the major vs minor modes that getmixed together in \"lighter\" strings."
  (let ((decorated-separator
         (danny--add-face-text-property 'danny-mode-line-punctuation danny-mode-line-separator)))
    (->> (pcase-exhaustive (danny--get-modes-and-apply-properties)
           (`(,major . ,minor-modes)
            (cons
             (--> major
                  (danny--add-face-text-property 'danny-major-mode-mode-line it)
                  (propertize it 'was-major t))
             (->> minor-modes
                  (--map (danny--set-help-echo it))
                  (-map #'danny--try-string-against-actions)
                  (--map (danny--add-face-text-property 'danny-minor-mode-mode-line it))
                  (--sort (cond
                           ((/= (danny--how-much-truncated it)
                                (danny--how-much-truncated other))
                            (< (danny--how-much-truncated it)
                               (danny--how-much-truncated other)))
                           (t (string-lessp it other))))))))
         (--reduce (concat acc decorated-separator it)))))


(defgroup danny-faces nil
"  \"Customization of faces in `danny-theme'.\""
  :group 'danny-display
  :group 'faces)

(defface danny-very-shadowed '((t (:foreground "#454545" :inherit shadow)))
  "A face for something which is so shadowed as to be almost invisible.

Similar to `shadow', but more."
  :group 'danny-faces)

(defgroup danny-mode-line nil
  "Customization specific to the mode line in `danny-theme'."
  :group 'danny-faces)

(defface danny-line-number '((t (:background "black" :foreground "white")))
  "Face for the line number in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-column-number '((t (:background "white" :foreground "black")))
  "Face for the column number in the mode line in `danny-theme'."
  :group 'danny-mode-line)

;;; TODO:
;;; (1) make the background on the left rise and fall with the buffer percentage.
;;; (2) if a minor mode matched a non-default regexp, order it earlier.

(defface danny-modified-string '((t (:foreground "white" :box (:line-width (2 . 2) :style pressed-button) :weight bold)))
  "Face for the modified string in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-buffer-progress '((t))
  "Face for the buffer progress indicator in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-mode-line-initial-punctuation '((t :foreground "white"))
  "Face for any punctuation in the INITIAL part of the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-mode-line-punctuation '((t (:foreground "silver" :slant italic :weight bold)))
  "Face for any punctuation in the LATER part of the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-help-ish-mode-line '((t :inverse-video t))
  "Face for any buffer not backed a real file in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-major-mode-mode-line '((t (:box (:line-width (4 . 4) :style released-button))))
  "Face for the major mode string in the mode line in `danny-theme'."
  :group 'danny-mode-line)

(defface danny-minor-mode-mode-line '((t (:slant italic)))
  "Face for each minor mode string in the mode line in `danny-theme'."
  :group 'danny-mode-line)


(deftheme danny
  "My theme!")

(defvar-local danny--theme-line-number-max-length 0
  "???")

(defvar-local danny--theme-column-number-max-length 0
  "???")

(custom-theme-set-variables
 'danny

 '(mode-line-format
   '((:eval (let* ((line-num (format-mode-line "%l"))
                   (corrected-line-num
                    (if (<= (length line-num) danny--theme-line-number-max-length)
                        (format (format "%%%ds" danny--theme-line-number-max-length) line-num)
                      (setq-local danny--theme-line-number-max-length (length line-num))
                      it)))
              (danny--add-face-text-property 'danny-line-number corrected-line-num)))
     ;; (:propertize ":" face danny-mode-line-initial-punctuation)
     (:eval (let* ((column-num (format-mode-line "%c"))
                   (corrected-column-num
                    (if (<= (length column-num) danny--theme-column-number-max-length)
                        (format (format "%%%ds" danny--theme-column-number-max-length) column-num)
                      (setq-local danny--theme-column-number-max-length (length column-num))
                      it)))
              (danny--add-face-text-property 'danny-column-number corrected-column-num)))
     ;; (:propertize ":" face danny-mode-line-initial-punctuation)
     (:eval (danny--get-buffer-mode-line-text))
     ;; (:propertize ":" face danny-mode-line-initial-punctuation)
     (:eval (danny--get-modified-mark))
     ;; (:propertize ":" face danny-mode-line-initial-punctuation)
     (:eval (danny--format-mode-list-from-help-echo))))
 ;; FIXME: *need* to somehow get all these available in this package!
 '(after-save-hook
   '(executable-make-buffer-file-executable-if-script-p output-lines-in-buffer helm-swoop--clear-cache rmail-after-save-hook))
 '(archive-visit-single-files t)
 ;; FIXME: *need* to somehow get all these available in this package!
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
 '(truncate-lines nil)
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
 '(org-indent-indentation-per-level 0)
 '(org-quote ((t (:inherit (org-block font-lock-string-face)))))
 '(org-target ((t (:foreground "yellow" :underline "#00ced1"))))
 '(global-company-mode t)
 '(helm-completion-style 'emacs)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-gnus-prefer-web-links t)
 '(org-link-keep-stored-after-insertion t)
 '(org-n-level-faces 8)
 '(org-pretty-tags-global-mode t)
 `(org-todo-keywords ,(-danny-collect-todo-keyword-sets danny-org-todo-keyword-sets))
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
 '(tutorial-warning-face ((t (:inherit font-lock-warning-face)))))


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
(provide 'danny)
