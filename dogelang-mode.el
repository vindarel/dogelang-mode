;;; dogelang-mode.el --- Major mode for editing Dogelang files

;; Copyright (C) 2016 vindarel
;; Copyright (C) 2012-2014 Hisamatsu Yasuyuki

;; Author  : Vindarel, based on work by Hisamatsu Yasuyuki <yas@null.net>
;; URL     : https://gitlab.com/vindarel/dogelang-mode
;; Keywords: languages dogelang
;; Version : 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Dogelang code.

;;; Code:

(require 'font-lock)

(eval-when-compile (require 'cl))

;;
;; Group
;;

(defgroup dogelang nil
  "Major mode for editing Dogelang code."
  :prefix "dogelang-"
  :group 'languages)

(defgroup dogelang-faces nil
  "Fontification colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "dogelang-"
  :group 'dogelang)

;;
;; Syntax table
;;

(defvar dogelang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry '(0 . 127) "_   " st)
    (dolist (range '((?0 . ?9) (?A . ?Z) (?a . ?z)))
      (modify-syntax-entry range "@   " st))
    (dolist (ch '(?\t ?\f ?\r ?\s))
      (modify-syntax-entry ch "-   " st))
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{  "(}  " st)
    (modify-syntax-entry ?}  "){  " st)
    (modify-syntax-entry ?#  "<   " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?*  "_ 23b" st)
    (modify-syntax-entry ?/ "$ 14" st)
    (dolist (ch '(?, ?: ?! ??))
      (modify-syntax-entry ch ".   " st))
    (modify-syntax-entry ?\" "\"\"   " st)
    (modify-syntax-entry ?'  "\"'   " st)
    (modify-syntax-entry ?`  "$   " st)
    (modify-syntax-entry ?@  "'   " st)
    (modify-syntax-entry ?\\ "\\   " st)
    st)
  "Syntax table in use in `dogelang-mode' buffers.")

(defvar dogelang-mode-abbrev-table nil)
(define-abbrev-table 'dogelang-mode-abbrev-table ())

;;
;; Face
;;
(defface dogelang-font-lock-bold-face
  '((t :inherit bold))
  "Font Lock mode face used to highlight interpolation in Dogelang regexps."
  :group 'dogelang-faces)


(defface dogelang-font-lock-shadow-face
  '((((class color grayscale) (min-colors 88) (background light))
     :foreground "grey30"
     :weight semi-bold)
    (((class color grayscale) (min-colors 88) (background dark))
     :foreground "grey70"
     :weight semi-bold)
    (t :inherit shadow))
  "Font Lock mode face used to highlight this/it/that in Dogelang regexps."
  :group 'dogelang-faces)


;; Utility

(defun dogelang--regexp-from-symbols (sequence)
  (concat "\\_<" (regexp-opt (mapcar #'symbol-name sequence) t) "\\>"))

(defun dogelang--join-string (strings separator)
  (mapconcat #'identity strings separator))

;;
;; Search based highlighting
;;

(defvar dogelang-keywords-regexp
  (let* ((js-keywords    [if while for where subclass yield qualified import
                             otherwise raise except with finally
                                ])
         (cs-keywords    [])
         (dg-keywords    [True False Ellipsis None NotImplemented
                               abs all any ascii bin bool
                               bytearray bytes callable chr classmethod compile
                               complex delattr dict dir divmod
                               enumerate exec eval filter float format frozenset
                               getattr globals hasattr hash help hex id input int
                               isinstance issubclass iter len list
                               locals map max memoryview min next object oct open
                               ord pow print property range repr reversed round set
                               setattr slice sorted staticmethod str sum super tuple
                               type vars zip
                               ;; dg-specific built-ins
                               bind break continue flip foldl foldl1 drop dropwhile
                               iterate scanl scanl1 take takewhile
                               exhaust head fst snd tail init last

                               ])

         (js/cs-reserved [
                               ___debug__ __doc__ __file__ __name__ __package__
                               __import__ ])
         (keywords (vconcat js-keywords cs-keywords dg-keywords js/cs-reserved)))
    (dogelang--regexp-from-symbols keywords))
  "Regular expression to match ordinary keywords of Dogelang.")

(defvar dogelang-boolean-operators-regexp
  (dogelang--regexp-from-symbols [and in is isnt not or])
  "Regular expression to match boolean operators.")

(defvar dogelang-builtins-regexp
  (dogelang--regexp-from-symbols [instanceof typeof])
  "Regular expression to match 'instanceof' or 'typeof'.")

(defvar dogelang-context-regexp
  (dogelang--regexp-from-symbols [this it that])
  "Regular expression to match 'this', 'it' or 'that'.")

(defvar dogelang-boolean-regexp
  (dogelang--regexp-from-symbols [true false yes no on off null undefined])
  "Regular expression to match booleans themselves.")

(defvar dogelang-property-regexp "\\(\\w+\\)\\s-*:"
  "Regular expression to match property names.")

(defvar dogelang-negate-regexp "\\(\\w+\\s-*:[:=]\\|@@\\w*\\)"
  "Regular expression to negate highlighting.")

(defvar dogelang-instance-regexp "\\(@\\w+\\)"
  "Regular expression to match instance variables.")

(defvar dogelang-function-name-regexp
  (let* ((param     "\\s-*\\(?:\\w\\|\\.\\)+\\s-*")
         (default   "\\(?:\\(?:[:=?]\\|||\\)\\s-*\\|\\s-+or\\s-+\\)[^,\)]+?")
         (arg       (concat param "\\(?:" default "\\)?"))
         (args      (concat arg "\\(?:," arg "\\)*"))
         (arrow     "\\(?:--?\\|~~?\\)>")
         (anon-func (concat "!?\\s-*\\(?:(" args ")\\)?\\s-*" arrow))
         (func-name "\\(?1:\\w+\\)")
         ;; func = [(args)] ->
         (func-def1 (concat "\\_<" func-name "\\s-*[:=]\\s-*" anon-func))
         ;; function func [args] [(then|=>) ...]
         (func-def2 (concat "^\\s-*[!~]?\\s-*function\\s-+" func-name)))
    (format "\\(?:%s\\|%s\\)" func-def1 func-def2))
  "Regular expression to match function names.")

(defvar dogelang-class-name-regexp
  "\\_<class\\s-+\\(?:exports\.\\)?\\(\\w+\\)"
  "Regular expression to match class names.")

(defun dogelang-interpolation-matcher (bound)
  "Function to match interpolation."
  (catch 'found
    (while (re-search-forward
			"\\(#\\(?:{\\(?2:.*?\\)\\}\\|\\w+\\)\\)"
			bound t)
      (let ((face         (dogelang--get-face   (1- (point))))
            (syntax-class (dogelang--get-syntax (match-beginning 1))))
        (when (dogelang--interpolatable-p face syntax-class)
          (throw 'found t))))))

(defvar dogelang-heregex-face 'font-lock-constant-face)

(defun dogelang-comment-inside-heregex-matcher (bound)
  "Function to match comment inside heregex."
  (catch 'found
    (while (re-search-forward "\\(#\\s-.*$\\)" bound t)
      (let ((face (dogelang--get-face (1- (point)))))
        (when (memq dogelang-heregex-face face)
          (throw 'found t))))))

(defun dogelang--interpolatable-p (face syntax-class)
  (and (dogelang--interpolatable-face-p         face)
       (dogelang--interpolatable-syntax-class-p syntax-class)))

(defvar dogelang-interpolatable-faces
  '(font-lock-string-face font-lock-constant-face))

(defun dogelang--interpolatable-face-p (face)
  (catch 'found
    (dolist (f face)
      (when (memq f dogelang-interpolatable-faces) (throw 'found t)))))

(defvar dogelang-interpolatable-syntax-classes '(6))

(defun dogelang--interpolatable-syntax-class-p (syntax-class)
  (not (and syntax-class
            (memq syntax-class dogelang-interpolatable-syntax-classes))))

(defun dogelang--get-face (point)
  (let ((face (get-text-property point 'face)))
    (if (listp face) face (list face))))

(defun dogelang--get-syntax (point)
  (syntax-class (get-text-property point 'syntax-table)))

;;
;; Font lock keywords
;;

(defconst dogelang-font-lock-keywords-1
  `((,dogelang-negate-regexp         1 font-lock-negation-char-face)
    (,dogelang-instance-regexp       1 font-lock-variable-name-face)
    (,dogelang-function-name-regexp  1 font-lock-function-name-face)
    (,dogelang-class-name-regexp     1 font-lock-type-face)
    (,dogelang-property-regexp       1 font-lock-type-face)
    (,dogelang-boolean-regexp        1 font-lock-constant-face)
    (,dogelang-keywords-regexp       1 font-lock-keyword-face)))

(defconst dogelang-font-lock-keywords-2
  (append dogelang-font-lock-keywords-1
          `((,dogelang-boolean-operators-regexp 1 font-lock-builtin-face)
            (,dogelang-builtins-regexp          1 font-lock-builtin-face)
            (,dogelang-context-regexp
             (1 'dogelang-font-lock-shadow-face))
            (dogelang-interpolation-matcher
             (1 'dogelang-font-lock-bold-face prepend)
             (2 (dogelang--font-lock-pop-bold-face) t t))
            (dogelang-comment-inside-heregex-matcher
             (1 font-lock-comment-face prepend t)))))

(defun dogelang--font-lock-pop-bold-face ()
  (let* ((pop-beginning-position (match-beginning 2))
         (face (get-text-property pop-beginning-position 'face)))
    (when face (remove 'dogelang-font-lock-bold-face face))))

(defvar dogelang-font-lock-keywords dogelang-font-lock-keywords-1
  "Default `font-lock-keywords' of Dogelang mode.")

;;
;; Imenu support
;;

(defvar dogelang-imenu-generic-expression
  `((nil ,dogelang-function-name-regexp 1))
  "Imenu generic expression for Dogelang mode.")

;;
;; Commands
;;

(defvar dogelang-mode-map
  (let ((map (make-sparse-keymap)))

    map)
  "Keymap used in Dogelang mode.")

;;
;; Syntax propertize
;;

(defconst dogelang--conflicting-syntax-classes
  (let ((string-quote   7)
        (escape         9)
        (comment-start 11))
    (list comment-start string-quote escape))
  "List of syntax classes which can conflict with syntax-table property.")

(defun dogelang--put-syntax (beg end syntax)
  "Set the syntax property on the current buffer to SYNTAX between BEG and END.
SYNTAX is a string which `string-to-syntax' accepts."
  (put-text-property beg end 'syntax-table (string-to-syntax syntax)))

(defun dogelang--escape-syntax (beg end subst)
  (loop for i from beg to end
        do (let ((class (syntax-class (syntax-after i))))
             (when (memq class dogelang--conflicting-syntax-classes)
               (dogelang--put-syntax i (1+ i) subst)))))

(defun dogelang--put-enclosing-syntax (beg end syntax &optional subst)
  (dogelang--put-syntax beg (1+ beg) syntax)
  (when subst
    (dogelang--escape-syntax (1+ beg) (1- end) subst))
  (dogelang--put-syntax (1- end) end syntax))

(defun dogelang--put-syntax-multiline (beg end syntax &optional subst)
  (put-text-property beg end 'syntax-multiline    t)
  (put-text-property beg end 'font-lock-multiline t)
  (dogelang--put-enclosing-syntax beg end syntax subst))

(defun dogelang--multiline-rule (open close syntax &optional subst)
  `(,(format "\\(%s[[:ascii:]]*?%s\\)" open (or close open))
    (1 (ignore
        (dogelang--put-syntax-multiline
         (match-beginning 1) (match-end 1) ,syntax ,subst)))))

(defvar dogelang--unclosed-positions nil
  "Unclosed literals and their positions.")

(defconst dogelang-complex-syntax '(\'\'\' \"\"\" <\\\[ //)
  "List of symbols whose names are complex syntax elements.
Complex syntax elements are heredocument, string list and heregexp.")

(defun dogelang--make-unclosed-positions ()
  (let ((tbl (make-hash-table :size 11)))
    (dolist (syntax dogelang-complex-syntax)
      (puthash syntax nil tbl))
    tbl))

(defun dogelang--make-syntax-propertize-function ()
  "Return a function used for highlighting codes syntactically."
  ;; prepare rules
  (let ((skip-comment '("\\(\\s<.*\\)$" (1 (ignore))))
        (heredoc1     (dogelang--multiline-rule "'''"    nil "\"" "'"))
        (heredoc2     (dogelang--multiline-rule "\"\"\"" nil "\"" "_"))
        (skip-string  '("\\([^\"'\\]\\s\"\\S\"+?\\s\"\\)" (1 (ignore))))
        (string-list  (dogelang--multiline-rule "<\\[" "\\]>" "|"))
        (heregex      (dogelang--multiline-rule "//"     nil "\"/")))
    ;; embed rules before calling macro
    (eval `(syntax-propertize-rules
            ,skip-comment
            ,heredoc1
            ,heredoc2
            ,skip-string
            ,string-list
            ,heregex

            ;; /regular-expression/
            ((concat "\\(?:^\\|\\W\\)"
                     "\\(/\\)"
                     "\\(?:\\\\.\\|\\[\\(?:\\\\.\\|.\\)+\\]\\|[^*/\n]\\)"
                     "\\(?:\\\\.\\|\\[\\(?:\\\\.\\|.\\)+\\]\\|[^/\n]\\)*"
                     "\\(/\\)"
                     "[gimy$?]\\{0,4\\}")
             (1 "\"/") (2 "\"/"))

            ;; \string
            ("\\(\\\\[^[:space:]\n][^]}\),;[:space:]\n]*\\)"
             (1 (ignore
                 (dogelang--put-enclosing-syntax
                  (match-beginning 1) (match-end 1) "|" "'"))))

            ;; unclosed multiline literals
            ((let ((complex (mapcar #'symbol-name dogelang-complex-syntax)))
               (concat "\\(" (dogelang--join-string complex "\\|") "\\)"))
             (1 (ignore
                 (puthash (intern-soft (match-string 1)) (match-beginning 1)
                          dogelang--unclosed-positions))))
            ))))

(defconst dogelang-syntax-propertize-function
  (dogelang--make-syntax-propertize-function))

(defvar dogelang-syntax-propertize-extend-region-functions
  (append
   '(dogelang-syntax-propertize-extend-region-function-1)
   '(dogelang-syntax-propertize-extend-region-function-2)
   syntax-propertize-extend-region-functions))

(defun dogelang-syntax-propertize-extend-region-function-1 (start end)
  "Fix the range of syntax propertization from START to END."
  (let* ((new-start start)
         (new-end end)
         (min-unclosed (dogelang-minimum-unclosed)))
    (when min-unclosed
      (save-excursion
        (goto-char (cdr min-unclosed))
        (setq new-start (line-beginning-position)))
      (dogelang--clear-unclosed-positions))
    (cons new-start new-end)))

(defun dogelang-syntax-propertize-extend-region-function-2 (start end)
  "Fix the range of syntax propertization from START to END."
  (let ((new-start start)
        (new-end end))
    (save-excursion
      (goto-char start)
      (when (and (prog1 (zerop (forward-line -1)) (end-of-line))
                 (get-text-property (point) 'syntax-multiline))
        (setq new-start (point))))
    (cons new-start new-end)))

(defun dogelang-minimum-unclosed ()
  "Return the position where the first unclosed syntax appears."
  (let (kv-alist)
    (maphash #'(lambda (k v) (when v (push (cons k v) kv-alist)))
             dogelang--unclosed-positions)
    (when kv-alist
      (car (sort kv-alist #'(lambda (a b) (< (cdr a) (cdr b))))))))

(defun dogelang--clear-unclosed-positions ()
  (maphash #'(lambda (k v) (puthash k nil dogelang--unclosed-positions))
           dogelang--unclosed-positions))

(defvar font-lock-beg)
(defun dogelang-font-lock-extend-region-function ()
  (let ((min-unclosed (dogelang-minimum-unclosed)))
    (when (and min-unclosed (< (cdr min-unclosed) font-lock-beg))
      (setq font-lock-beg (cdr min-unclosed)))))

(defvar dogelang-font-lock-extend-region-functions
  (append
   '(dogelang-font-lock-extend-region-function)
   font-lock-extend-region-functions))

(defun dogelang-syntactic-face-function (state)
  "Return one of font-lock's basic face according to the parser's STATE.
STATE is a return value of `syntax-ppss'."
  (case (dogelang--string-state state)
    ((nil) 'font-lock-comment-face)
    ((?/)  'font-lock-constant-face)
    (t     'font-lock-string-face)))

(defun dogelang--string-state (state) (nth 3 state))

;;
;; Setup
;;

(defun dogelang-mode-variables ()
  "Setup buffer-local variables for `dogelang-mode'."
  ;; Syntax table
  (set-syntax-table dogelang-mode-syntax-table)
  ;; Abbrev
  (setq local-abbrev-table dogelang-mode-abbrev-table)
  ;; Comment
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-add) 1)
  ;; Imenu
  (setq imenu-case-fold-search t)
  (setq imenu-syntax-alist '(("-_$" . "w")))
  (setq imenu-generic-expression dogelang-imenu-generic-expression)
  ;; Font-lock
  (set (make-local-variable 'font-lock-defaults)
       '((dogelang-font-lock-keywords
          dogelang-font-lock-keywords-1 dogelang-font-lock-keywords-2)
         nil nil (("-_$" . "w")) nil
         (font-lock-syntactic-face-function
          . dogelang-syntactic-face-function)
         (parse-sexp-lookup-properties . t)))
  ;; Syntactic fontification
  (set (make-local-variable 'dogelang--unclosed-positions)
       (dogelang--make-unclosed-positions))

  (set (make-local-variable 'syntax-propertize-extend-region-functions)
       dogelang-syntax-propertize-extend-region-functions)
  (set (make-local-variable 'font-lock-extend-region-functions)
       dogelang-font-lock-extend-region-functions)
  (set (make-local-variable 'syntax-propertize-function)
       dogelang-syntax-propertize-function))

;;;###autoload
;; (define-derived-mode dogelang-mode prog-mode "Dogelang"
(define-derived-mode dg-mode prog-mode "Dogelang"
  "Major mode for editing Dogelang code.

Commands:

\\{dogelang-mode-map}"
  (dogelang-mode-variables))

;;
;; Customize variables
;;

(defcustom dogelang-mode-hook nil
  "Normal hook run when entering `dogelang-mode'.
See `run-hooks'."
  :type 'hook
  :group 'dogelang)

(defcustom dogelang-program-name "dogelang"
  "The command to evaluate Dogelang code."
  :type 'string
  :group 'dogelang)


;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.ls\\'" . dogelang-mode))
(add-to-list 'auto-mode-alist '("\\.dg\\'" . dogelang-mode))

(provide 'dg-mode)


;;;###compile in emacs
;;; all this section is copy/paste awesome feature from
;;; https://github.com/defunkt/coffee-mode

(defcustom dogelang-args-compile '("-c")
  "The arguments to pass to `dogelang-command' to compile a file."
  :type 'list
  :group 'dogelang)

(defcustom dogelang-command "python -m dg"
  "The DogelangScript command used for evaluating code."
  :type 'string
  :group 'dogelang)

(defcustom dogelang-compiled-buffer-name "*dogelang-compiled*"
  "The name of the scratch buffer used for compiled DogelangScript."
  :type 'string
  :group 'dogelang)

(defun dogelang-compile-region (start end)
  "Compiles a region and displays the JavaScript in a buffer called
`dogelang-compiled-buffer-name'."
  (interactive "r")

  (let ((buffer (get-buffer dogelang-compiled-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer))))

  (apply (apply-partially 'call-process-region start end
                          dogelang-command nil
                          (get-buffer-create dogelang-compiled-buffer-name)
                          nil)
         (append dogelang-args-compile (list "-s" "-p" "-b")))

  (let ((buffer (get-buffer dogelang-compiled-buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (let ((buffer-file-name "tmp.js")) (set-auto-mode)))))

(defun dogelang-compile-buffer ()
  "Compiles the current buffer and displays the JavaScript in a buffer
called `dogelang-compiled-buffer-name'."
  (interactive)
  (save-excursion
    (dogelang-compile-region (point-min) (point-max))))

;;; dogelang-mode.el ends here
