;;; chf.el --- Chombo Fortran mode for GNU Emacs

;; Copyright (C) 1986, 1993-1995, 1997-2016 Free Software Foundation,
;; Inc.

;; Author: Michael D. Prange <prange@erl.mit.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>

;; Keywords: fortran, languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode is Fortran mode with a few extras for Chombo Fortran
;; syntax. Fortran mode is documented in the Emacs manual. ChF
;; introduces no new functions, only modifying font-lock and
;; indentation (for now).
;;
;; Note that it is for editing Fortran77 or Fortran90 fixed source
;; form.  For editing Fortran 90 free format source, use `f90-mode'
;; (f90.el).  It is meant to support the GNU Fortran language
;; implemented by g77 (its extensions to Fortran77 and
;; interpretations, e.g. of backslash in strings).

;;; History:

;; Fortran mode was upgraded by Stephen A. Wood (saw@cebaf.gov).
;; Chombo Fortran syntax added by Joshua Christopher
;; (joshuac@rams.colostate.edu). 

;; We acknowledge many contributions and valuable suggestions by
;; Lawrence R. Dodd, Ralf Fassel, Ralph Finch, Stephen Gildea,
;; Dr. Anil Gokhale, Ulrich Mueller, Mark Neale, Eric Prestemon,
;; Gary Sabot and Richard Stallman.


;;; Code:

;; Todo:

;; * Tidy it all up (more)!
;; * Implement insertion and removal of statement continuations in
;;   mixed f77/f90 style, with the first `&' past column 72 and the
;;   second in column 6.
;; * Support any other extensions to f77 grokked by GNU Fortran I've missed.

;; silence compiler
(defvar dabbrev-case-fold-search)
(defvar gud-find-expr-function)
(defvar imenu-case-fold-search)
(defvar imenu-syntax-alist)
(defvar comment-region-function)
(defvar uncomment-region-function)

(defgroup chf nil
  "Major mode for editing fixed format ChF code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :link '(custom-manual "(emacs)ChF")
  :group 'languages)

(defgroup chf-indent nil
  "Indentation variables in ChF mode."
  :prefix "chf-"
  :group  'chf)

(defgroup chf-comment nil
  "Comment-handling variables in ChF mode."
  :prefix "chf-"
  :group  'chf)


(defcustom chf-tab-mode-default nil
  "Default tabbing/carriage control style for empty files in ChF mode.
A non-nil value specifies tab-digit style of continuation control.
A value of nil specifies that continuation lines are marked
with a character in column 6."
  :type  'boolean
  :safe  'booleanp
  :group 'chf-indent)

;; TODO add more detail of what tab mode is to doc string.
(defcustom chf-tab-mode-string
  (propertize "/t" 'help-echo "This buffer is in ChF TAB mode"
              'mouse-face 'mode-line-highlight
              'local-map
              (make-mode-line-mouse-map 'mouse-1
                                        (lambda ()
                                          (interactive)
                                          (describe-variable
                                           'chf-tab-mode-string))))
  "String to appear in mode line in TAB format buffers.
See Info node `(emacs)ForIndent Cont'."
  :type  'string
  :risky t
  :group 'chf-indent)

(defcustom chf-do-indent 3
  "Extra indentation applied to DO blocks."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent)

(defcustom chf-if-indent 3
  "Extra indentation applied to IF, SELECT CASE and WHERE blocks."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent)

(defcustom chf-structure-indent 3
  "Extra indentation applied to STRUCTURE, UNION, MAP and INTERFACE blocks."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent)

(defcustom chf-continuation-indent 5
  "Extra indentation applied to continuation lines."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent)

(defcustom chf-comment-indent-style 'fixed
  "How to indent comments.
nil forces comment lines not to be touched;
`fixed' indents to `chf-comment-line-extra-indent' columns beyond
  `chf-minimum-statement-indent-fixed' (if `indent-tabs-mode' nil), or
  `chf-minimum-statement-indent-tab' (if `indent-tabs-mode' non-nil);
`relative' indents to current ChF indentation plus
  `chf-comment-line-extra-indent'."
  :type  '(radio (const :tag "Untouched" nil) (const fixed) (const relative))
  :safe  (lambda (value) (memq value '(nil fixed relative)))
  :group 'chf-indent)

(defcustom chf-comment-line-extra-indent 0
  "Amount of extra indentation for text within full-line comments."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent
  :group 'chf-comment)

(defcustom chf-comment-line-start "C"
  "Delimiter inserted to start new full-line comment.
You might want to change this to \"*\", for instance; or \"!\" to
allow trailing comments on a line."
  :version "21.1"
  :type    'string
  :safe    'stringp
  :group   'chf-comment)

;; This used to match preprocessor lines too, but that messes up
;; filling and doesn't seem to be necessary.
(defcustom chf-comment-line-start-skip
  "^[CcDd*!]\\(\\([^ \t\n]\\)\\2+\\)?[ \t]*"
  "Regexp to match the start of a full-line comment."
  :version "21.1"
  :type    'regexp
  :safe    'stringp
  :group   'chf-comment)

(defcustom chf-directive-re
  "^[ \t]*#.*"
  "Regexp to match a directive line.
The matching text will be fontified with `font-lock-preprocessor-face'.
The matching line will be given zero indentation."
  :version "22.1"
  :type    'regexp
  :safe    'stringp
  :group   'chf-indent)

(defcustom chf-minimum-statement-indent-fixed 6
  "Minimum statement indentation for fixed format continuation style."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent)

(defcustom chf-minimum-statement-indent-tab (max tab-width 6)
  "Minimum statement indentation for TAB format continuation style."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent)

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defcustom chf-comment-indent-char " "
  "Single-character string inserted for ChF comment indentation.
Normally a space."
  :type  'string
  :safe  (lambda (value) (or (characterp value)
                             (and (stringp value) (= (length value) 1))))
  :group 'chf-comment)

(defcustom chf-line-number-indent 1
  "Maximum indentation for ChF line numbers.
5 means right-justify them within their five-column field."
  :type  'integer
  :safe  'integerp
  :group 'chf-indent)

(defcustom chf-check-all-num-for-matching-do nil
  "Non-nil causes all numbered lines to be treated as possible DO loop ends."
  :type  'boolean
  :safe  'booleanp
  :group 'chf)

(defcustom chf-blink-matching-if nil
  "Non-nil causes \\[chf-indent-line] on ENDIF to blink on matching IF.
Also, from an ENDDO statement blink on matching DO [WHILE] statement."
  :type  'boolean
  :safe  'booleanp
  :group 'chf)

(defcustom chf-continuation-string "$"
  "Single-character string used for ChF continuation lines.
In fixed format continuation style, this character is inserted in
column 6 by \\[chf-split-line] to begin a continuation line.
Also, if \\[chf-indent-line] finds this at the beginning of a
line, it will convert the line into a continuation line of the
appropriate style.  Normally \"$\"."
  :type  'string
  :safe  (lambda (value) (and (stringp value) (= (length value) 1)))
  :group 'chf)

(defcustom chf-comment-region "c$$$"
  "String inserted by \\[chf-comment-region] at start of each \
line in region."
  :type  'string
  :safe  'stringp
  :group 'chf-comment)

(defcustom chf-electric-line-number t
  "Non-nil causes line numbers to be moved to the correct column as typed."
  :type  'boolean
  :safe  'booleanp
  :group 'chf)

;; TODO use chf-line-length, somehow.
(defcustom chf-column-ruler-fixed
  "0   4 6  10        20        30        40        5\
0        60        70\n\
[   ]|{   |    |    |    |    |    |    |    |    \
|    |    |    |    |}\n"
  "String displayed above current line by \\[chf-column-ruler].
This variable is used in fixed format mode.
See the variable `chf-column-ruler-tab' for TAB format mode."
  :type  'string
  :safe  'stringp
  :group 'chf)

;; TODO use chf-line-length, somehow.
(defcustom chf-column-ruler-tab
  "0       810        20        30        40        5\
0        60        70\n\
[   ]|  { |    |    |    |    |    |    |    |    \
|    |    |    |    |}\n"
  "String displayed above current line by \\[chf-column-ruler].
This variable is used in TAB format mode.
See the variable `chf-column-ruler-fixed' for fixed format mode."
  :type  'string
  :safe  'stringp
  :group 'chf)

(defcustom chf-analyze-depth 100
  "Number of lines to scan to identify fixed or TAB format style."
  :type  'integer
  :safe  'integerp
  :group 'chf)

(defcustom chf-break-before-delimiters t
  "Non-nil causes filling to break lines before delimiters.
Delimiters are characters matching the regexp `chf-break-delimiters-re'."
  :type  'boolean
  :safe  'booleanp
  :group 'chf)

;; TODO 0 as no-limit, as per g77.
(defcustom chf-line-length 72
  "Maximum number of characters in a line of fixed-form ChF code.
Characters beyond this point are treated as comments.  Setting
this variable directly (after ChF mode is loaded) does not
take effect.  Use either \\[customize] (which affects all ChF
buffers and the default) or the function
`chf-line-length' (which can also operate on just the current
buffer).  This corresponds to the g77 compiler option
`-ffixed-line-length-N'."
  :type 'integer
  :safe 'integerp
  :initialize 'custom-initialize-default
  :set (lambda (_symbol value)
         ;; Do all chf buffers, and the default.
         (chf-line-length value t))
  :version "23.1"
  :group 'chf)

(make-variable-buffer-local 'chf-line-length)

(defcustom chf-mode-hook nil
  "Hook run when entering ChF mode."
  :type  'hook
  :group 'chf)


(defconst chf-break-delimiters-re "[-+*/><=, \t]"
  "Regexp matching delimiter characters at which lines may be broken.
There are certain tokens comprised entirely of characters
matching this regexp that should not be split, and these are
specified by the constant `chf-no-break-re'.")

;; The ">=", etc F77 extensions are supported by g77.
(defconst chf-no-break-re
  (regexp-opt '("**" "//" "=>" ">=" "<=" "==" "/=") 'paren)
  "Regexp specifying where not to break lines when filling.
This regexp matches certain tokens comprised entirely of
characters matching the regexp `chf-break-delimiters-re' that should
not be split by filling.  Each element is assumed to be two
characters long.")

(defconst chf-if-start-re "\\(\\(\\sw\\|\\s_\\)+:[ \t]*\\)?if[ \t]*("
  "Regexp matching the start of an IF statement.")

;; Note chf-current-defun uses the subgroups.
(defconst chf-start-prog-re
  "^[ \t]*\\(program\\|subroutine\\|function\
\\|[ \ta-z0-9*()]*[ \t]+function\\|\
\\(block[ \t]*data\\)\\)"
  "Regexp matching the start of a subprogram, from the line start.")

(defconst chf-end-prog-re1
  "end\
\\([ \t]*\\(program\\|subroutine\\|function\\|block[ \t]*data\\)\\>\
\\([ \t]*\\(\\sw\\|\\s_\\)+\\)?\\)?"
  "Regexp possibly matching the end of a subprogram.")

(defconst chf-end-prog-re
  (concat "^[ \t0-9]*" chf-end-prog-re1)
  "Regexp possibly matching the end of a subprogram, from the line start.
See also `chf-end-prog-re1'.")

(defconst chf-type-types
  (concat "\\<"
          (mapconcat 'identity          ; " " -> "[ \t]*"
                     (split-string
                      (regexp-opt
                       (let ((simple-types
                              '("character" "byte" "integer" "logical"
                                "none" "real" "complex"
                                "double precision" "double complex"))
                             (structured-types '("structure" "union" "map"))
                             (other-types '("record" "dimension"
                                            "parameter" "common" "save"
                                            "external" "intrinsic" "data"
                                            "equivalence")))
                         (append
                          (mapcar (lambda (x) (concat "implicit " x))
                                  simple-types)
                          simple-types
                          (mapcar (lambda (x) (concat "end " x))
                                  structured-types)
                          structured-types
                          other-types)) 'paren))
                     "[ \t]*") "\\>")
  "Regexp matching ChF types.")

(defvar chf-font-lock-keywords-1
  ;; Program, subroutine and function declarations, plus calls.
  '(("\\<\\(block[ \t]*data\\|call\\|entry\\|function\\|\
program\\|subroutine\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t)))
  "Subdued level highlighting for ChF mode.")

(defvar chf-font-lock-keywords-2
  (append chf-font-lock-keywords-1
          (list
           ;; Fontify all type specifiers (must be first - see below).
           (cons chf-type-types 'font-lock-type-face)
           ;; Builtin keywords (except logical, do and goto - see below).
           (concat "\\<" (regexp-opt
                          '("CHF_AUTOMULTIDO" "CHF_ENDDO" "continue" "format" "end" "enddo"
                            "if" "then" "else" "endif" "elseif"
                            "while" "inquire" "stop" "return"
                            "include" "open" "close" "read"
                            "write" "format" "print" "select" "case"
                            "cycle" "exit" "rewind" "backspace"
                            "where" "elsewhere")
                          'paren) "\\>")
           ;; Builtin operators.
           (concat "\\." (regexp-opt
                          '("and" "eq" "eqv" "false" "ge" "gt" "le" "lt" "ne"
                            "neqv" "not" "or" "true")
                          'paren) "\\.")
           ;; do/goto keywords and targets, and goto tags.
           '("\\<\\(do\\|go *to\\)\\>[ \t]*\\([0-9]+\\)?"
             (1 font-lock-keyword-face)
             (2 font-lock-constant-face nil t))
           '("^ *\\([0-9]+\\)" . font-lock-constant-face)))
  "Medium level highlighting for ChF mode.")

;; See bug#1385. Never really looked into _why_ this matters...
(defun chf-match-and-skip-declaration (limit)
  "Like `font-lock-match-c-style-declaration-item-and-skip-to-next'.
The only difference is, it returns t in a case when the default returns nil."
  (when (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?")
    (when (and (match-end 2) (> (- (match-end 2) (match-beginning 2)) 1))
      (let ((pos (point)))
	(skip-chars-backward " \t\n")
	(skip-syntax-backward "w")
	(unless (looking-at "\\(\\sw+\\)[ \t\n]*\\sw+[ \t\n]*\\(((?\\)?")
	  (goto-char pos)
	  (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?"))))
    (save-match-data
      (condition-case nil
	  (save-restriction
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 1))
	    (while (not (looking-at "[ \t\n]*\\(\\(,\\)\\|;\\|\\'\\)"))
	      (goto-char (or (scan-sexps (point) 1) (point-max))))
            (goto-char (match-end 2)))
	(error t)))))

(defvar chf-font-lock-keywords-3
  (append
   chf-font-lock-keywords-1
   ;; All type specifiers plus their declared items.
   (list
    (list (concat chf-type-types "[ \t(/]*\\(*\\)?")
          ;; Type specifier.
          '(1 font-lock-type-face)
          ;; Declaration item (or just /.../ block name).
          `(chf-match-and-skip-declaration
            ;; Start after any *(...) expression.
            (condition-case nil
                (and (match-beginning ,(1+ (regexp-opt-depth
                                            chf-type-types)))
                     (forward-sexp)
                     (forward-sexp))
              (error nil))
            ;; No need to clean up.
            nil
            ;; Fontify as a variable name, functions fontified elsewhere.
            (1 font-lock-variable-name-face nil t))))
   ;; Things extra to `chf-font-lock-keywords-3' (must be done first).
   (list
    ;; Goto-like `err=label'/`end=label' in read/write statements.
    '(", *\\(e\\(nd\\|rr\\)\\)\\> *\\(= *\\([0-9]+\\)\\)?"
      (1 font-lock-keyword-face) (4 font-lock-constant-face nil t))
    ;; Standard continuation character and in a TAB-formatted line.
    '("^ \\{5\\}\\([^ 0\n]\\)" 1 font-lock-string-face)
    '("^\t\\([1-9]\\)"         1 font-lock-string-face))
   `((,chf-directive-re (0 font-lock-preprocessor-face t)))
   ;; `chf-font-lock-keywords-2' without types (see above).
   (cdr (nthcdr (length chf-font-lock-keywords-1)
                chf-font-lock-keywords-2)))
  "Gaudy level highlighting for ChF mode.")

(defvar chf-font-lock-keywords-4
  (append chf-font-lock-keywords-3
          (list (list
                 (concat "\\<"
                         (regexp-opt
                          '("int" "ifix" "idint" "real" "float" "sngl"
                            "dble" "cmplx" "ichar" "char" "aint" "dint"
                            "anint" "dnint" "nint" "idnint" "iabs" "abs"
                            "dabs" "cabs" "mod" "amod" "dmod" "isign"
                            "sign" "dsign" "idim" "dim" "ddim" "dprod"
                            "max" "max0" "amax1" "dmax1" "amax0" "max1"
                            "min" "min0" "amin1" "dmin1" "amin0" "min1"
                            "len" "index" "lge" "lgt" "lle" "llt" "aimag"
                            "conjg" "sqrt" "dsqrt" "csqrt" "exp" "dexp"
                            "cexp" "log" "alog" "dlog" "clog" "log10"
                            "alog10" "dlog10" "sin" "dsin" "csin" "cos"
                            "dcos" "ccos" "tan" "dtan" "asin" "dasin"
                            "acos" "dacos" "atan" "datan" "atan2" "datan2"
                            "sinh" "dsinh" "cosh" "dcosh" "tanh" "dtanh")
                          'paren) "[ \t]*(") '(1 font-lock-builtin-face))))
  "Maximum highlighting for ChF mode.
Consists of level 3 plus all other intrinsics not already highlighted.")

;; Comments are real pain in ChF because there is no way to
;; represent the standard comment syntax in an Emacs syntax table.
;; (We can do so for F90-style).  Therefore an unmatched quote in a
;; standard comment will throw fontification off on the wrong track.
;; So we do syntactic fontification with regexps.
(defun chf-make-syntax-propertize-function (line-length)
  "Return a value for `syntax-propertize-function' in ChF mode.
This varies according to the value of LINE-LENGTH.
This is used to fontify fixed-format ChF comments."
  ;; This results in a non-byte-compiled function.  We could pass it through
  ;; `byte-compile', but simple benchmarks indicate that it's probably not
  ;; worth the trouble (about 0.5% of slow down).
  (eval                         ;I hate `eval', but it's hard to avoid it here.
   `(syntax-propertize-rules
     ("^[CcDd\\*]" (0 "<"))
     ;; We mark all chars after line-length as "comment-start", rather than
     ;; just the first one.  This is so that a closing ' that's past the
     ;; line-length will indeed be ignored (and will result in a string that
     ;; leaks into subsequent lines).
     ((format "^[^CcDd\\*\t\n].\\{%d\\}\\(.+\\)" (1- line-length))
      (1 "<")))))

(defvar chf-font-lock-keywords chf-font-lock-keywords-1
  "Default expressions to highlight in ChF mode.")

(defvar chf-imenu-generic-expression
  ;; These patterns could be confused by sequence nos. in cols 72+ and
  ;; don't allow continuations everywhere.
  (list
   (list
    nil
    ;; [This will be fooled by `end function' allowed by G77.  Also,
    ;; it assumes sensible whitespace is employed.]
    (concat
     ;; leading whitespace:
     "^\\s-+\\("
     ;; function declaration with optional type, e.g. `real',
     ;; `real*4', character(*), `double precision':
     "\\(\\sw\\|\\s-\\|[*()+]\\)*"
     "\\<function\\|subroutine\\|entry\\|block\\s-*data\\|program\\)"
     ;; Possible statement continuation:
     "[ \t" chf-continuation-string "]+"
     ;; Variable to index:
     "\\(\\sw+\\)")
    3)
   ;; Un-named block data.
   '(nil "^\\s-+\\(block\\s-*data\\)\\s-*$" 1))
  "Value for `imenu-generic-expression' in ChF mode.")


;; Hideshow support.
(defconst chf-blocks-re
  (concat "block[ \t]*data\\|select[ \t]*case\\|"
          (regexp-opt '("do" "if" "interface" "function" "map" "program"
                        "structure" "subroutine" "union" "where")))
  "Regexp potentially indicating the start or end of a ChF \"block\".
Omits naked END statements, and DO-loops closed by anything other
than ENDDO.")

(defconst chf-end-block-re
  ;; Do-loops terminated by things other than ENDDO cannot be handled
  ;; with a regexp. This omission does not seem to matter to hideshow...
  (concat "^[ \t0-9]*\\<end[ \t]*\\("
          chf-blocks-re
          ;; Naked END statement.
          "\\|!\\|$\\)")
  "Regexp matching the end of a ChF \"block\", from the line start.
Note that only ENDDO is handled for the end of a DO-loop.  Used
in the ChF entry in `hs-special-modes-alist'.")

(defconst chf-start-block-re
  (concat
   "^[ \t0-9]*\\("                      ; statement number
   ;; Structure label for DO, IF, SELECT, WHERE.
   "\\(\\(\\sw+[ \t]*:[ \t]*\\)?"
   ;; IF blocks are a nuisance:
   ;; IF ( ... ) foo   is not a block, but a single statement.
   ;; IF ( ... ) THEN  can be split over multiple lines.
   ;; [So can, eg, a DO WHILE (... ), but that is less common, I hope.]
   ;; The regexp below allows for it to be split over at most 2 lines.
   ;; That leads to the problem of not matching two consecutive IF
   ;; statements as one, eg:
   ;; IF ( ... ) foo
   ;; IF ( ... ) THEN
   ;; It simply is not possible to do this in a 100% correct fashion
   ;; using a regexp - see the functions chf-end-if,
   ;; chf-beginning-if for the hoops we have to go through.
   ;; An alternative is to match on THEN at a line end, eg:
   ;;   ".*)[ \t]*then[ \t]*\\($\\|!\\)"
   ;; This would also match ELSE branches, though. This does not seem
   ;; right to me, because then one has neighboring blocks that are
   ;; not nested in each other.
   "\\(if[ \t]*(\\(.*\\|"
   ".*\n\\([^if]*\\([^i].\\|.[^f]\\|.\\>\\)\\)\\)\\<then\\|"
   "do\\|select[ \t]*case\\|where\\)\\)\\|"
   (regexp-opt '("interface" "function" "map" "program"
                 "structure" "subroutine" "union"))
   "\\|block[ \t]*data\\)[ \t]*")
  "Regexp matching the start of a ChF \"block\", from the line start.
A simple regexp cannot do this in fully correct fashion, so this
tries to strike a compromise between complexity and flexibility.
Used in the ChF entry in `hs-special-modes-alist'.")

(add-to-list 'hs-special-modes-alist
             `(chf-mode ,chf-start-block-re ,chf-end-block-re
                            "^[cC*!]" chf-end-of-block nil))


(defvar chf-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Was a word-constituent (for abbrevs), now punctuation (g77
    ;; multi-statement lines).
    (modify-syntax-entry ?\; "."  table)
    (modify-syntax-entry ?\r " "  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?%  "."  table) ; bug#8820
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; Consistent with GNU ChF's default -- see the manual.
    ;; The F77 standard imposes no rule on this issue.
    (modify-syntax-entry ?\\ "\\" table)
    ;; This might be better as punctuation, as for C, but this way you
    ;; can treat floating-point numbers as symbols.
    (modify-syntax-entry ?.  "_"  table) ; e.g. `a.ne.b'
    (modify-syntax-entry ?_  "_"  table)
    (modify-syntax-entry ?$  "_"  table) ; esp. VMSisms
    (modify-syntax-entry ?\! "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    table)
  "Syntax table used in ChF mode.")

(defvar chf-gud-syntax-table
  (let ((st (make-syntax-table chf-mode-syntax-table)))
    (modify-syntax-entry ?\n "." st)
    st)
  "Syntax table used to parse ChF expressions for printing in GUD.")

(defvar chf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";"        'chf-abbrev-start)
    (define-key map "\C-c;"    'chf-comment-region)
    ;; The default comment-dwim does at least as much as this.
;;;    (define-key map "\M-;"     'chf-indent-comment)
    (define-key map "\M-\n"    'chf-split-line)
    (define-key map "\M-\C-n"  'chf-end-of-block)
    (define-key map "\M-\C-p"  'chf-beginning-of-block)
    (define-key map "\M-\C-q"  'chf-indent-subprogram)
    (define-key map "\C-c\C-w" 'chf-window-create-momentarily)
    (define-key map "\C-c\C-r" 'chf-column-ruler)
    (define-key map "\C-c\C-p" 'chf-previous-statement)
    (define-key map "\C-c\C-n" 'chf-next-statement)
    (define-key map "\C-c\C-d" 'chf-join-line) ; like f90
    (define-key map "\M-^"     'chf-join-line) ; subvert delete-indentation
    (define-key map "0" 'chf-electric-line-number)
    (define-key map "1" 'chf-electric-line-number)
    (define-key map "2" 'chf-electric-line-number)
    (define-key map "3" 'chf-electric-line-number)
    (define-key map "4" 'chf-electric-line-number)
    (define-key map "5" 'chf-electric-line-number)
    (define-key map "6" 'chf-electric-line-number)
    (define-key map "7" 'chf-electric-line-number)
    (define-key map "8" 'chf-electric-line-number)
    (define-key map "9" 'chf-electric-line-number)

    (easy-menu-define chf-menu map "Menu for ChF mode."
      `("ChF"
        ["Manual" (info "(emacs)ChF") :active t
         :help "Read the Emacs manual chapter on ChF mode"]
        ("Customization"
         ,(custom-menu-create 'chf)
         ;; FIXME useless?
         ["Set"  Custom-set :active t
          :help "Set current value of all edited settings in the buffer"]
         ["Save" Custom-save :active t
          :help "Set and save all edited settings"]
         ["Reset to Current" Custom-reset-current :active t
          :help "Reset all edited settings to current"]
         ["Reset to Saved" Custom-reset-saved :active t
          :help "Reset all edited or set settings to saved"]
         ["Reset to Standard Settings" Custom-reset-standard :active t
          :help "Erase all customizations in buffer"]
         )
        "--"
        ["Comment Region" chf-comment-region mark-active]
        ["Uncomment Region"
         (chf-comment-region (region-beginning) (region-end) 1)
         mark-active]
        ["Indent Region"     indent-region mark-active]
        ["Indent Subprogram" chf-indent-subprogram t]
        "--"
        ["Beginning of Subprogram" chf-beginning-of-subprogram :active t
         :help "Move point to the start of the current subprogram"]
        ["End of Subprogram" chf-end-of-subprogram :active t
         :help "Move point to the end of the current subprogram"]
        ("Mark"
         :help "Mark a region of code"
         ["Subprogram" mark-defun      t]
         ["IF Block"   chf-mark-if t]
         ["DO Block"   chf-mark-do t]
         )
        ["Narrow to Subprogram" narrow-to-defun t]
        ["Widen" widen t]
        "--"
        ["Temporary Column Ruler" chf-column-ruler :active t
         :help "Briefly display ChF column numbers"]
        ;; May not be '72', depending on chf-line-length, but this
        ;; seems ok for a menu item.
        ["72-column Window" chf-window-create :active t
         :help "Set window width to ChF line length"]
        ["Full Width Window"
         (enlarge-window-horizontally (- (frame-width) (window-width)))
         :active (not (window-full-width-p))
         :help "Make window full width"]
        ["Momentary 72-Column Window" chf-window-create-momentarily
         :active t :help "Briefly set window width to ChF line length"]
        "--"
        ["Break Line at Point" chf-split-line :active t
         :help "Break the current line at point"]
        ["Join Line" chf-join-line :active t
         :help "Join the current line to the previous one"]
        ["Fill Statement/Comment" fill-paragraph t]
        "--"
        ["Toggle Auto Fill" auto-fill-mode :selected auto-fill-function
         :style toggle
         :help "Automatically fill text while typing in this buffer"]
        ["Toggle Abbrev Mode" abbrev-mode :selected abbrev-mode
         :style toggle :help "Expand abbreviations while typing in this buffer"]
        ["Add Imenu Menu" imenu-add-menubar-index
         :active   (not (lookup-key (current-local-map) [menu-bar index]))
         :included (fboundp 'imenu-add-to-menubar)
         :help "Add an index menu to the menu-bar"]))
    map)
  "Keymap used in ChF mode.")


(define-abbrev-table 'chf-mode-abbrev-table
  (mapcar (lambda (e) (list (car e) (cdr e) nil :system t))
          '((";au"   . "automatic"         )
            (";b"    . "byte"              )
            (";bd"   . "block data"        )
            (";ch"   . "character"         )
            (";cl"   . "close"             )
            (";c"    . "continue"          )
            (";cm"   . "common"            )
            (";cx"   . "complex"           )
            (";df"   . "define"            )
            (";di"   . "dimension"         )
            (";do"   . "double"            )
            (";dc"   . "double complex"    )
            (";dp"   . "double precision"  )
            (";dw"   . "do while"          )
            (";e"    . "else"              )
            (";ed"   . "enddo"             )
            (";el"   . "elseif"            )
            (";en"   . "endif"             )
            (";eq"   . "equivalence"       )
            (";ew"   . "endwhere"          )
            (";ex"   . "external"          )
            (";ey"   . "entry"             )
            (";f"    . "format"            )
            (";fa"   . ".false."           )
            (";fu"   . "function"          )
            (";g"    . "goto"              )
            (";im"   . "implicit"          )
            (";ib"   . "implicit byte"     )
            (";ic"   . "implicit complex"  )
            (";ich"  . "implicit character")
            (";ii"   . "implicit integer"  )
            (";il"   . "implicit logical"  )
            (";ir"   . "implicit real"     )
            (";inc"  . "include"           )
            (";in"   . "integer"           )
            (";intr" . "intrinsic"         )
            (";l"    . "logical"           )
            (";n"    . "namelist"          )
            (";o"    . "open"              ) ; was ;op
            (";pa"   . "parameter"         )
            (";pr"   . "program"           )
            (";ps"   . "pause"             )
            (";p"    . "print"             )
            (";rc"   . "record"            )
            (";re"   . "real"              )
            (";r"    . "read"              )
            (";rt"   . "return"            )
            (";rw"   . "rewind"            )
            (";s"    . "stop"              )
            (";sa"   . "save"              )
            (";st"   . "structure"         )
            (";sc"   . "static"            )
            (";su"   . "subroutine"        )
            (";tr"   . ".true."            )
            (";ty"   . "type"              )
            (";vo"   . "volatile"          )
            (";w"    . "write"             )
            (";wh"   . "where"             )))
  "Abbrev table for ChF mode."
  ;; Accept ; as the first char of an abbrev.  Also allow _ in abbrevs.
  :regexp "\\(?:[^[:word:]_;]\\|^\\)\\(;?[[:word:]_]+\\)[^[:word:]_]*")


;;;###autoload
(define-derived-mode chf-mode prog-mode "ChF"
  "Major mode for editing ChF code in fixed format.
For free format code, use `f90-mode'.

\\[chf-indent-line] indents the current ChF line correctly.
Note that DO statements must not share a common CONTINUE.

Type ;? or ;\\[help-command] to display a list of built-in abbrevs for\
 ChF keywords.

Key definitions:
\\{chf-mode-map}

Variables controlling indentation style and extra features:

`chf-comment-line-start'
  To use comments starting with `!', set this to the string \"!\".
`chf-do-indent'
  Extra indentation within DO blocks (default 3).
`chf-if-indent'
  Extra indentation within IF blocks (default 3).
`chf-structure-indent'
  Extra indentation within STRUCTURE, UNION, MAP and INTERFACE blocks.
  (default 3)
`chf-continuation-indent'
  Extra indentation applied to continuation statements (default 5).
`chf-comment-line-extra-indent'
  Amount of extra indentation for text in full-line comments (default 0).
`chf-comment-indent-style'
  How to indent the text in full-line comments. Allowed values are:
  nil         don't change the indentation
  `fixed'     indent to `chf-comment-line-extra-indent' beyond the
              value of either
                `chf-minimum-statement-indent-fixed' (fixed format) or
                `chf-minimum-statement-indent-tab' (TAB format),
              depending on the continuation format in use.
  `relative'  indent to `chf-comment-line-extra-indent' beyond the
              indentation for a line of code.
  (default `fixed')
`chf-comment-indent-char'
  Single-character string to be inserted instead of space for
  full-line comment indentation (default \" \").
`chf-minimum-statement-indent-fixed'
  Minimum indentation for statements in fixed format mode (default 6).
`chf-minimum-statement-indent-tab'
  Minimum indentation for statements in TAB format mode (default 9).
`chf-line-number-indent'
  Maximum indentation for line numbers (default 1).  A line number will
  get less than this much indentation if necessary to avoid reaching
  column 5.
`chf-check-all-num-for-matching-do'
  Non-nil causes all numbered lines to be treated as possible \"continue\"
  statements (default nil).
`chf-blink-matching-if'
  Non-nil causes \\[chf-indent-line] on an ENDIF (or ENDDO) statement
  to blink on the matching IF (or DO [WHILE]).  (default nil)
`chf-continuation-string'
  Single-character string to be inserted in column 5 of a continuation
  line (default \"$\").
`chf-comment-region'
  String inserted by \\[chf-comment-region] at start of each line in
  the region (default \"c$$$\").
`chf-electric-line-number'
  Non-nil causes line number digits to be moved to the correct column
  as typed (default t).
`chf-break-before-delimiters'
  Non-nil causes lines to be broken before delimiters (default t).

Turning on ChF mode calls the value of the variable `chf-mode-hook'
with no args, if that value is non-nil."
  :group 'chf
  :syntax-table chf-mode-syntax-table
  :abbrev-table chf-mode-abbrev-table
  (set (make-local-variable 'indent-line-function) 'chf-indent-line)
  (set (make-local-variable 'indent-region-function)
       (lambda (start end)
         (let (chf-blink-matching-if ; avoid blinking delay
               indent-region-function)
           (indent-region start end nil))))
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  ;; The syntax tables don't understand the column-0 comment-markers.
  (set (make-local-variable 'comment-use-syntax) nil)
  (set (make-local-variable 'comment-padding) "$$$")
  (set (make-local-variable 'comment-start) chf-comment-line-start)
  (set (make-local-variable 'comment-start-skip)
       ;; We can't reuse `chf-comment-line-start-skip' directly because
       ;; it contains backrefs whereas we need submatch-1 to end at the
       ;; beginning of the comment delimiter.
       ;; (concat "\\(\\)\\(![ \t]*\\|" chf-comment-line-start-skip "\\)")
       "\\(\\)\\(?:^[CcDd*]\\|!\\)\\(?:\\([^ \t\n]\\)\\2+\\)?[ \t]*")
  (set (make-local-variable 'comment-indent-function) 'chf-comment-indent)
  (set (make-local-variable 'comment-region-function) 'chf-comment-region)
  (set (make-local-variable 'uncomment-region-function)
       'chf-uncomment-region)
  (set (make-local-variable 'comment-insert-comment-function)
       'chf-indent-comment)
  (set (make-local-variable 'abbrev-all-caps) t)
  (set (make-local-variable 'normal-auto-fill-function) 'chf-auto-fill)
  (set (make-local-variable 'indent-tabs-mode) (chf-analyze-file-format))
  (setq mode-line-process '(indent-tabs-mode chf-tab-mode-string))
  (set (make-local-variable 'fill-column) chf-line-length)
  (set (make-local-variable 'fill-paragraph-function) 'chf-fill-paragraph)
  (set (make-local-variable 'font-lock-defaults)
       '((chf-font-lock-keywords
          chf-font-lock-keywords-1
          chf-font-lock-keywords-2
          chf-font-lock-keywords-3
          chf-font-lock-keywords-4)
         nil t ((?/ . "$/") ("_$" . "w"))
         chf-beginning-of-subprogram))
  (set (make-local-variable 'syntax-propertize-function)
       (chf-make-syntax-propertize-function chf-line-length))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
       chf-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist) '(("_$" . "w")))
  (set (make-local-variable 'beginning-of-defun-function)
       #'chf-beginning-of-subprogram)
  (set (make-local-variable 'end-of-defun-function)
       #'chf-end-of-subprogram)
  (set (make-local-variable 'add-log-current-defun-function)
       #'chf-current-defun)
  (set (make-local-variable 'dabbrev-case-fold-search) 'case-fold-search)
  (set (make-local-variable 'gud-find-expr-function) 'chf-gud-find-expr)
  (add-hook 'hack-local-variables-hook 'chf-hack-local-variables nil t))


(defun chf-line-length (nchars &optional global)
  "Set the length of fixed-form ChF lines to NCHARS.
By default this only affects the current buffer, which must be in
ChF mode.  If the optional argument GLOBAL is non-nil, it affects
all ChF buffers, and also the default.  The default value of NCHARS
is the current column.  A numeric prefix argument specifies a value to
use instead of the current column.  A non-numeric prefix argument prompts
for the value to use."
  (interactive
   (list (cond
          ((numberp current-prefix-arg) current-prefix-arg)
          (current-prefix-arg
           (read-number "Line length: " (default-value 'chf-line-length)))
          (t (current-column)))))
  (dolist (buff (if global
                    (buffer-list)
                  (list (current-buffer))))
    (with-current-buffer buff
      (when (derived-mode-p 'chf-mode)
        (unless (eq chf-line-length nchars)
          (setq chf-line-length nchars
                fill-column chf-line-length
                syntax-propertize-function
                (chf-make-syntax-propertize-function nchars))
          (syntax-ppss-flush-cache (point-min))
          (if font-lock-mode (font-lock-mode 1))))))
          (if global
      (setq-default chf-line-length nchars)))

(defun chf-hack-local-variables ()
  "ChF mode adds this to `hack-local-variables-hook'."
  (chf-line-length chf-line-length))

(declare-function gud-find-c-expr "gud.el" nil)

(defun chf-gud-find-expr ()
  ;; Consider \n as punctuation (end of expression).
  (with-syntax-table chf-gud-syntax-table
    (gud-find-c-expr)))

(defsubst chf-comment-indent ()
  "Return the indentation appropriate for the current comment line.
This is 0 for a line matching `chf-comment-line-start-skip', else
the value of `comment-column' (leaving at least one space after code)."
  (if (looking-at chf-comment-line-start-skip) 0
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column)) comment-column))))

(defun chf-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned,
if the value of `comment-start' is not nil and allows such comments.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive "*")
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((chf-find-comment-start-skip 'all)
         (goto-char (match-beginning 0))
         (if (bolp)
             (chf-indent-line)
           (unless (= (current-column) (chf-comment-indent))
             (delete-horizontal-space)
             (indent-to (chf-comment-indent)))))
        ;; No existing comment.
        ;; If side-by-side comments are defined, insert one,
        ;; unless line is now blank.
        ((and comment-start (not (looking-at "[ \t]*$"))
              (string-match comment-start-skip (concat " " comment-start)))
         (end-of-line)
         (delete-horizontal-space)
         (indent-to (chf-comment-indent))
         (insert comment-start))
        ;; Else insert separate-line comment, making a new line if nec.
        (t
         (if (looking-at "^[ \t]*$")
             (delete-horizontal-space)
           (beginning-of-line)
           (insert ?\n)
           (forward-char -1))
         (insert chf-comment-line-start)
         (insert-char (if (stringp chf-comment-indent-char)
                          (aref chf-comment-indent-char 0)
                        chf-comment-indent-char)
                      (- (chf-calculate-indent) (current-column))))))

(defun chf-comment-region (beg-region end-region arg)
  "Comment every line in the region.
Inserts the string variable `chf-comment-region' at the beginning of
every line in the region.
BEG-REGION and END-REGION specify the region boundaries.
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (copy-marker end-region))
        (save-point (point-marker)))
    (goto-char beg-region)
    (beginning-of-line)
    (if arg
        (let ((com (regexp-quote chf-comment-region))) ; uncomment
          (if (looking-at com)
              (delete-region (point) (match-end 0)))
          (while (and (zerop (forward-line 1))
                      (< (point) end-region-mark))
            (if (looking-at com)
                (delete-region (point) (match-end 0)))))
      (insert chf-comment-region)   ; comment
      (while (and (zerop (forward-line 1))
                  (< (point) end-region-mark))
        (insert chf-comment-region)))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

;; uncomment-region calls this with 3 args.
(defun chf-uncomment-region (start end &optional ignored)
  "Uncomment every line in the region."
  (chf-comment-region start end t))


(defun chf-abbrev-start ()
  "Typing ;\\[help-command] or ;? lists all the ChF abbrevs.
Any other key combination is executed normally."
  (interactive "*")
  (insert last-command-event)
  (let* ((event (if (fboundp 'next-command-event) ; XEmacs
                    (next-command-event)
                  (read-event)))
         (char (if (fboundp 'event-to-character)
                   (event-to-character event) event)))
    ;; Insert char if not equal to `?', or if abbrev-mode is off.
    (if (and abbrev-mode (or (eq char ??) (eq char help-char)
                             (memq event help-event-list)))
        (chf-abbrev-help)
      (push event unread-command-events))))

(defun chf-abbrev-help ()
  "List the currently defined abbrevs in ChF mode."
  (interactive)
  (message "Listing abbrev table...")
  (display-buffer (chf-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun chf-prepare-abbrev-list-buffer ()
  "Create a buffer listing the ChF mode abbreviations."
  (with-current-buffer (get-buffer-create "*Abbrevs*")
    (erase-buffer)
    (insert-abbrev-table-description 'chf-mode-abbrev-table t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun chf-column-ruler ()
  "Insert a column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of `chf-column-ruler-fixed' in fixed
format mode, and `chf-column-ruler-tab' in TAB format mode.
The next key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display
   (if indent-tabs-mode
       chf-column-ruler-tab
     chf-column-ruler-fixed)
   (save-excursion
     (beginning-of-line)
     (if (eq (window-start) (window-point))
         (line-beginning-position 2)
       (point)))
   nil "Type SPC or any command to erase ruler."))

(defun chf-window-create ()
  "Make the window `chf-line-length' (default 72) columns wide.
See also `chf-window-create-momentarily'."
  (interactive)
  (let ((window-min-width 2))
    (unless (window-full-width-p)
        (enlarge-window-horizontally (- (frame-width)
                                        (window-width) 1)))
    (let* ((window-edges (window-edges))
           (scroll-bar-width (- (nth 2 window-edges)
                                (car window-edges)
                                (window-width))))
      (split-window-right (+ chf-line-length scroll-bar-width)))
    (other-window 1)
    (switch-to-buffer " chf-window-extra" t)
    (select-window (previous-window))))

(defun chf-window-create-momentarily (&optional arg)
  "Momentarily make the window `chf-line-length' (default 72) columns wide.
Optional ARG non-nil and non-unity disables the momentary feature.
See also `chf-window-create'."
  (interactive "p")
  (if (or (not arg)
          (= arg 1))
      (save-window-excursion
        (progn
          (condition-case nil
              (chf-window-create)
            (error (error "No room for ChF window")))
          (message "Type SPC to continue editing.")
          (let ((char (read-event)))
            (or (equal char ?\s)
                (push char unread-command-events)))))
    (chf-window-create)))

(defun chf-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive "*")
  (delete-horizontal-space)
  (if (save-excursion
        (let ((pos (point)))
          (beginning-of-line)
          (and (chf-find-comment-start-skip 'all)
               (< (match-beginning 0) pos))))
      (insert ?\n (match-string 0))
    (if indent-tabs-mode
        (insert ?\n ?\t (chf-numerical-continuation-char))
      (insert "\n " chf-continuation-string))) ; space after \n important
  (chf-indent-line))               ; when cont string is C, c or *

(defun chf-remove-continuation ()
  "Delete any ChF continuation characters at point.
Returns t if anything actually deleted."
  (when (looking-at "\\( \\{5\\}[^ 0\n]\\|\t[1-9]\\|&\\)")
    (replace-match "")
    (delete-indentation)
    t))

(defun chf-join-line (arg)
  "Join current line to the previous one and re-indent.
With a prefix argument, repeat this operation that many times.
If the prefix argument ARG is negative, join the next -ARG lines.
Continuation lines are correctly handled."
  (interactive "*p")
  (save-excursion
    (when (> 0 arg)
      (setq arg (- arg))
      (forward-line arg))
    (while (not (zerop arg))
      (beginning-of-line)
      (or (chf-remove-continuation)
          (delete-indentation))
      (setq arg (1- arg)))
    (chf-indent-line)))

(defun chf-numerical-continuation-char ()
  "Return a digit for tab-digit style of continuation lines.
If previous line is a tab-digit continuation line, return that digit
plus one, otherwise return 1.  Zero not allowed."
  (save-excursion
    (forward-line -1)
    (if (looking-at "\t[1-9]")
        (+ ?1 (% (- (char-after (1+ (point))) ?0) 9))
      ?1)))

(put 'chf-electric-line-number 'delete-selection t)
(defun chf-electric-line-number (arg)
  "Self insert, but if part of a ChF line number indent it automatically.
Auto-indent does not happen if a numeric ARG is used."
  (interactive "*P")
  (if (or arg (not chf-electric-line-number))
      (if arg
          (self-insert-command (prefix-numeric-value arg))
        (self-insert-command 1))
    (if (or (and (= 5 (current-column))
                 (save-excursion
                   (beginning-of-line)
                    ;; In col 5 with only spaces to the left.
                   (looking-at " \\{5\\}")))
            (and (= (if indent-tabs-mode
                        chf-minimum-statement-indent-tab
                      chf-minimum-statement-indent-fixed) (current-column))
                 ;; In col 8 with a single tab to the left.
                 (eq ?\t (char-after (line-beginning-position)))
                 (not (or (eq last-command 'chf-indent-line)
                          (eq last-command
                              'chf-indent-new-line))))
            (save-excursion
              (re-search-backward "[^ \t0-9]"
                                  (line-beginning-position)
                                  t))   ; not a line number
            (looking-at "[0-9]"))       ; within a line number
        (self-insert-command (prefix-numeric-value arg))
      (skip-chars-backward " \t")
      (insert last-command-event)
      (chf-indent-line))))


(defun chf-check-end-prog-re ()
  "Check a preliminary match against `chf-end-prog-re'."
  ;; Having got a possible match for the subprogram end, we need a
  ;; match of whitespace, avoiding possible column 73+ stuff.
  (save-match-data
    (string-match "^\\s-*\\(\\'\\|\\s<\\)"
                  (buffer-substring (match-end 0)
                                    (min (line-end-position)
                                         (+ chf-line-length
                                            (line-beginning-position)))))))

;; This is more complex than first expected because the beginning of a
;; main program may be implicit (ie not marked by a PROGRAM statement).
;; This would be fine (we could just go to bob in the absence of a match),
;; except it need not even be the first subprogram in the file (eg it
;; could follow a subroutine).  Hence we have to search for END
;; statements instead.
;; cf chf-beginning-of-block, f90-beginning-of-subprogram
;; Note that unlike the latter, we don't have to worry about nested
;; subprograms (?).
;; FIXME push-mark?
(defun chf-beginning-of-subprogram ()
  "Move point to the beginning of the current ChF subprogram."
  (interactive)
  (let ((case-fold-search t))
    ;; If called already at the start of subprogram, go to the previous.
    (beginning-of-line (if (bolp) 0 1))
    (save-match-data
      (or (looking-at chf-start-prog-re)
          ;; This leaves us at bob if before the first subprogram.
          (eq (chf-previous-statement) 'first-statement)
          (if (or (catch 'ok
                    (while (re-search-backward chf-end-prog-re nil 'move)
                      (if (chf-check-end-prog-re) (throw 'ok t))))
                  ;; If the search failed, must be at bob.
                  ;; First code line is the start of the subprogram.
                  ;; FIXME use a more rigorous test, cf chf-next-statement?
                  ;; Though that needs to handle continuations too.
                  (not (looking-at "^\\([ \t]*[0-9]\\|[ \t]+[^!#]\\)")))
              (chf-next-statement))))))

;; This is simpler than f-beginning-of-s because the end of a
;; subprogram is never implicit.
(defun chf-end-of-subprogram ()
  "Move point to the end of the current ChF subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line)
    (save-match-data
      (while (and (re-search-forward chf-end-prog-re nil 'move)
                  (not (chf-check-end-prog-re))))
      (forward-line))))

(defun chf-previous-statement ()
  "Move point to beginning of the previous ChF statement.
Returns `first-statement' if that statement is the first
non-comment ChF statement in the file, and nil otherwise.
Directive lines are treated as comments."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (setq continue-test
          (and
           (not (looking-at chf-comment-line-start-skip))
           (not (looking-at chf-directive-re))
           (or (looking-at
                (concat "[ \t]*"
                        (regexp-quote chf-continuation-string)))
               (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]"))))
    (while (and (setq not-first-statement (zerop (forward-line -1)))
                (or (looking-at chf-comment-line-start-skip)
                    (looking-at chf-directive-re)
                    (looking-at
                     (concat "[ \t]*"
                             (regexp-quote chf-continuation-string)))
                    (looking-at "[ \t]*$\\| \\{5\\}[^ 0\n]\\|\t[1-9]")
                    (looking-at (concat "[ \t]*" comment-start-skip)))))
    (cond ((and continue-test
                (not not-first-statement))
           (message "Incomplete continuation statement."))
          (continue-test
           (chf-previous-statement))
          ((not not-first-statement)
           'first-statement))))

(defun chf-next-statement ()
  "Move point to beginning of the next ChF statement.
Returns `last-statement' if that statement is the last
non-comment ChF statement in the file, and nil otherwise.
Directive lines are treated as comments."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
                      (and (zerop (forward-line 1))
                           (not (eobp))))
                (or (looking-at chf-comment-line-start-skip)
                    (looking-at chf-directive-re)
                    (looking-at "[ \t]*$\\|     [^ 0\n]\\|\t[1-9]")
                    (looking-at (concat "[ \t]*" comment-start-skip)))))
    (if (not not-last-statement)
        'last-statement)))

(defun chf-looking-at-if-then ()
  "Return non-nil if at the start of a line with an IF ... THEN statement."
  ;; cf f90-looking-at-if-then.
  (let ((p (point))
        (i (chf-beginning-if)))
    (if i
        (save-excursion
          (goto-char i)
          (= (line-beginning-position) p)))))

;; Used in hs-special-modes-alist.
(defun chf-end-of-block (&optional num)
  "Move point forward to the end of the current code block.
With optional argument NUM, go forward that many balanced blocks.
If NUM is negative, go backward to the start of a block.  Does
not check for consistency of block types.  Interactively, pushes
mark before moving point."
  (interactive "p")
  (if (called-interactively-p 'any) (push-mark (point) t))
  (and num (< num 0) (chf-beginning-of-block (- num)))
  (let ((case-fold-search t)
        (count (or num 1)))
    (end-of-line)
    (while (and (> count 0)
                (re-search-forward
                 (concat "\\(" chf-blocks-re
                         (if chf-check-all-num-for-matching-do
                             "\\|^[ \t]*[0-9]+" "")
                         "\\|continue\\|end\\)\\>")
                 nil 'move))
      (beginning-of-line)
      (if (if (looking-at (concat "^[0-9 \t]*" chf-if-start-re))
              (chf-looking-at-if-then)
            (looking-at chf-start-block-re))
          (setq count (1+ count))
        (if (or (looking-at chf-end-block-re)
                (and (or (looking-at "^[0-9 \t]*continue")
                         (and chf-check-all-num-for-matching-do
                              (looking-at "[ \t]*[0-9]+")))
                     (chf-check-for-matching-do)))
            (setq count (1- count))))
      (end-of-line))
    (if (> count 0) (error "Missing block end"))))

(defun chf-beginning-of-block (&optional num)
  "Move point backwards to the start of the current code block.
With optional argument NUM, go backward that many balanced
blocks.  If NUM is negative, go forward to the end of a block.
Does not check for consistency of block types.  Interactively,
pushes mark before moving point."
  (interactive "p")
  (if (called-interactively-p 'any) (push-mark (point) t))
  (and num (< num 0) (chf-end-of-block (- num)))
  (let ((case-fold-search t)
        (count (or num 1)))
    (beginning-of-line)
    (while (and (> count 0)
                (re-search-backward
                 (concat "\\(" chf-blocks-re
                         (if chf-check-all-num-for-matching-do
                             "\\|^[ \t]*[0-9]+" "")
                         "\\|continue\\|end\\)\\>")
                 nil 'move))
      (beginning-of-line)
      (if (if (looking-at (concat "^[0-9 \t]*" chf-if-start-re))
              (chf-looking-at-if-then)
            (looking-at chf-start-block-re))
          (setq count (1- count))
        (if (or (looking-at chf-end-block-re)
                (and (or (looking-at "^[0-9 \t]*continue")
                         (and chf-check-all-num-for-matching-do
                              (looking-at "[ \t]*[0-9]+")))
                     (chf-check-for-matching-do)))
            (setq count (1+ count)))))
    ;; Includes an un-named main program block.
    (if (> count 0) (error "Missing block start"))))


(defun chf-blink-match (regex keyword find-begin)
  "From a line matching REGEX, blink matching KEYWORD statement line.
Use function FIND-BEGIN to match it."
  (let ((top-of-window (window-start))
        (end-point (point))
        (case-fold-search t)
        matching
        message)
    (when (save-excursion
            (beginning-of-line)
            (skip-chars-forward " \t0-9")
            (looking-at regex))
      (if (not (setq matching (funcall find-begin)))
          (setq message (concat "No matching " keyword "."))
        (if (< matching top-of-window)
            (save-excursion
              (goto-char matching)
              (beginning-of-line)
              (setq message
                    (concat "Matches "
                            (buffer-substring (point)
                                              (line-end-position)))))))
      (if message
          (message "%s" message)
        (goto-char matching)
        (sit-for blink-matching-delay)
        (goto-char end-point)))))

(defun chf-blink-matching-if ()
  "From an ENDIF or ELSE statement, blink the matching IF statement."
  (chf-blink-match "e\\(nd[ \t]*if\\|lse\\([ \t]*if\\)?\\)\\b"
                       "if" #'chf-beginning-if))

(defun chf-blink-matching-do ()
  "From an ENDDO statement, blink the matching DO or DO WHILE statement."
  (chf-blink-match "end[ \t]*do\\b" "do" #'chf-beginning-do))

(defun chf-mark-do ()
  "Put mark at end of ChF DO [WHILE]-ENDDO construct, point at beginning.
The marks are pushed."
  (interactive)
  (let (enddo-point do-point)
    (if (setq enddo-point (chf-end-do))
        (if (not (setq do-point (chf-beginning-do)))
            (message "No matching do.")
          (goto-char enddo-point)
          (push-mark)
          (goto-char do-point)))))

(defun chf-end-do ()
  "Search forward for first unmatched ENDDO.
Return point or nil."
  (let ((case-fold-search t))
    (if (save-excursion (beginning-of-line)
                        (skip-chars-forward " \t0-9")
                        (looking-at "end[ \t]*do\\b"))
        ;; Sitting on one.
        (match-beginning 0)
      ;; Search for one.
      (save-excursion
        (let ((count 1))
          (while (and (not (zerop count))
                      (not (eq (chf-next-statement) 'last-statement))
                      ;; Keep local to subprogram.
                      (not (and (looking-at chf-end-prog-re)
                                (chf-check-end-prog-re))))
            (skip-chars-forward " \t0-9")
            (cond ((looking-at "end[ \t]*do\\b")
                   (setq count (1- count)))
                  ((looking-at
                    "\\(\\(\\sw\\|\\s_\\)+:[ \t]*\\)?do[ \t]+[^0-9]")
                   (setq count (1+ count)))))
          (and (zerop count)
               ;; All pairs accounted for.
               (point)))))))

(defun chf-beginning-do ()
  "Search backwards for first unmatched DO [WHILE].
Return point or nil.  Ignores labeled DO loops (ie DO 10 ... 10 CONTINUE)."
  (let ((case-fold-search t)
        (dostart-re "\\(\\(\\sw\\|\\s_\\)+:[ \t]*\\)?do[ \t]+[^0-9]"))
    (if (save-excursion
          (beginning-of-line)
          (skip-chars-forward " \t0-9")
          (looking-at dostart-re))
        ;; Sitting on one.
        (match-beginning 0)
      ;; Search for one.
      (save-excursion
        (let ((count 1))
          (while (and (not (zerop count))
                      (not (eq (chf-previous-statement) 'first-statement))
                      ;; Keep local to subprogram.
                      (not (and (looking-at chf-end-prog-re)
                                (chf-check-end-prog-re))))
            (skip-chars-forward " \t0-9")
            (cond ((looking-at dostart-re)
                   (setq count (1- count)))
                  ;; Note labeled loop ends not considered.
                  ((looking-at "end[ \t]*do\\b")
                   (setq count (1+ count)))))
          (and (zerop count)
               ;; All pairs accounted for.
               (point)))))))

(defun chf-mark-if ()
  "Put mark at end of ChF IF-ENDIF construct, point at beginning.
The marks are pushed."
  (interactive)
  (let (endif-point if-point)
    (if (setq endif-point (chf-end-if))
        (if (not (setq if-point (chf-beginning-if)))
            (message "No matching if.")
          ;; Set mark, move point.
          (goto-char endif-point)
          (push-mark)
          (goto-char if-point)))))

(defun chf-end-if ()
  "Search forwards for first unmatched ENDIF.
Return point or nil."
  (let ((case-fold-search t))
    (if (save-excursion (beginning-of-line)
                        (skip-chars-forward " \t0-9")
                        (looking-at "end[ \t]*if\\b"))
        ;; Sitting on one.
        (match-beginning 0)
      ;; Search for one.  The point has been already been moved to first
      ;; letter on line but this should not cause troubles.
      (save-excursion
        (let ((count 1))
          (while (and (not (zerop count))
                      (not (eq (chf-next-statement) 'last-statement))
                      ;; Keep local to subprogram.
                      (not (and (looking-at chf-end-prog-re)
                                (chf-check-end-prog-re))))
            (skip-chars-forward " \t0-9")
            (cond ((looking-at "end[ \t]*if\\b")
                   (setq count (1- count)))
                  ((looking-at chf-if-start-re)
                   (save-excursion
                     (if (or
                          (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
                          (let (then-test) ; multi-line if-then
                            (while
                                (and
                                 (zerop (forward-line 1))
                                 ;; Search forward for then.
                                 (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]")
                                 (not
                                  (setq then-test
                                        (looking-at
                                         ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
                            then-test))
                         (setq count (1+ count)))))))
          (and (zerop count)
               ;; All pairs accounted for.
               (point)))))))

(defun chf-beginning-if ()
  "Search backwards for first unmatched IF-THEN.
Return point or nil."
  (let ((case-fold-search t))
    (if (save-excursion
          ;; May be sitting on multi-line if-then statement, first
          ;; move to beginning of current statement.  Note:
          ;; `chf-previous-statement' moves to previous statement
          ;; *unless* current statement is first one.  Only move
          ;; forward if not first-statement.
          (if (not (eq (chf-previous-statement) 'first-statement))
              (chf-next-statement))
          (skip-chars-forward " \t0-9")
          (and
           (looking-at chf-if-start-re)
           (save-match-data
             (or (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
                 ;; Multi-line if-then.
                 (let (then-test)
                   (while
                       (and (zerop (forward-line 1))
                            ;; Search forward for then.
                            (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]")
                            (not
                             (setq then-test
                                   (looking-at
                                    ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
                   then-test)))))
        ;; Sitting on one.
        (match-beginning 0)
      ;; Search for one.
      (save-excursion
        (let ((count 1))
          (while (and (not (zerop count))
                      (not (eq (chf-previous-statement) 'first-statement))
                      ;; Keep local to subprogram.
                      (not (and (looking-at chf-end-prog-re)
                                (chf-check-end-prog-re))))
            (skip-chars-forward " \t0-9")
            (cond ((looking-at chf-if-start-re)
                   (save-excursion
                     (if (or
                          (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
                          (let (then-test) ; multi-line if-then
                            (while
                                (and
                                 (zerop (forward-line 1))
                                 ;; Search forward for then.
                                 (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]")
                                 (not
                                  (setq then-test
                                        (looking-at
                                         (concat ".*then\\b[ \t]*"
                                                 "[^ \t(=a-z0-9]"))))))
                            then-test))
                         (setq count (1- count)))))
                  ((looking-at "end[ \t]*if\\b")
                   (setq count (1+ count)))))
          (and (zerop count)
               ;; All pairs accounted for.
               (point)))))))


(defun chf-indent-line ()
  "Indent current ChF line based on its contents and on previous lines."
  (interactive "*")
  (let ((cfi (chf-calculate-indent)))
    (save-excursion
      (beginning-of-line)
      (if (or (not (= cfi (chf-current-line-indentation)))
              (and (re-search-forward "^[ \t]*[0-9]+" (+ (point) 4) t)
                   (not (chf-line-number-indented-correctly-p))))
          (chf-indent-to-column cfi)
        (beginning-of-line)
        (if (chf-find-comment-start-skip)
            (chf-indent-comment))))
    ;; Never leave point in left margin.
    (if (< (current-column) cfi)
        (move-to-column cfi))
    (and auto-fill-function
         (> (save-excursion (end-of-line) (current-column))
            fill-column)
         (save-excursion
           (end-of-line)
           (chf-fill)))
    (when chf-blink-matching-if
      (chf-blink-matching-if)
      (chf-blink-matching-do))))

(defun chf-auto-fill ()
  "Function to use for `normal-auto-fill-function' in ChF mode."
  (if (> (current-column) (current-fill-column))
      (let ((cfi (chf-calculate-indent)))
        (save-excursion
          (beginning-of-line)
          (if (or (not (= cfi (chf-current-line-indentation)))
                  (and (re-search-forward "^[ \t]*[0-9]+"
                                          (+ (point) 4) t)
                       (not (chf-line-number-indented-correctly-p))))
              (chf-indent-to-column cfi)
            (beginning-of-line)
            (if (chf-find-comment-start-skip)
                (chf-indent-comment))))
        (chf-fill)
        ;; Never leave point in left margin.
        (if (< (current-column) cfi)
            (move-to-column cfi)))))

;; Historically this was a separate function which advertised itself
;; as reindenting but only did so where `most likely to be necessary'.
(defalias 'chf-indent-new-line 'reindent-then-newline-and-indent)

(defun chf-indent-subprogram ()
  "Properly indent the ChF subprogram containing point."
  (interactive "*")
  (save-excursion
    (mark-defun)
    (message "Indenting subprogram...")
    (indent-region (point) (mark) nil))
  (message "Indenting subprogram...done."))

(defun chf-calculate-indent ()
  "Calculates the ChF indent column based on previous lines."
  (let (icol first-statement (case-fold-search t)
             (chf-minimum-statement-indent
              (if indent-tabs-mode
                  chf-minimum-statement-indent-tab
                chf-minimum-statement-indent-fixed)))
    (save-excursion
      (setq first-statement (chf-previous-statement))
      (if first-statement
          (setq icol chf-minimum-statement-indent)
        (if (= (point) (point-min))
            (setq icol chf-minimum-statement-indent)
          (setq icol (chf-current-line-indentation)))
        (skip-chars-forward " \t0-9")
        (cond ((looking-at "\\(\\(\\sw\\|\\s_\\)+:[ \t]*\\)?if[ \t]*(")
               (if (or (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t_$(=a-z0-9]")
                       (let (then-test) ; multi-line if-then
                         (while (and (zerop (forward-line 1))
                                     ;; Search forward for then.
                                     (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]")
                                     (not (setq then-test
                                                (looking-at
                                                 ".*then\\b[ \t]\
*[^ \t_$(=a-z0-9]")))))
                         then-test))
                   (setq icol (+ icol chf-if-indent))))
              ((looking-at "else\\(if\\)?\\b")
               (setq icol (+ icol chf-if-indent)))
              ((looking-at "select[ \t]*case[ \t](.*)")
               (setq icol (+ icol chf-if-indent)))
              ((looking-at "case[ \t]*(.*)")
               (setq icol (+ icol chf-if-indent)))
              ((looking-at "case[ \t]*default\\b")
               (setq icol (+ icol chf-if-indent)))
              ((looking-at "\\(otherwise\\|else[ \t]*where\\)\\b")
               (setq icol (+ icol chf-if-indent)))
              ((looking-at "where[ \t]*(.*)[ \t]*\n")
               (setq icol (+ icol chf-if-indent)))
	      ((looking-at "\\<CHF_AUTOMULTIDO\\b")
               (setq icol (+ icol chf-do-indent)))
              ((looking-at "\\<do\\b")
               (setq icol (+ icol chf-do-indent)))
              ((looking-at
                "\\(structure\\|union\\|map\\|interface\\)\
\\b[ \t]*[^ \t=(a-z]")
               (setq icol (+ icol chf-structure-indent)))
              ((and (looking-at chf-end-prog-re1)
                    (chf-check-end-prog-re))
               ;; Previous END resets indent to minimum.
               (setq icol chf-minimum-statement-indent))
              ;; Previous statement was a numbered DO loop without a
              ;; closing CONTINUE or END DO, so we indented the
              ;; terminator like the loop body.
              ((and chf-check-all-num-for-matching-do
                    (not (looking-at "\\(continue\\|\\<CHF_ENDDO\\b\\|end[ \t]*do\\)\\>"))
                    (progn
                      (beginning-of-line)
                      (and (looking-at "[ \t]*[0-9]+")
                           (chf-check-for-matching-do))))
               (setq icol (- icol chf-do-indent))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))
            ;; Check for directive before comment, so as not to indent.
            ((looking-at chf-directive-re)
             (setq chf-minimum-statement-indent 0 icol 0))
            ((looking-at chf-comment-line-start-skip)
             (cond ((eq chf-comment-indent-style 'relative)
                    (setq icol (+ icol chf-comment-line-extra-indent)))
                   ((eq chf-comment-indent-style 'fixed)
                    (setq icol (+ chf-minimum-statement-indent
                                  chf-comment-line-extra-indent))))
             (setq chf-minimum-statement-indent 0))
            ((or (looking-at (concat "[ \t]*"
                                     (regexp-quote
                                      chf-continuation-string)))
                 (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]"))
             (skip-chars-forward " \t")
             ;; Do not introduce extra whitespace into a broken string.
             (setq icol
                   (if (chf-is-in-string-p (point))
                       6
                     (+ icol chf-continuation-indent))))
            (first-statement)
            ;; The terminating statement is actually part of the
            ;; loop body, so unless it is a CONTINUE or END DO, we
            ;; indent it like the loop body (see above).
            ((and chf-check-all-num-for-matching-do
                  (looking-at "[ \t]*[0-9]+[ \t]*\
\\(continue\\|\\<CHF_ENDDO\\b\\|end[ \t]*do\\)\\>")
                  (chf-check-for-matching-do))
             (setq icol (- icol chf-do-indent)))
            (t
             (skip-chars-forward " \t0-9")
             (cond ((looking-at "end[ \t]*\\(if\\|select\\|where\\)\\b")
                    (setq icol (- icol chf-if-indent)))
                   ((looking-at "else\\(if\\)?\\b")
                    (setq icol (- icol chf-if-indent)))
                   ((looking-at "case[ \t]*\\((.*)\\|default\\>\\)")
                    (setq icol (- icol chf-if-indent)))
                   ((looking-at "\\(otherwise\\|else[ \t]*where\\)\\b")
                    (setq icol (- icol chf-if-indent)))
                   ((and (looking-at "continue\\b")
                         (chf-check-for-matching-do))
                    (setq icol (- icol chf-do-indent)))
		   ((looking-at "\\<CHF_ENDDO\\b")
                    (setq icol (- icol chf-do-indent)))
                   ((looking-at "end[ \t]*do\\b")
                    (setq icol (- icol chf-do-indent)))
                   ((looking-at "end[ \t]*\
\\(structure\\|union\\|map\\|interface\\)\\b[ \t]*[^ \t=(a-z]")
                    (setq icol (- icol chf-structure-indent)))
                   ((and (looking-at chf-end-prog-re1)
                         (chf-check-end-prog-re)
                         (not (= icol chf-minimum-statement-indent)))
                    (message "Warning: `end' not in column %d.  Probably\
 an unclosed block." chf-minimum-statement-indent))))))
    (max chf-minimum-statement-indent icol)))


(defun chf-current-line-indentation ()
  "Indentation of current line, ignoring ChF line number or continuation.
This is the column position of the first non-whitespace character
aside from the line number and/or column 5/8 line-continuation character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at chf-comment-line-start-skip)
           (goto-char (match-end 0))
           (skip-chars-forward
            (if (stringp chf-comment-indent-char)
                chf-comment-indent-char
              (char-to-string chf-comment-indent-char))))
          ((or (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]"))
           (goto-char (match-end 0)))
          (t
           ;; Move past line number.
           (skip-chars-forward "[ \t0-9]")))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun chf-indent-to-column (col)
  "Indent current line to column COL.
notes: 1) A non-zero/non-blank character in column 5 indicates a continuation
          line, and this continuation character is retained on indentation;
       2) If `chf-continuation-string' is the first non-whitespace
          character, this is a continuation line;
       3) A non-continuation line which has a number as the first
          non-whitespace character is a numbered line.
       4) A TAB followed by a digit indicates a continuation line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at chf-comment-line-start-skip)
        (if chf-comment-indent-style
            (let* ((char (if (stringp chf-comment-indent-char)
                             (aref chf-comment-indent-char 0)
                           chf-comment-indent-char))
                   (chars (string ?\s ?\t char)))
              (goto-char (match-end 0))
              (skip-chars-backward chars)
              (delete-region (point) (progn (skip-chars-forward chars)
                                            (point)))
              (insert-char char (- col (current-column)))))
      (if (looking-at "\t[1-9]")
          (if indent-tabs-mode
              (goto-char (match-end 0))
            (delete-char 2)
            (insert-char ?\s 5)
            (insert chf-continuation-string))
        (if (looking-at " \\{5\\}[^ 0\n]")
            (if indent-tabs-mode
                (progn (delete-char 6)
                       (insert ?\t (chf-numerical-continuation-char) 1))
              (forward-char 6))
          (delete-horizontal-space)
          ;; Put line number in columns 0-4, or
          ;; continuation character in column 5.
          (cond ((eobp))
                ((looking-at (regexp-quote chf-continuation-string))
                 (if indent-tabs-mode
                     (progn
                       (indent-to
                        (if indent-tabs-mode
                            chf-minimum-statement-indent-tab
                          chf-minimum-statement-indent-fixed))
                       (delete-char 1)
                       (insert-char (chf-numerical-continuation-char) 1))
                   (indent-to 5)
                   (forward-char 1)))
                ((looking-at "[0-9]+")
                 (let ((extra-space (- 5 (- (match-end 0) (point)))))
                   (if (< extra-space 0)
                       (message "Warning: line number exceeds 5-digit limit.")
                     (indent-to (min chf-line-number-indent extra-space))))
                 (skip-chars-forward "0-9")))))
      ;; Point is now after any continuation character or line number.
      ;; Put body of statement where specified.
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
      (when (chf-find-comment-start-skip)
        (goto-char (match-beginning 0))
        (unless (= (current-column) (chf-comment-indent))
          (delete-horizontal-space)
          (indent-to (chf-comment-indent)))))))

(defun chf-line-number-indented-correctly-p ()
  "Return t if current line's line number is correctly indented.
Do not call if there is no line number."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (<= (current-column) chf-line-number-indent)
         (or (= (current-column) chf-line-number-indent)
             (progn (skip-chars-forward "0-9")
                    (= (current-column) 5))))))

(defun chf-check-for-matching-do ()
  "When called from a numbered statement, return t if matching DO is found.
Otherwise return nil."
  (let ((case-fold-search t)
        charnum)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "[ \t]*[0-9]+")
        (skip-chars-forward " \t")
        (skip-chars-forward "0")        ; skip past leading zeros
        (setq charnum
              (buffer-substring (point) (progn
                                          (skip-chars-forward "0-9")
                                          (point))))
        (beginning-of-line)
        (save-restriction
          (save-excursion
            (narrow-to-defun)
            (and (re-search-backward
                  (concat
                   "\\(^[ \t0-9]*do[ \t]*0*"
                   charnum "\\b\\)\\|" "\\(^[ \t]*0*"
                   charnum "\\b\\)")
                  nil t)
                 (looking-at
                  (concat "^[ \t0-9]*do[ \t]*0*"
                          charnum)))))))))

(defun chf-find-comment-start-skip (&optional all)
  "Move to past `comment-start-skip' found on current line.
Return non-nil if `comment-start-skip' found, nil if not.
If ALL is nil, only match comments that start in column > 0."
  ;; Hopefully at some point we can just use the line below!  -stef
  ;; (comment-search-forward (line-end-position) t))
  (when (or all comment-start-skip)
    (let ((pos (point))
          (css (if comment-start-skip
                   (concat chf-comment-line-start-skip
                           "\\|" comment-start-skip)
                 chf-comment-line-start-skip)))
      (when (re-search-forward css (line-end-position) t)
        (if (and (or all (> (match-beginning 0) (line-beginning-position)))
                 (or (save-match-data
                       (not (chf-is-in-string-p (match-beginning 0))))
                     ;; Recurse for rest of line.
                     (chf-find-comment-start-skip all)))
            (point)
          (goto-char pos)
          nil)))))

;; From: ralf@up3aud1.gwdg.de (Ralf Fassel)
;; Test if TAB format continuation lines work.
(defun chf-is-in-string-p (where)
  "Return non-nil if WHERE (a buffer position) is inside a ChF string."
  (save-excursion
    (goto-char where)
    (cond
     ((bolp) nil)                       ; bol is never inside a string
     ((save-excursion                   ; comment lines too
        (beginning-of-line)
        (looking-at chf-comment-line-start-skip)) nil)
     (t (let ((parse-state '(0 nil nil nil nil nil 0))
              (quoted-comment-start (if comment-start
                                        (regexp-quote comment-start)))
              (not-done t)
              parse-limit end-of-line)
          ;; Move to start of current statement.
          (chf-next-statement)
          (chf-previous-statement)
          ;; Now parse up to WHERE.
          (while not-done
            (if (or ;; Skip to next line if:
                 ;; - comment line?
                 (looking-at chf-comment-line-start-skip)
                 ;; - at end of line?
                 (eolp)
                 ;; - not in a string and after comment-start?
                 (and (not (nth 3 parse-state))
                      comment-start
                      (equal comment-start
                             (char-to-string (preceding-char)))))
                (if (> (forward-line) 0)
                    (setq not-done nil))
              ;; else:
              ;; If we are at beginning of code line, skip any
              ;; whitespace, labels and tab continuation markers.
              (if (bolp) (skip-chars-forward " \t0-9"))
              ;; If we are in column <= 5 now, check for continuation char.
              (cond ((= 5 (current-column)) (forward-char 1))
                    ((and (< (current-column) 5)
                          (equal chf-continuation-string
                                 (char-to-string (following-char)))
                          (forward-char 1))))
              ;; Find out parse-limit from here.
              (setq end-of-line (line-end-position))
              (setq parse-limit (min where end-of-line))
              ;; Parse max up to comment-start, if non-nil and in current line.
              (if comment-start
                  (save-excursion
                    (if (re-search-forward quoted-comment-start end-of-line t)
                        (setq parse-limit (min (point) parse-limit)))))
              ;; Now parse if still in limits.
              (if (< (point) where)
                  (setq parse-state (parse-partial-sexp
                                     (point) parse-limit nil nil parse-state))
                (setq not-done nil))))
          ;; Result.
          (nth 3 parse-state))))))

;; From old version.
(defalias 'chf-auto-fill-mode 'auto-fill-mode)

(defun chf-fill ()
  "Fill the current line at an appropriate point(s)."
  (let* ((auto-fill-function #'chf-auto-fill)
         (opoint (point))
         (bol (line-beginning-position))
         (eol (line-end-position))
         (bos (min eol (+ bol (chf-current-line-indentation))))
         ;; If in a string at fill-column, break it either before the
         ;; initial quote, or at fill-col (if string is too long).
         (quote
          (save-excursion
            (goto-char bol)
            ;; OK to break quotes on comment lines.
            (unless (looking-at chf-comment-line-start-skip)
              (let (fcpoint start)
                (move-to-column fill-column)
                (when (chf-is-in-string-p (setq fcpoint (point)))
                  (save-excursion
                    (re-search-backward "\\S\"\\s\"\\S\"?" bol t)
                    (setq start
                          (if chf-break-before-delimiters
                              (point)
                            (1+ (point)))))
                  (if (re-search-forward "\\S\"\\s\"\\S\"" eol t)
                      (backward-char 2))
                  ;; If the current string is longer than (fill-column
                  ;; - 6) chars, break it at the fill column (else
                  ;; infinite loop).
                  (if (> (- (point) start)
                         (- fill-column 6 chf-continuation-indent))
                      fcpoint
                    start))))))
         ;; Decide where to split the line. If a position for a quoted
         ;; string was found above then use that, else break the line
         ;; before/after the last delimiter.
         (fill-point
          (or quote
              (save-excursion
                ;; If f-b-b-d is t, have an extra column to play with,
                ;; since delimiter gets shifted to new line.
                (move-to-column (if chf-break-before-delimiters
                                    (1+ fill-column)
                                  fill-column))
                (let ((repeat t))
                  (while repeat
                    (setq repeat nil)
                    ;; Adapted from f90-find-breakpoint.
                    (re-search-backward chf-break-delimiters-re bol)
                    (if (not chf-break-before-delimiters)
                        (if (looking-at chf-no-break-re)
                            ;; Deal with cases such as "**" split over
                            ;; fill-col. Simpler alternative would be
                            ;; to start from (1- fill-column) above.
                            (if (> (+ 2 (current-column)) fill-column)
                                (setq repeat t)
                              (forward-char 2))
                          (forward-char 1))
                      (backward-char)
                      (or (looking-at chf-no-break-re)
                          (forward-char)))))
                ;; Line indented beyond fill-column?
                (when (<= (point) bos)
                  (move-to-column (1+ fill-column))
                  ;; What is this doing???
                  (or (re-search-forward "[\t\n,'+-/*)=]" eol t)
                      (goto-char bol)))
                (if (bolp)
                    (re-search-forward "[ \t]" opoint t))
                (point)))))
    ;; If we are in an in-line comment, don't break unless the
    ;; line of code is longer than it should be. Otherwise
    ;; break the line at the column computed above.
    ;;
    ;; Need to use chf-find-comment-start-skip to make sure that
    ;; quoted !'s don't prevent a break.
    (when (and (save-excursion
                 (beginning-of-line)
                 (if (not (chf-find-comment-start-skip))
                     t
                   (goto-char (match-beginning 0))
                   (>= (point) fill-point)))
               (save-excursion
                 (goto-char fill-point)
                 (not (bolp)))
               (> (save-excursion
                    (goto-char opoint)
                    (current-column))
                  (min (1+ fill-column)
                       (+ (chf-calculate-indent)
                          chf-continuation-indent))))
      (goto-char fill-point)
      (chf-break-line)
      (end-of-line))))

(defun chf-break-line ()
  "Call `chf-split-line'.  Joins continuation lines first, then refills."
  (let ((bol (line-beginning-position))
        (comment-string
         (save-excursion
           (if (chf-find-comment-start-skip)
               (delete-and-extract-region
                (match-beginning 0) (line-end-position))))))
    ;; Forward line 1 really needs to go to next non white line.
    (if (save-excursion (forward-line)
                        (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]"))
        (progn
          (end-of-line)
          (delete-region (point) (match-end 0))
          (delete-horizontal-space)
          (chf-fill))
      (chf-split-line))
    (if comment-string
        (save-excursion
          (goto-char bol)
          (end-of-line)
          (delete-horizontal-space)
          (indent-to (chf-comment-indent))
          (insert comment-string)))))

(defun chf-analyze-file-format ()
  "Return nil if fixed format is used, t if TAB formatting is used.
Use `chf-tab-mode-default' if no non-comment statements are found
before the end or in the first `chf-analyze-depth' lines."
  (let ((i 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (or
                   (eobp)
                   (eq (char-after) ?\t)
                   (looking-at " \\{6\\}")
                   (> i chf-analyze-depth)))
        (forward-line)
        (setq i (1+ i)))
      (cond
       ((eq (char-after) ?\t) t)
       ((looking-at " \\{6\\}") nil)
       (t chf-tab-mode-default)))))

(defun chf-fill-paragraph (&optional justify)
  "Fill surrounding comment block as paragraphs, else fill statement.
Intended as the value of `fill-paragraph-function'.
A comment block is filled by calling `fill-comment-paragraph' with
argument JUSTIFY, otherwise `chf-fill-statement' is called.
Always returns non-nil (to prevent `fill-paragraph' being called)."
  (interactive "*P")
  (or (fill-comment-paragraph justify)
      (chf-fill-statement)
      t))

(defun chf-fill-statement ()
  "Fill a ChF statement up to `fill-column'."
  (interactive "*")
  (let ((auto-fill-function #'chf-auto-fill))
    (unless (save-excursion
              (beginning-of-line)
              (or (looking-at "[ \t]*$")
                  (looking-at chf-comment-line-start-skip)
                  (and comment-start-skip
                       (looking-at (concat "[ \t]*" comment-start-skip)))))
      (save-excursion
        ;; Find beginning of statement.
        (chf-next-statement)
        (chf-previous-statement)
        ;; Re-indent initially.
        (chf-indent-line)
        ;; Replace newline plus continuation field plus indentation with
        ;; single space.
        (while (progn
                 (forward-line)
                 (chf-remove-continuation)))
        (chf-previous-statement)))
    (chf-indent-line)))

(defun chf-strip-sequence-nos (&optional do-space)
  "Delete all text in column `chf-line-length' (default 72) and up.
This is assumed to be sequence numbers.  Normally also deletes
trailing whitespace after stripping such text.  Supplying prefix
arg DO-SPACE prevents stripping the whitespace."
  (interactive "*p")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (format "^.\\{%d\\}\\(.*\\)" chf-line-length)
                              nil t)
      (replace-match "" nil nil nil 1)
      (unless do-space (delete-horizontal-space)))))

;; This code used to live in add-log.el, but this is a better place for it.
(defun chf-current-defun ()
  "Function to use for `add-log-current-defun-function' in ChF mode."
  (save-excursion
    ;; We must be inside function body for this to work.
    (chf-beginning-of-subprogram)
    (let ((case-fold-search t))
      ;; Search for chf subprogram start.
      (if (re-search-forward
           chf-start-prog-re
           (save-excursion (chf-end-of-subprogram)
                           (point))
           t)
          (or (match-string-no-properties 2)
              (progn
                ;; Move to EOL or before first left paren.
                (if (re-search-forward "[(\n]" nil t)
                    (progn (backward-char)
                           (skip-chars-backward " \t"))
                  (end-of-line))
                ;; Use the name preceding that.
                (buffer-substring-no-properties (point) (progn (backward-sexp)
                                                               (point)))))
        "main"))))

(provide 'chf)

;;; chf-mode.el ends here
