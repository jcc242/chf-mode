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

(defvar chf-mode-keyword-words
  (concat "\\<"
          (regexp-opt '("CHF_MULTIDO" "CHF_AUTOMULTIDO" "CHF_ENDDO")
		      'paren)
          "\\>"))


(defvar chf-mode-builtin-words
  (concat "\\<" (regexp-opt
		 '("CHF_DDECL" "CHF_AUTODECL" "CHF_DTERM"
		   "CHF_DINVTERM" "CHF_DSELECT" "CHF_ID" "CHF_AUTOID"
		   "CHF_IX" "CHF_AUTOIX" "CHF_OFFSETIX"
		   "CHF_LBOUND" "CHF_UBOUND" "CHF_NCOMP")
		 'paren) "\\>"))

(defvar chf-mode-type-words
  (concat "\\<" (regexp-opt
		 '("real_t" "CHF_INT" "CHF_CONST_INT" 
		   "CHF_REAL" "CHF_CONST_REAL" "CHF_COMPLEX"
		   "CHF_CONST_COMPLEX" "CHFF_REALVECT" "CHF_CONST_REALVECT"
		   "CHF_INTVECT" "CHF_CONST_INTVECT" "CHF_I1D"
		   "CHF_CONST_I1D" "CHF_R1D" "CHF_CONST_R1D" "CHF_VI"
		   "CHF_CONST_VI" "CHF_VR" "CHF_CONST_VR" "CHF_VC"
		   "CHF_CONST_VC" "CHF_FIA" "CHF_CONST_FIA" "CHF_FRA"
		   "CHF_CONST_FRA" "CHF_FIA1" "CHF_CONST_FIA1"
		   "CHF_FRA1" "CHF_CONST_FRA1" "CHF_BOX")
		 'paren)
	  "\\>"))

;; This emacs regexp syntax finds strings inside of square brackets
;; and marks them as variables
;; "Read" excludes all numbers: "\\(\\[\\)\\([[:alpha:]]+\\w*;?\\w?\\)\\(\\]\\)"
;; "String" excludes only numbers: "\(\[\)\([[:alpha:]]+\w*;?\w?\)\(\]\)"
(defvar chf-mode-variable-words
      "\\(\\[\\)\\([[:alpha:]]+\\w*;?\\w?\\)\\(\\]\\)")
      
(defvar chf-mode-mode-font-lock-keywords
      `((,chf-mode-keyword-words . font-lock-keyword-face)
	(,chf-mode-type-words . font-lock-type-face)
	(,chf-mode-builtin-words . font-lock-preprocessor-face)
	(,chf-mode-variable-words 2 font-lock-variable-name-face)
	))

(define-derived-mode chf-mode fortran-mode "ChF"
  "Major mode for Chombo Fortran!"
  (font-lock-add-keywords nil chf-mode-mode-font-lock-keywords))

(provide 'chf-mode)

(defun fortran-calculate-indent ()
  "Calculates the Fortran indent column based on previous lines."
  (let (icol first-statement (case-fold-search t)
             (fortran-minimum-statement-indent
              (if indent-tabs-mode
                  fortran-minimum-statement-indent-tab
                fortran-minimum-statement-indent-fixed)))
    (save-excursion
      (setq first-statement (fortran-previous-statement))
      (if first-statement
          (setq icol fortran-minimum-statement-indent)
        (if (= (point) (point-min))
            (setq icol fortran-minimum-statement-indent)
          (setq icol (fortran-current-line-indentation)))
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
                   (setq icol (+ icol fortran-if-indent))))
              ((looking-at "else\\(if\\)?\\b")
               (setq icol (+ icol fortran-if-indent)))
              ((looking-at "select[ \t]*case[ \t](.*)")
               (setq icol (+ icol fortran-if-indent)))
              ((looking-at "case[ \t]*(.*)")
               (setq icol (+ icol fortran-if-indent)))
              ((looking-at "case[ \t]*default\\b")
               (setq icol (+ icol fortran-if-indent)))
              ((looking-at "\\(otherwise\\|else[ \t]*where\\)\\b")
               (setq icol (+ icol fortran-if-indent)))
              ((looking-at "where[ \t]*(.*)[ \t]*\n")
               (setq icol (+ icol fortran-if-indent)))
              ((looking-at "\\<CHF_MULTIDO\\b")
               (setq icol (+ icol fortran-do-indent)))
	      ((looking-at "\\<CHF_AUTOMULTIDO\\b")
               (setq icol (+ icol fortran-do-indent)))
              ((looking-at "\\<do\\b")
               (setq icol (+ icol fortran-do-indent)))
              ((looking-at
                "\\(structure\\|union\\|map\\|interface\\)\
\\b[ \t]*[^ \t=(a-z]")
               (setq icol (+ icol fortran-structure-indent)))
              ((and (looking-at fortran-end-prog-re1)
                    (fortran-check-end-prog-re))
               ;; Previous END resets indent to minimum.
               (setq icol fortran-minimum-statement-indent))
              ;; Previous statement was a numbered DO loop without a
              ;; closing CONTINUE or END DO, so we indented the
              ;; terminator like the loop body.
              ((and fortran-check-all-num-for-matching-do
                    (not (looking-at "\\(continue\\|\\<CHF_ENDDO\\b\\|end[ \t]*do\\)\\>"))
                    (progn
                      (beginning-of-line)
                      (and (looking-at "[ \t]*[0-9]+")
                           (fortran-check-for-matching-do))))
               (setq icol (- icol fortran-do-indent))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))
            ;; Check for directive before comment, so as not to indent.
            ((looking-at fortran-directive-re)
             (setq fortran-minimum-statement-indent 0 icol 0))
            ((looking-at fortran-comment-line-start-skip)
             (cond ((eq fortran-comment-indent-style 'relative)
                    (setq icol (+ icol fortran-comment-line-extra-indent)))
                   ((eq fortran-comment-indent-style 'fixed)
                    (setq icol (+ fortran-minimum-statement-indent
                                  fortran-comment-line-extra-indent))))
             (setq fortran-minimum-statement-indent 0))
            ((or (looking-at (concat "[ \t]*"
                                     (regexp-quote
                                      fortran-continuation-string)))
                 (looking-at " \\{5\\}[^ 0\n]\\|\t[1-9]"))
             (skip-chars-forward " \t")
             ;; Do not introduce extra whitespace into a broken string.
             (setq icol
                   (if (fortran-is-in-string-p (point))
                       6
                     (+ icol fortran-continuation-indent))))
            (first-statement)
            ;; The terminating statement is actually part of the
            ;; loop body, so unless it is a CONTINUE or END DO, we
            ;; indent it like the loop body (see above).
            ((and fortran-check-all-num-for-matching-do
                  (looking-at "[ \t]*[0-9]+[ \t]*\
\\(continue\\|\\<CHF_ENDDO\\b\\|end[ \t]*do\\)\\>")
                  (fortran-check-for-matching-do))
             (setq icol (- icol fortran-do-indent)))
            (t
             (skip-chars-forward " \t0-9")
             (cond ((looking-at "end[ \t]*\\(if\\|select\\|where\\)\\b")
                    (setq icol (- icol fortran-if-indent)))
                   ((looking-at "else\\(if\\)?\\b")
                    (setq icol (- icol fortran-if-indent)))
                   ((looking-at "case[ \t]*\\((.*)\\|default\\>\\)")
                    (setq icol (- icol fortran-if-indent)))
                   ((looking-at "\\(otherwise\\|else[ \t]*where\\)\\b")
                    (setq icol (- icol fortran-if-indent)))
                   ((and (looking-at "continue\\b")
                         (fortran-check-for-matching-do))
                    (setq icol (- icol fortran-do-indent)))
		   ((looking-at "\\<CHF_ENDDO\\b")
                    (setq icol (- icol fortran-do-indent)))
                   ((looking-at "end[ \t]*do\\b")
                    (setq icol (- icol fortran-do-indent)))
                   ((looking-at "end[ \t]*\
\\(structure\\|union\\|map\\|interface\\)\\b[ \t]*[^ \t=(a-z]")
                    (setq icol (- icol fortran-structure-indent)))
                   ((and (looking-at fortran-end-prog-re1)
                         (fortran-check-end-prog-re)
                         (not (= icol fortran-minimum-statement-indent)))
                    (message "Warning: `end' not in column %d.  Probably\
 an unclosed block." fortran-minimum-statement-indent))))))
    (max fortran-minimum-statement-indent icol)))
