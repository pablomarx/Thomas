;*              Copyright 1992 Digital Equipment Corporation
;*                         All Rights Reserved
;*
;* Permission to use, copy, and modify this software and its documentation is
;* hereby granted only under the following terms and conditions.  Both the
;* above copyright notice and this permission notice must appear in all copies
;* of the software, derivative works or modified versions, and any portions
;* thereof, and both notices must appear in supporting documentation.
;*
;* Users of this software agree to the terms and conditions set forth herein,
;* and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;* right and license under any changes, enhancements or extensions made to the
;* core functions of the software, including but not limited to those affording
;* compatibility with other hardware or software environments, but excluding
;* applications which incorporate this software.  Users further agree to use
;* their best efforts to return to Digital any such changes, enhancements or
;* extensions that they make and inform Digital of noteworthy uses of this
;* software.  Correspondence should be provided to Digital at:
;*
;*			Director, Cambridge Research Lab
;*			Digital Equipment Corp
;*			One Kendall Square, Bldg 700
;*			Cambridge MA 02139
;*
;* This software may be distributed (but not offered for sale or transferred
;* for compensation) to third parties, provided such third parties agree to
;* abide by the terms and conditions of this notice.
;*
;* THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;* MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;* CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;* DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;* PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;* ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;* SOFTWARE.

; $Id: portable-rep.scm,v 1.8 1992/09/21 21:30:56 birkholz Exp $

;;; Just use current (user?) environment and keep a list of known module
;;; variables in thomas-rep-module-variables.

(define thomas-rep-module-variables '())

(define (empty-thomas-environment!)
  ;; Just dump thomas-rep-module-variables.
  (set! thomas-rep-module-variables '()))

(define (thomas-rep)
  (newline)
  (display "Entering Thomas read-eval-print-loop.")
  (newline)
  (display "Exit by typing \"thomas:done\"")
  (newline)
  (dylan::catch-all-conditions
   (lambda ()
     (let loop ()
       (newline)
       (display "? ")
       (let ((input (read)))
	 (newline)
	 (if (and (eq? input 'thomas:done))
	     'thomas:done
	     (compile-expression
	      input '!MULTIPLE-VALUES thomas-rep-module-variables
	      (lambda (new-vars preamble compiled-output)
		(implementation-specific:eval
		 `(BEGIN
		    ,@preamble
		    (LET* ((!MULTIPLE-VALUES (VECTOR '()))
			   (!RESULT ,compiled-output))
		      (IF (EQ? !RESULT !MULTIPLE-VALUES)
			  (LET RESULT-LOOP
			      ((COUNT 1)
			       (RESULTS (VECTOR-REF !MULTIPLE-VALUES 0)))
			    (IF (PAIR? RESULTS)
				(LET ((RESULT (CAR RESULTS)))
				  (NEWLINE)
				  (DISPLAY ";Value[")(DISPLAY COUNT)
				  (DISPLAY "]: ")(WRITE RESULT)
				  (RESULT-LOOP (+ 1 COUNT) (CDR RESULTS)))
				(NEWLINE)))
			  (BEGIN
			    (NEWLINE)(DISPLAY ";Value: ")(WRITE !RESULT)
			    (NEWLINE))))))
		(set! thomas-rep-module-variables
		      (append new-vars thomas-rep-module-variables))
		(loop)))))))))

(display "
Apply thomas-rep to start a Thomas read-eval-print loop.
")
