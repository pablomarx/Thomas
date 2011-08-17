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

; $Id: rep.scm,v 1.5 1992/09/10 16:15:10 jmiller Exp $

(define (thomas-rep . mod-vars)
  ;; mod-vars is an optional list of variable names already assumed to exist.
  ;; Returns the names of the variables defined in the loop.
  (newline)
  (display "Entering Thomas read-eval-print-loop.")
  (newline)
  (display "Exit by typing \"thomas:done\"")
  (newline)
  (dylan::catch-all-conditions
   (lambda ()
     (let loop ((module-variables (if (null? mod-vars) '() (car mod-vars))))
       (newline)
       (display "? ")
       (let ((input (read)))
	 (newline)
	 (if (and (eq? input 'thomas:done))
	     module-variables		; Returns names defined in loop
	     (compile-expression
	      input '!MULTIPLE-VALUES module-variables
	      (lambda (new-vars preamble compiled-output)
		(implementation-specific:eval
		 `(BEGIN
		    ,@preamble
		    (LET* ((!MULTIPLE-VALUES (VECTOR '()))
			   (!RESULT ,compiled-output))
		      (IF (EQ? !RESULT !MULTIPLE-VALUES)
			  (LET* ((RESULT (VECTOR-REF !MULTIPLE-VALUES 0))
				 (COUNT (LENGTH RESULT)))
			    (DISPLAY COUNT)
			    (DISPLAY " value")
			    (IF (NOT (= COUNT 1)) (DISPLAY "s"))
			    (DISPLAY ":")
			    (FOR-EACH (LAMBDA (X) (NEWLINE) (WRITE X)) RESULT))
			  (BEGIN
			    (DISPLAY "Result: ")
			    (WRITE !RESULT))))))
		(loop (append new-vars module-variables))))))))))
