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

; $Id: full-test.scm,v 1.2 1992/09/21 21:26:17 birkholz Exp $

(define (display-simple-condition condition)
  (display (dylan-call dylan:condition-format-string condition))
  (do ((args (dylan-call dylan:condition-format-arguments condition)
	     (cdr args)))
      ((null? args))
    (display " ") (write (car args))))

(define (display-condition condition)
  (newline)
  (let ((condition-type (get-type condition)))
    (cond
     ((eq? condition-type <simple-error>)
      (display ";Error: ") (display-simple-condition condition))
     ((eq? condition-type <simple-warning>)
      (display ";Warning: ") (display-simple-condition condition))
     ((eq? condition-type <type-error>)
      (display ";Error: ")
      (write (dylan-call dylan:type-error-value condition))
      (display " is not an instance of ")
      (display (class.debug-name
		(dylan-call dylan:type-error-expected-type
			    condition))))
     (else
      (display ";Unhandled dylan condition: ")
      (write condition)))))

(define (make-expression preamble compiled-output)
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
		   (DISPLAY ";Value[")(DISPLAY COUNT)(DISPLAY "]: ")
		   (WRITE RESULT)
		   (RESULT-LOOP (+ 1 COUNT) (CDR RESULTS)))
		 (NEWLINE)))
	   (BEGIN
	     (NEWLINE)(DISPLAY ";Value: ")(WRITE !RESULT)(NEWLINE))))))

(define (test file)
  (with-input-from-file file
    (lambda ()
      (let loop ((module-variables '()))
	(let ((sexpr (read)))
	  (if (eof-object? sexpr)
	      (begin
		(newline)
		(newline))
	      (begin
		(pp sexpr)
		(loop
		 ;; Return from here with new module-variables.
		 (call-with-current-continuation
		  (lambda (error-exit)
		    (dylan::catch-all-conditions
		     (lambda ()
		       (dylan::handler-bind
			<condition>			; type
			(make-dylan-callable		; function
			 (lambda (condition next-handler)
			   next-handler
			   (display-condition condition)
			   (newline)
			   (error-exit module-variables)))
			(make-dylan-callable		; test
			 (lambda (condition)
			   condition
			   #T))
			(make-dylan-callable		; description
			 (lambda (stream)
			   (display "error handler from full-test.scm"
				    stream)))
			(lambda ()
			  (compile-expression
			   sexpr '!MULTIPLE-VALUES module-variables
			   (lambda (new-vars preamble compiled-output)
			     (implementation-specific:eval
			      (make-expression preamble compiled-output))
			     (append new-vars module-variables)))))))))))))))))

(define (test-dylan-examples) (test "dylan-examples.dyl"))
