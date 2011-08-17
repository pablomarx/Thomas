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

; $Id: mit-rep.scm,v 1.3 1992/09/21 21:29:31 birkholz Exp $

;;; Use a package called (THOMAS), but also keep a list of known module
;;; variables in !thomas-rep-module-variables in that environment.

(define (empty-thomas-environment!)
  (let ((package (name->package '(THOMAS)))
	(parent (name->package '()))
	(set-package/children!
	 (environment-lookup (->environment '(package))
			     'set-package/children!)))
    (set-package/children! parent
      (delq! package (package/children parent)))
    (package/add-child! parent 'THOMAS
			(let ((!THOMAS-REP-MODULE-VARIABLES '()))
			  (the-environment)))
    unspecific))

(define (thomas-rep)
  (repl/start (make-thomas-repl)
	      (cmdl-message/active
	       (lambda (port)
		 (let ((n-names
			(length (environment-bindings
				 (->environment '(THOMAS))))))
		   (newline port)
		   (display "Entering Thomas" port)
		   (newline port)
		   (display "(There " port)
		   (display (if (= n-names 1) "is" "are") port)
		   (display " now " port)
		   (display n-names port)
		   (display " defined name" port)
		   (display (if (= n-names 1) "" "s") port)
		   (display "s available.)" port)
		   (newline port))))))

(define make-thomas-repl
  (let ((make-repl-state
	 (environment-lookup (->environment '(runtime rep)) 'make-repl-state))
	(default-repl-operations
	  (environment-lookup (->environment '(runtime rep))
			      'default-repl-operations)))
    (lambda ()
      (let ((p (nearest-repl)))
	(make-cmdl p				; parent
		   (cmdl/port p)		; port
		   thomas-repl-driver		; driver
		   (make-repl-state		; state
		    "?"				;     prompt
		    (->environment '(Thomas))	;     environment
		    (repl/syntax-table p)	;     syntax-table
		    false			;     condition
		    )
		   default-repl-operations	; operations
		   )))))

;;; This is a modified copy of repl-driver from runtime/rep.scm

(define (thomas-repl-driver repl)
  (let ((reader-history (repl/reader-history repl)))
    (fluid-let ((standard-error-hook false)
		(standard-warning-hook false))
      (let ((env (->environment '(THOMAS))))
	(let loop ()
	  (thomas-eval (let ((s-expression
			      (prompt-for-command-expression
			       (string-append
				(number->string (cmdl/level repl))
				" "
				(repl/prompt repl))
			       (cmdl/port repl))))
			 (repl-history/record! reader-history s-expression)
			 s-expression)
		       env)
	  (loop))))))

(define (thomas-eval sexpr environment)
  (compile-expression
   sexpr
   '!MULTIPLE-VALUES
   (environment-lookup environment '!THOMAS-REP-MODULE-VARIABLES)
   (lambda (new-vars preamble compiled-output)
     (eval
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
	       (IF (UNDEFINED-VALUE? !RESULT)
		   (BEGIN
		     (NEWLINE)(DISPLAY ";No value")(NEWLINE))
		   (BEGIN
		     (NEWLINE)(DISPLAY ";Value: ")(WRITE !RESULT)(NEWLINE)))))
	 (SET! !THOMAS-REP-MODULE-VARIABLES
	       (APPEND ',new-vars !THOMAS-REP-MODULE-VARIABLES)))
      environment))))

(empty-thomas-environment!)

(display "
Apply thomas-rep to start a Thomas read-eval-print loop.
")
