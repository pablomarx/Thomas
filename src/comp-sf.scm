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

; $Id: comp-sf.scm,v 1.6 1992/09/05 16:05:03 jmiller Exp $

;;;; More of the compiler: simple special forms
;;;;
;;;; SET!, COND, CASE, SELECT, FOR

;;; SET!

(define (compile-SET!-form
	 e mod-vars bound-vars compiler multiple-values? continue)
  (must-be-list-of-length e 2 "SET! -- invalid syntax")
  (let ((place (car e))
	(value (cadr e)))
    (compiler value mod-vars bound-vars #F
     (lambda (value-code module-vars)
       (cond
	((variable-name? place)
	 (let ((name (variable->name place)))
	   (continue
	    `(LET ((!SAVED-VALUE ,value-code))
	       (SET! ,name !SAVED-VALUE)
	       !SAVED-VALUE)
	    (add-variable name bound-vars module-vars))))
	((list? place)
	 (compile-forms `((SETTER ,(car place)) ,@(cdr place))
	   module-vars bound-vars compiler #F
	   (lambda (subform-codes mod-vars)
	     (continue `(LET ((!SAVED-VALUE ,value-code))
			  (DYLAN::APPLY
			   ,multiple-values?
			   ,@(map (lambda (x) `(LAMBDA () ,x)) subform-codes)
			   (LAMBDA () !SAVED-VALUE))
			  !SAVED-VALUE)
		       mod-vars))))
	(else (dylan::error "SET! -- bad place specification")))))))

;;; COND

(define (compile-COND-form
	 clauses module-vars bound-vars compiler
	 multiple-values? continue)
  (define (compile-clause clause mod-vars continue)
    (must-be-list-of-at-least-length clause 1 "COND -- bad clause syntax")
    (let ((predicate (car clause))
	  (consequents (cdr clause)))
      (compiler predicate mod-vars bound-vars #F
	(lambda (pred-code mod-vars)
	  (if (null? consequents)
	      (continue `(,pred-code) mod-vars)
	      (compile-forms consequents mod-vars bound-vars
			     compiler multiple-values?
	        (lambda (consequent-code mod-vars)
		  (continue `(,pred-code ,@consequent-code) mod-vars))))))))
  (define (compile-remaining-clauses clauses mod-vars continue)
    (if (null? clauses)
	(continue `((ELSE ,compiled-sharp-f)) mod-vars)
	(compile-clause (car clauses) mod-vars
          (lambda (this-clause-code mod-vars)
	    (compile-remaining-clauses (cdr clauses) mod-vars
	      (lambda (rest-clauses-code mod-vars)
		(continue
		 (cons this-clause-code rest-clauses-code)
		 mod-vars)))))))
  (if (not (list? clauses)) (dylan::error "COND -- bad syntax" clauses))
  (compile-remaining-clauses clauses module-vars
    (lambda (clause-code mod-vars)
      (continue `(COND ,@clause-code) mod-vars))))

;;; CASE

(define (compile-CASE-form
	 e module-vars bound-vars compiler multiple-values? continue)
  (must-be-list-of-at-least-length e 1 "CASE -- bad syntax")
  (let ((Have-default? #F))
    (define (compile-clause clause mod-vars continue)
      (if (not (pair? clause))
	  (dylan::error "CASE -- bad clause syntax" clause))
      (let ((match-list (car clause))
	    (consequents (cdr clause)))
	(let ((new-match
	       (cond ((or (eq? match-list 'else:) (eq? match-list #T))
		      (set! Have-Default? #T)
		      'else)
		     ((list? match-list) match-list)
		     (else (dylan::error "CASE -- bad match list" clause)))))
	  (if (null? consequents)
	      (continue `(,new-match ,compiled-sharp-f) mod-vars)
	      (compile-forms consequents mod-vars
			     bound-vars compiler multiple-values?
		(lambda (consequent-code mod-vars)
		  (continue `(,new-match ,@consequent-code)
			    mod-vars)))))))
    (let ((expr (car e)))
      (compiler expr module-vars bound-vars #F
	(lambda (expr-code mod-vars)
	  (let loop ((clauses (cdr e))
		     (compiled '())
		     (mod-vars mod-vars))
	    (if (null? clauses)
		(continue
		 `(CASE ,expr-code
		    ,@(reverse compiled)
		    ,@(if Have-Default?
			  '()
			  `((ELSE
			    (DYLAN-CALL DYLAN:ERROR
					"CASE -- no matches" ',e)))))
		 mod-vars)
		(if Have-Default?
		    (dylan::error "CASE -- else isn't last clause" e)
		    (compile-clause (car clauses) mod-vars
		      (lambda (code mod-vars)
			(loop (cdr clauses)
			      (cons code compiled)
			      mod-vars)))))))))))

;;; SELECT

(define (compile-SELECT-form
	 e module-vars bound-vars compiler multiple-values? continue)
  (must-be-list-of-at-least-length e 2 "SELECT -- bad syntax")
  (let ((Have-Default? #F))
    (define (compile-clause clause mod-vars continue)
      (if (not (pair? clause))
	  (dylan::error "SELECT -- bad clause syntax" clause))
      (let ((match-list (car clause))
	    (consequents (cdr clause)))
	(let ((new-match
	       (cond ((or (eq? match-list 'else:)
			  (eq? match-list #T))
		      (set! Have-Default? #T)
		      'else)
		     ((list? match-list)
		      (compile-forms
		       match-list mod-vars bound-vars compiler #F
		       (lambda (matches new-mod-vars)
			 (set! mod-vars new-mod-vars)
			 `(OR ,@(map
				 (lambda (code)
				   `(DYLAN::APPLY
				     #F
				     (LAMBDA () !SELECT-TEST)
				     (LAMBDA () !SELECT-TARGET)
				     (LAMBDA () ,code)))
				 matches)))))
		     (else (dylan::error "SELECT -- bad match list" clause)))))
	  (if (null? consequents)
	      (continue `(,new-match ,compiled-sharp-f) mod-vars)
	      (compile-forms consequents mod-vars bound-vars
			     compiler multiple-values?
		(lambda (consequent-code mod-vars)
		  (continue `(,new-match ,@consequent-code)
			    mod-vars)))))))
    (let ((target (car e))
	  (test (cadr e)))
      (compile-forms (list target test) module-vars bound-vars compiler #F
	(lambda (expr-codes mod-vars)
	  (let ((target (car expr-codes))
		(test (cadr expr-codes)))
	    (let loop ((clauses (cddr e))
		       (compiled '())
		       (mod-vars mod-vars))
	      (if (null? clauses)
		  (continue
		   `(LET ((!SELECT-TARGET ,target)
			  (!SELECT-TEST ,test))
		      (COND
		       ,@(reverse compiled)
		       ,@(if Have-Default?
			     '()
			     `((ELSE
				(DYLAN-CALL DYLAN:ERROR
					    "SELECT -- no match" ',e))))))
		   mod-vars)
		  (if Have-Default?
		      (dylan::error "SELECT -- else isn't last clause" e)
		      (compile-clause (car clauses) mod-vars
			(lambda (code mod-vars)
			  (loop (cdr clauses)
				(cons code compiled)
				mod-vars))))))))))))

;;; FOR

(define (compile-FOR-form
	 e module-vars bound-vars compiler multiple-values? continue)
  (must-be-list-of-at-least-length e 2 "FOR -- bad syntax")
  (let ((bindings (car e))
	(end-test (cadr e))
	(body (cddr e)))
    (if (or (not (list? bindings))
	    (not (all? (lambda (x) (list-of-length? x 3))
		       bindings)))
	(dylan::error "FOR -- illegal binding list" e))
    (let ((vars (map (lambda (x)
		       (if (not (variable-name? (car x)))
			   (dylan::error "FOR -- illegal variable name" x e))
		       (variable->name (car x)))
		     bindings))
	  (inits (map cadr bindings))
	  (steps (map caddr bindings)))
      (let ((inner-bound (append vars bound-vars)))
	(compile-forms body module-vars inner-bound compiler #F
	  (lambda (body-code module-vars)
	    (compile-forms inits module-vars bound-vars compiler #F
	      (lambda (init-code module-vars)
		(compile-forms steps module-vars inner-bound compiler #F
		  (lambda (step-code module-vars)
		    (compile-forms
		     end-test module-vars inner-bound
		     compiler multiple-values?
		     (lambda (end-code module-vars)
		       (continue
			`(DO ,(map list vars init-code step-code)
			     ,end-code
			   ,@body-code)
			module-vars)))))))))))))

