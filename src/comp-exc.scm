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

; $Id: comp-exc.scm,v 1.8 1992/09/09 20:30:50 jmiller Exp $

;;;; More of the compiler: exception handler special forms
;;;;
;;;; HANDLER-BIND, HANDLER-CASE

(define (compile-one-binding module-vars bound-vars really-compile)
  (lambda (type compiled-func compiled-form keywords continue)
    (let ((test (dylan::find-keyword
		 keywords 'TEST:
		 (lambda () '(METHOD (CONDITION) CONDITION #T))))
	  (desc (dylan::find-keyword
		 keywords 'DESCRIPTION:
		 (lambda () '(METHOD (STREAM) STREAM #F)))))
      (compile-forms (list type test desc)
		     module-vars bound-vars really-compile #F
		     (lambda (comp-handler-spec module-vars)
		       (continue
			`(DYLAN::HANDLER-BIND
			  ,(car comp-handler-spec)		; Type
			  ,compiled-func			; Function
			  ,(cadr comp-handler-spec)		; Test
			  ,(caddr comp-handler-spec)		; Description
			  (LAMBDA () ,compiled-form))
			module-vars))))))

(define (compile-HANDLER-BIND-form
	 e module-vars bound-vars really-compile multiple-values? continue)
  (must-be-list-of-at-least-length e 1 "HANDLER-BIND: bad syntax")
  (let ((handler-spec (car e))
	(form (if (null? (cdr e)) #F `(BEGIN ,@(cdr e)))))
    (must-be-list-of-at-least-length
     handler-spec 2 "HANDLER-BIND: bad syntax for handler specification")
    (let ((keywords (cddr handler-spec)))
      (validate-keywords keywords '(TEST: DESCRIPTION:) dylan::error)
      (let ((type (car handler-spec))
	    (func (cadr handler-spec)))
	(compile-forms (list func form)	; Form is a reduction
		       module-vars bound-vars
		       really-compile multiple-values?
		       (lambda (compiled-func-and-forms module-vars)
			 ((compile-one-binding module-vars bound-vars
					       really-compile)
			  type
			  (car compiled-func-and-forms)
			  (cadr compiled-func-and-forms)
			  keywords
			  continue)))))))

(define (compile-HANDLER-CASE-form
	 e module-vars bound-vars really-compile multiple-values? continue)
  (must-be-list-of-at-least-length e 1 "HANDLER-CASE: bad syntax")
  (let ((protected-form (car e))
	(protections (cdr e)))
    (for-each (lambda (protection)
		(must-be-list-of-at-least-length
		 protection 1
		 "HANDLER-CASE: bad protection clause")
		(must-be-list-of-at-least-length
		 (car protection) 1
		 "HANDLER-CASE: bad handler description"))
	      protections)
    (really-compile protected-form module-vars bound-vars multiple-values?
      (lambda (compiled-form module-vars)
	(let loop ((protections protections)
		   (code `(LET ((!HANDLER-CASE:VALUE ,compiled-form))
			    (!HANDLER-CASE:EXIT
			     (LAMBDA () !HANDLER-CASE:VALUE))))
		   (module-vars module-vars))
	  (if (null? protections)
	      (continue
	       `((CALL-WITH-CURRENT-CONTINUATION
		  (LAMBDA (!HANDLER-CASE:EXIT) ,code)))
	       module-vars)
	      (let* ((this-binding (car protections))
		     (protection (car this-binding))
		     (forms (cdr this-binding))
		     (type (car protection))
		     (keywords (cdr protection))
		     (condition
		      (dylan::find-keyword keywords 'CONDITION:
					   (lambda () #F))))
		(really-compile
		 (if forms `(BEGIN ,@forms) #F) module-vars
		 (if condition (cons condition bound-vars) bound-vars)
		 multiple-values?
		 (lambda (compiled-function-body module-vars)
		   ((compile-one-binding module-vars bound-vars really-compile)
		    type
		    `(LAMBDA (!HANDLER-CASE:MULTIPLE-VALUES
			      !NEXT-METHOD
			      ,(or condition '!CONDITION)
			      !NEXT-HANDLER)
		       !HANDLER-CASE:MULTIPLE-VALUES ; Ignore
		       !NEXT-METHOD	; Ignore
		       !NEXT-HANDLER	; Ignore
		       ,(or condition '!CONDITION) ; Ignore
		       (!HANDLER-CASE:EXIT
			(LAMBDA () ,compiled-function-body)))
		    code
		    keywords
		    (lambda (compiled-code module-vars)
		      (loop (cdr protections)
			    compiled-code
			    module-vars))))))))))))
