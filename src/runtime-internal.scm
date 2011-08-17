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

; $Id: runtime-internal.scm,v 1.8 1992/09/01 19:35:06 jmiller Exp $

;;;; This file contains the functions that are referenced only
;;;; directly by the output of the Dylan to Scheme compiler, rather
;;;; than user code written in Dylan.
;;;;
;;;; Many of the functions here are just renamings of ordinary Scheme
;;;; functions.  The renaming is necessary to prevent name clashes
;;;; between Dylan and Scheme variables at run time.

(define (dylan::free-variable-ref value name)
  (if (eq? value the-unassigned-value)
      (dylan-call dylan:error "unbound variable" name)
      value))

(define dylan::call/cc			; Used for BIND-EXIT
  call-with-current-continuation)

(define (dylan::dotimes count result-fn fn)
  ;; Used for DOTIMES special form
  (let loop ((n 0))
    (if (>= n count)
	(result-fn)
	(begin
	  (fn n)
	  (loop (+ n 1))))))

(define (dylan::while test thunk)
  ;; Used for UNTIL and WHILE special forms
  (let loop ()
    (if (test)
	(begin (thunk) (loop))
	#F)))

(define (dylan::apply multiple-values? operator-thunk . operand-thunks)
  ;; Used for combinations.
  ;; Forces left-to-right evaluation, and adds an initial #F next
  ;; method argument.
  (let loop ((rands '())
	     (rest operand-thunks))
    (if (null? rest)
	(apply (operator-thunk)
	       multiple-values?
	       NEXT-METHOD:NOT-GENERIC
	       (reverse rands))
	(let ((next ((car rest))))
	  (loop (cons next rands) (cdr rest))))))

(define dylan::scheme-apply apply)

(define dylan::dynamic-wind		; Used for UNWIND-PROTECT
  dynamic-wind)

(define (dylan::type-check value class) ; Used for BIND
  (let ((type-of-object (get-type value)))
    (if (not (subclass? type-of-object class))
	(dylan-call dylan:error
		    "BINDing-time restriction violation" class value))))

(define dylan::list list)		; BIND
(define dylan::cons cons)		; BIND
(define dylan::car car)			; BIND
(define dylan::vector vector)		; BIND
(define dylan::vector-ref vector-ref)	; BIND
(define dylan::not not)			; DEFINE-GENERIC-FUNCTION, UNLESS
(define dylan::eq? eq?)			; DEFINE-GENERIC-FUNCTION
(define dylan::class? class?)		; DEFINE-CLASS
(define dylan::make-param-list		; METHOD
  make-param-list)
(define dylan::add-method add-method)	; DEFINE-METHOD
(define dylan::null? null?)		; COND

(define (dylan::for-each fn . collections)
  (for-each
   (lambda (collection)
     (if (not (subclass? (get-type collection) <collection>))
	 (dylan-call dylan:error "for-each -- not a collection" collection)))
   collections)
  (collections-iterate fn
		       (lambda (result)
			 (and result (lambda () (car result))))
		       #F
		       collections))
