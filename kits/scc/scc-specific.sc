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

; $Id: scc-specific.scm,v 1.16 1992/09/23 15:35:25 birkholz Exp $

;;; This file contains the definitions of all functions used in the
;;; implementation of Dylan which aren't part of R4RS.

(module sccspecific)

;;;; Populations

;(load "aftergc.sc")
;(load "poplat.sc")

;;;; Hash tables that use weak links for objects

;(load "hash.sc")

;;;; Record package

;(load "record.sc")

;;;; Compiler's error procedure.

(define (dylan::error string . args)
  (error 'dylan::error (string-append string ": ~A") args))

;;;; Load-up

(define (dylan::load string)
  (load (string-append string ".scm")))

(define (implementation-specific:generate-file in-exprs out-expr)
  (define (print x) (newline) (display x))
  (print ";;;; Compiled output:")
  (print "")
  (print "(module dylan-compiled-code)")
  (print "")
  (pp out-expr)
  (newline))

;;;; Eval

(define (implementation-specific:eval expression)
  (eval expression))

;;;; Interface between Dylan condition system (runtime-exceptions.scm) and
;;;; native condition system.

(define *dylan-handlers* (list))
(define *scc-error-handler* *error-handler*)
(define scc-error-tag (list 'scc 'error))

(define (implementation-specific:push-handler
	 type function test description thunk)
  (dynamic-wind
   (lambda ()
     (set! *dylan-handlers* (cons (list type function test description)
				  *dylan-handlers*)))
   thunk
   (lambda ()
     (set! *dylan-handlers* (cdr *dylan-handlers*)))))

(define (implementation-specific:get-dylan-handler-frames)
  *dylan-handlers*)

(define (implementation-specific:enter-debugger dylan-condition)
  (*scc-error-handler*
   'enter-debugger "Dylan condition ~A" dylan-condition))

(define (implementation-specific:induce-error format-string format-args)
  (*scc-error-handler* 'induce-error
		       (string-append format-string ": ~A")
		       format-args))

(define (implementation-specific:induce-type-error value class-name)
  (*scc-error-handler*
   'induce-type-error "Type error. ~A not of type ~A."
   value class-name))

(define (implementation-specific:signal-unhandled-dylan-condition
	 dylan-condition)
  (*scc-error-handler*
   'signal-unhandled-dylan-condition "Dylan condition ~A" dylan-condition))

(define (implementation-specific:warning format-string format-args)
  (apply *scc-error-handler* 'warning format-string format-args))

(define (implementation-specific:catch-all-errors handler thunk)
  (let ((old-handler 'xxx))
    (dynamic-wind
     (lambda ()
       (set! old-handler *error-handler*)
       (set! *error-handler*
	     (lambda (procedure-name format-string . args)
	       (handler `(,scc-error-tag
			  ,procedure-name
			  ,format-string
			  ,@args)))))
     thunk
     (lambda ()
       (set! *error-handler* old-handler)))))

(define (implementation-specific:get-error-message scheme-condition)
  (if (and (pair? scheme-condition)
	   (eq? (car scheme-condition) scc-error-tag))
      (string-append (symbol->string (cadr scheme-condition))
		     (caddr scheme-condition))
      (*scc-error-handler* 'get-error-message
			   "Not a Scheme error: ~A"
			   scheme-condition)))

(define (implementation-specific:get-error-arguments scheme-condition)
  (if (and (pair? scheme-condition)
	   (eq? (car scheme-condition) scc-error-tag))
      (cdddr scheme-condition)
      (*scc-error-handler* 'get-error-arguments
			   "Not a Scheme error: ~A"
			   scheme-condition)))

(define (implementation-specific:is-reflected-error? f-string f-args)
  #F)

(define (implementation-specific:let-scheme-handle-it serious)
  ;; This can't happen if is-reflected-error? is returning #F  
  (car 34))

;;;; Additional Dylan bindings

(define (dylan:scheme-variable-ref mv nm variable-name)
  (eval variable-name))

(define (dylan:scheme-procedure-ref mv nm variable-name)
  (make-dylan-callable (eval variable-name)))

(define (dylan:pp mv nm obj)
  mv nm					; Ignored
  (pp obj))

(define implementation-specific:additional-dylan-bindings
  `((pp dylan:pp)
    (scheme-variable dylan:scheme-variable-ref)
    (scheme-procedure dylan:scheme-procedure-ref)))

;;;; Other things

;;; For conversion from strings to symbols, we need a function that
;;; canonicalizes the case of the string.

(define (canonicalize-string-for-symbol string)
  (list->string (map char-upcase (string->list string))))

;(load "msort.sc")

(define (write-line x)
  (write x)
  (newline))

;; pp -- already provided

;(load "dynwnd.sc")

;;; Imaginary numbers aren't supported by all implementations
(define (get-+i)
  (error 'get-+i "Complex numbers aren't supported"))

(define (numerator x) x)
(define (denominator x) 1)
(define (angle x) 0)
(define (magnitude x) x)
(define (real-part x) x)
(define (imag-part x) 0)
(define (make-polar mag angle)
  (if (zero? angle)
      mag
      (get-+i)))
(define (make-rectangular x y)
  (if (zero? y)
      x
      (get-+i)))
(define (rationalize x . y)
  (error 'rationalize "We aren't rational"))
