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

; $Id: mit-specific.scm,v 1.30 1992/09/21 13:07:02 jmiller Exp $

;;;; This file contains the definitions of all functions used in the
;;;; implementation of Dylan which aren't part of R4RS.

;;;; Populations

(define make-population
  (environment-lookup (->environment '(runtime population))
		      'make-population))
(define add-to-population!
  (environment-lookup (->environment '(runtime population))
		      'add-to-population!))
(define remove-from-population!
  (environment-lookup (->environment '(runtime population))
		      'remove-from-population!))
(define map-over-population
  (environment-lookup (->environment '(runtime population))
		      'map-over-population))
(define map-over-population!
  (environment-lookup (->environment '(runtime population))
		      'map-over-population!))

;;;; Hash tables that use weak links for objects

(define make-OneD-table
  (environment-lookup (->environment '(runtime 1d-property))
		      'make-1d-table))
(define OneD-table/get
  (environment-lookup (->environment '(runtime 1d-property))
		      '1d-table/get))
(define OneD-table/put!
  (environment-lookup (->environment '(runtime 1d-property))
		      '1d-table/put!))

;;;; Record package

(define make-record-type
  (environment-lookup (->environment '(runtime record))
		      'make-record-type))
(define record-accessor
  (environment-lookup (->environment '(runtime record))
		      'record-accessor))
(define record-constructor
  (environment-lookup (->environment '(runtime record))
		      'record-constructor))
(define record-predicate
  (environment-lookup (->environment '(runtime record))
		      'record-predicate))
(define record-updater
  (environment-lookup (->environment '(runtime record))
		      'record-updater))

;;;; Compiler's error procedure.

(define (dylan::error string . args)
  (apply error (string-append "Error: " string) args))

;;;; Load-up

(define dylan::load load)

(define with-output-to-string
  (environment-lookup (->environment '(runtime string-output))
		      'with-output-to-string))

(define (implementation-specific:generate-file in-exprs out-expr)
  (define (print x) (newline) (display x))
  (define (pp-as-code x)
    (pp x (current-output-port) 'as-code))
  (define (pp-to-string exprs)
    (with-output-to-string
      (lambda ()
	(for-each (lambda (x) (pp-as-code x))
		  exprs))))
  (define (split-char-list chars continue)
    (let loop ((output '())
	       (chars chars))
      (cond ((null? chars)
	     (continue (list->string (reverse output)) '()))
	    ((char=? (car chars) #\newline)
	     (continue (list->string (reverse output)) (cdr chars)))
	    (else (loop (cons (car chars) output) (cdr chars))))))
  (define (string->strings string)
    (let loop ((output '())
	       (input (string->list string)))
      (if (null? input)
	  (reverse output)
	  (split-char-list input
	    (lambda (string rest-chars)
	      (loop (cons string output) rest-chars))))))
  (print ";;;; Input expressions:")
  (for-each (lambda (line)
	      (if (not (zero? (string-length line))) (display "; "))
	      (display line)
	      (newline))
	    (string->strings (pp-to-string in-exprs)))
  (print ";;;; Compiled output:")
  (newline)
  (print "(declare (usual-integrations))")
  (newline)
  (pp-as-code out-expr)
  (newline))

;;;; Eval

(define eval (environment-lookup (->environment '()) 'eval))
(define nearest-repl/environment
  (environment-lookup (->environment '(runtime rep))
		      'nearest-repl/environment))

(define (implementation-specific:eval expression)
  (eval expression (nearest-repl/environment)))

;;;; Interface between Dylan condition system (runtime-exceptions.scm) and
;;;; native condition system.

(define access-condition
  (environment-lookup (->environment '(runtime error-handler))
		      'access-condition))
(define bind-condition-handler
  (environment-lookup (->environment '(runtime error-handler))
		      'bind-condition-handler))
(define condition-predicate
  (environment-lookup (->environment '(runtime error-handler))
		      'condition-predicate))
(define condition-signaller
  (environment-lookup (->environment '(runtime error-handler))
		      'condition-signaller))
(define condition-type:error
  (environment-lookup (->environment '(runtime error-handler))
		      'condition-type:error))
(define condition-type:simple-error
  (environment-lookup (->environment '(runtime error-handler))
		      'condition-type:simple-error))
(define condition?
  (environment-lookup (->environment '(runtime error-handler))
		      'condition?))
(define error
  (environment-lookup (->environment '(runtime error-handler))
		      'error))
(define error-irritant/noise
  (environment-lookup (->environment '(runtime error-handler))
		      'error-irritant/noise))
(define make-condition-type
  (environment-lookup (->environment '(runtime error-handler))
		      'make-condition-type))
(define standard-error-handler
  (environment-lookup (->environment '(runtime error-handler))
		      'standard-error-handler))
(define warn
  (environment-lookup (->environment '(runtime error-handler))
		      'warn))

(define dylan-handler-frames '())

(define reflected-error-tag "Scheme condition: ")

(define condition-type:unhandled-dylan-condition
  (make-condition-type
      'UNHANDLED-DYLAN-CONDITION condition-type:error
      '(DYLAN-CONDITION)
    (lambda (condition port)
      (display "Error: unhandled dylan condition" port)
      (display (access-condition condition 'DYLAN-CONDITION) port))))

(define error:unhandled-dylan-condition
  (condition-signaller condition-type:unhandled-dylan-condition
		       '(DYLAN-CONDITION)
		       standard-error-handler))

(define is-simple-error?
  (condition-predicate condition-type:simple-error))

(define (implementation-specific:push-handler
	 type function test description thunk)
  (fluid-let ((dylan-handler-frames
	       (cons (list type function test description)
		     dylan-handler-frames)))
    (thunk)))

(define (implementation-specific:get-dylan-handler-frames)
  dylan-handler-frames)

(define (implementation-specific:enter-debugger dylan-condition)
  (bkpt dylan-condition))

(define (implementation-specific:induce-error format-string format-args)
  (apply error (string-append "Error: " format-string) format-args))

(define (implementation-specific:induce-type-error value class-name)
  (error "Error:" value
	 (error-irritant/noise " is not an instance of")
	 class-name))

(define (implementation-specific:signal-unhandled-dylan-condition
	 dylan-condition)
  (error:unhandled-dylan-condition dylan-condition))

(define (implementation-specific:warning format-string format-args)
  (apply warn format-string format-args))

(define (implementation-specific:catch-all-errors handler thunk)
  (bind-condition-handler '() handler thunk))

(define (implementation-specific:get-error-message scheme-condition)
  (if (is-simple-error? scheme-condition)
      (access-condition scheme-condition 'MESSAGE)
      reflected-error-tag))

(define (implementation-specific:get-error-arguments scheme-condition)
  (if (is-simple-error? scheme-condition)
      (access-condition scheme-condition 'IRRITANTS)
      (list scheme-condition)))

(define (implementation-specific:is-reflected-error? f-string f-args)
  (and (eq? reflected-error-tag f-string)
       (pair? f-args)
       (condition? (car f-args))
       (null? (cdr f-args))))

(define (implementation-specific:let-scheme-handle-it serious)
  serious
  ;; Return back into catch-all-conditions to propogate the error back along
  ;; the Scheme error handler chain
  #F)

;;;; Additional Dylan bindings

(define dylan:scheme-variable-ref
  (let ((env (nearest-repl/environment)))
    (lambda (mv nm variable-name)
      mv nm				; Ignored
      (environment-lookup env variable-name))))

(define dylan:scheme-procedure-ref
  (let ((env (nearest-repl/environment)))
    (lambda (mv nm variable-name)
      mv nm				; Ignored
      (make-dylan-callable
       (environment-lookup env variable-name)))))

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

(define canonicalize-string-for-symbol
  (environment-lookup (->environment '()) 'string-downcase))

(define sort
  (environment-lookup (->environment '())
		      'sort))

(define write-line
  (environment-lookup (->environment '(runtime output-port))
		      'write-line))

(define pp
  (environment-lookup (->environment '(runtime pretty-printer))
		      'pp))

(define dynamic-wind
  (environment-lookup (->environment '(runtime state-space))
		      'dynamic-wind))

;;; Imaginary numbers aren't supported by all implementations
(define (get-+i) +i)

(define numerator
  (environment-lookup (->environment '(runtime number))
		      'numerator))
(define denominator
  (environment-lookup (->environment '(runtime number))
		      'denominator))
(define angle
  (environment-lookup (->environment '(runtime number))
		      'angle))
(define magnitude
  (environment-lookup (->environment '(runtime number))
		      'magnitude))
(define real-part
  (environment-lookup (->environment '(runtime number))
		      'real-part))
(define imag-part
  (environment-lookup (->environment '(runtime number))
		      'imag-part))
(define make-polar
  (environment-lookup (->environment '(runtime number))
		      'make-polar))
(define make-rectangular
  (environment-lookup (->environment '(runtime number))
		      'make-rectangular))
(define rationalize
  (environment-lookup (->environment '(runtime number))
		      'rationalize))