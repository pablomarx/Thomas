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

; $Id: gambit-specific.scm,v 1.6 1992/09/23 19:29:03 birkholz Exp $

;;;; This file contains the definitions of all functions used in the
;;;; implementation of Dylan which aren't part of R4RS.

;;;; Populations

(load "poplat")

;;;; Hash tables that use weak links for objects

(load "hash")

;;;; Record package

(define (error:wrong-type-argument record-type expected-type procedure)
  (error (string-append
           "Record package,"
           (symbol->string procedure)
           ": wrong argument type.  Expected "
           expected-type
           ", got ")
         record-type))

(define (error:bad-range-argument field-name procedure-name)
  (error (string-append
           "Record package,"
           (symbol->string procedure-name)
           ": unknown field name")
         field-name))

(load "record")

;;;; Compiler's error procedure.

(define (dylan::error string . args)
  (apply error (string-append "Error: " string) args))

;;;; Load-up

(define (dylan::load file)
  (display "Loading ")
  (display file)
  (newline)
  (load file))

(define (implementation-specific:generate-file in-exprs out-expr)
  (define (print x) (newline) (display x))
  (define (pp-to-string exprs)
    (let ((port (open-output-string)))
      (for-each (lambda (x) (newline port) (pp x port))
		exprs)
      (let ((str (get-output-string port)))
	(close-output-port port)
	str)))
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
  (print "(##declare (standard-bindings) (not safe))")
  (newline)
  (pp out-expr)
  (newline))

;;;; Eval

(define (implementation-specific:eval expression)
  (eval expression))

;;;; Interface between Dylan condition system (runtime-exceptions.scm) and
;;;; native condition system.

(define *dylan-handlers* '())

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
  ;; implementation-specific:enter-debugger is only called by `break',
  ;; so I label the ##debug-repl with "*** Breakpoint".
  ;; Printing the arguments to `break':
  ;; This may not always print in the right place (##repl-out), but
  ;; the existance of ##newline below suggests to me that I can't
  ;; write display-simple-error to produce it's output on ##repl-out.
  (newline)
  (display-simple-error
   (dylan-call dylan:condition-format-string dylan-condition)
   (dylan-call dylan:condition-format-arguments dylan-condition))
  (newline)
  (##call-with-current-continuation
    (lambda (cont)
      (##sequentially
        (lambda ()
          (let ((out (##repl-out)))
            (##newline out)
            (##write-string "*** Breakpoint" out)
            (##newline out)
            (##debug-repl cont)))))))

(define (implementation-specific:induce-error format-string format-args)
  (apply error format-string format-args))

(define (implementation-specific:induce-type-error value class-name)
  (error (string-append "not an instance of " (symbol->string class-name) ":")
	value))

(define (implementation-specific:signal-unhandled-dylan-condition
         dylan-condition)
  (error "unhandled condition:" dylan-condition))

(define (implementation-specific:warning format-string format-args)
  (newline) (display "*** WARNING -- ")
  (display-simple-error format-string format-args))

(define (display-simple-error format-string format-args)
  (display format-string)
  (do ((args format-args (cdr args)))
      ((null? args) #t)
    (display " ")(write (car args))))

;;; Gambit errors consist of constant objects denoting the error type,
;;; plus a list of "args".  To hand both pieces of info to the Thomas
;;; error reflector, cons them together.  Here're the operations.
(define make-condition cons)
(define condition-type car)
(define condition-args cdr)

(define (implementation-specific:catch-all-errors handler thunk)
  (##catch-all (lambda (s args) (handler (make-condition s args))) thunk))

;;; All gambit errors will be reflected as <simple-errors>.  We
;;; convert any types to some, usually descriptive, string.

(define (implementation-specific:get-error-message scheme-condition)
  (let ((s (condition-type scheme-condition)))
    (case s
      ((##SIGNAL.IO-ERROR)
       "io-error")
      ((##SIGNAL.READ-ERROR)
       "read-error")
      ((##SIGNAL.UNBOUND-DYNAMIC-VAR)
       "unbound-dynamic-var")
      ((##SIGNAL.GLOBAL-UNBOUND)
       "global-unbound")
      ((##SIGNAL.GLOBAL-UNBOUND-OPERATOR)
       "global-unbound-operator")
      ((##SIGNAL.GLOBAL-NON-PROCEDURE-OPERATOR)
       "global-non-procedure-operator")
      ((##SIGNAL.NON-PROCEDURE-JUMP)
       "non-procedure-jump")
      ((##SIGNAL.NON-PROCEDURE-OPERATOR)
       "non-procedure-operator")
      ((##SIGNAL.NON-PROCEDURE-SEND)
       "non-procedure-send")
      ((##SIGNAL.WRONG-NB-ARG)
       "wrong-nb-arg")
      ((##SIGNAL.APPLY-ARG-LIMIT)
       "apply-arg-limit")
      ((##SIGNAL.HEAP-OVERFLOW)
       "heap-overflow")
      ((##SIGNAL.STACK-OVERFLOW)
       "stack-overflow")
      ((##SIGNAL.PLACEHOLDER-ALREADY-DETERMINED)
       "placeholder-already-determined")
      ((##SIGNAL.RUNTIME-ERROR)
       "runtime-error")
      ((##SIGNAL.GLOBAL-ENV-OVERFLOW)
       "global-env-overflow")
      ((##SIGNAL.SYNTAX-ERROR)
       "syntax-error")
      (else
       "other-error"))))

(define (implementation-specific:get-error-arguments scheme-condition)
  (condition-args scheme-condition))

(define (implementation-specific:is-reflected-error? string args)
  ;; Can't tell which <simple-error>s are reflected Gambit errors or
  ;; which are user-generated.  (Actually, if we kept track of the
  ;; above string constants, we could recognize them again.)  I don't
  ;; know how to continue from the catch-all error handler anyway.
  #f)

(define (implementation-specific:let-scheme-handle-it serious)
  ;; If implementation-specific:is-reflected-error? always returns #f,
  ;; this should never be called.
  (error "unexpected call to implementation-specific:let-scheme-handle-it"))

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

(define canonicalize-string-for-symbol
  (let ((converter (if (char=? #\a (string-ref (symbol->string 'a) 0))
		       char-downcase
		       char-upcase)))
    (lambda (string)
      (list->string (map converter (string->list string))))))

(load "msort")

(define (write-line x)
  (write x)
  (newline))

;;; pp -- already provided

(load "dynwind")

;;; Imaginary numbers aren't supported by all implementations
(define (get-+i) +i)
