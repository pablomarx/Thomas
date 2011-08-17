;;; Using Aubrey's dynamic-wind to fluid-let *dylan-handlers* to the
;;; list of currently active handlers.

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
  (error (string-append "not a " (symbol->string class-name) ":")
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
