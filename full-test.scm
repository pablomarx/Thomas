(define (test:condition-handler exit)
  (define (test:display-simple-condition condition)
    (display (dylan-call dylan:condition-format-string condition))
    (do ((args (dylan-call dylan:condition-format-arguments condition)
	       (cdr args)))
	((null? args))
      (display " ") (write (car args))))

  (make-dylan-callable
   (lambda (condition next-handler)
     next-handler
     (newline)
     (let ((condition-type (get-type condition)))
       (cond
	((eq? condition-type <simple-error>)
	 (display "error: ") (test:display-simple-condition condition))
	((eq? condition-type <simple-warning>)
	 (display "warning: ") (test:display-simple-condition condition))
	((eq? condition-type <type-error>)
	 (display "error: ")
	 (write (dylan-call dylan:type-error-value condition))
	 (display " is not a ")
	 (display (class.debug-name
		   (dylan-call dylan:type-error-expected-type
			       condition))))
	(else
	 (display "unhandled dylan condition: ")
	 (write condition))))
     (exit 'ignored))))

(define (make-expression preamble compiled-output)
  `(BEGIN
     ,@preamble
     (LET* ((!MULTIPLE-VALUES (VECTOR '()))
	    (!RESULT ,compiled-output))
       (NEWLINE)
       (IF (EQ? !RESULT !MULTIPLE-VALUES)
	   (LET RESULT-LOOP
	       ((RESULTS
		 (VECTOR-REF !MULTIPLE-VALUES 0))
		(COUNT 1))
	     (AND (PAIR? RESULTS)
		  (LET ((RESULT (CAR RESULTS)))
		    (NEWLINE)
		    (DISPLAY "Value[")
		    (DISPLAY (NUMBER->STRING COUNT))
		    (DISPLAY "]: ")
		    (WRITE RESULT)
		    (RESULT-LOOP (CDR RESULTS) (+ 1 COUNT)))))
	   (BEGIN
	     (DISPLAY "Value: ")
	     (WRITE !RESULT)
	     #F)))))

(define (catch-it exit expr)
  (dylan::catch-all-conditions
   (lambda ()
     (dylan::handler-bind
      <condition>			; type
      (test:condition-handler exit)	; function
      (make-dylan-callable		; test
       (lambda (condition)
	 condition
	 #T))
      (make-dylan-callable		; description
       (lambda (stream)
	 (display "error handler from full-test.scm" stream)
	 #F))
      (lambda () (implementation-specific:eval expr))))))

(define (main-loop module-variables)
  (let ((sexpr (read)))
    (or (eof-object? sexpr)
	(begin
	  (newline)
	  (pp sexpr)
	  (compile-expression
	   sexpr '!MULTIPLE-VALUES module-variables
	   (lambda (new-vars preamble compiled-output)
	     (call-with-current-continuation
	      (lambda (error-exit)
		(catch-it error-exit
			  (make-expression preamble compiled-output))))
	     (main-loop (append new-vars module-variables))))))))

(define (go file-name)
  (with-input-from-file file-name (lambda () (main-loop '()))))

(define (test) (go "dylan-examples.dyl"))

