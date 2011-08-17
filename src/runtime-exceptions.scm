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

; $Id: runtime-exceptions.scm,v 1.21 1992/09/10 02:46:10 jmiller Exp $

;;;; The Dylan exception system.

;;; Note: Functions starting with "implementation-specific:" must be
;;; written for each Scheme implementation.  They go into files like
;;; "mit-specific.scm".

;;; Classes

(define dylan:condition-format-string
  (dylan::generic-fn 'condition-format-string
    (make-param-list `((condition ,<condition>)) #F #F #F)
    (lambda (condition)
      (dylan-call dylan:error
		  "(condition-format-string <condition>) -- no specialization"
		  condition))))

(define dylan:condition-format-arguments
  (dylan::generic-fn 'condition-format-arguments
    (make-param-list `((condition ,<condition>)) #F #F #F)
    (lambda (condition)
      (dylan-call
       dylan:error
       "(condition-format-arguments <condition>) -- no specialization"
       condition))))

(dylan::add-slot <simple-error>
  #F 'INSTANCE #F dylan:condition-format-string 'CONDITION-FORMAT-STRING
  #F #F #F 'FORMAT-STRING: #F)
(dylan::add-slot <simple-error>
  #F 'INSTANCE #F dylan:condition-format-arguments
  'CONDITION-FORMAT-ARGUMENTS #F #F #F 'FORMAT-ARGUMENTS: #F)

(define dylan:type-error-value
  (dylan::generic-fn 'type-error-value
    (make-param-list `((condition ,<condition>)) #F #F #F)
    (lambda (condition)
      (dylan-call dylan:error
		  "(type-error-value <condition>) -- no specialization"
		  condition))))

(define dylan:type-error-expected-type
  (dylan::generic-fn 'type-error-expected-type
    (make-param-list `((condition ,<condition>)) #F #F #F)
    (lambda (condition)
      (dylan-call dylan:error
		  "(type-error-expected-type <condition>) -- no specialization"
		  condition))))

(dylan::add-slot <type-error>
  #F 'INSTANCE #F dylan:type-error-value 'TYPE-ERROR-VALUE
  #F #F #F 'VALUE: #F)
(dylan::add-slot <type-error>
  #F 'INSTANCE #F dylan:type-error-expected-type
  'TYPE-ERROR-EXPECTED-TYPE #F #F #F 'TYPE: #F)

(dylan::add-slot <simple-warning>
  #F 'INSTANCE #F dylan:condition-format-string 'CONDITION-FORMAT-STRING
  #F #F #F 'FORMAT-STRING: #F)
(dylan::add-slot <simple-warning>
  #F 'INSTANCE #F dylan:condition-format-arguments
  'CONDITION-FORMAT-ARGUMENTS #F #F #F 'FORMAT-ARGUMENTS: #F)

;;; Basic Operators (pages 138 and 139)

(define (dylan::handler-bind type function test description thunk)
  ;; Assumes function is a method of two args, test is a method of one arg,
  ;; and description is a string or method of one argument.  Can't check???
  (if (and (not (and (class? type) (subclass? type <condition>)))
	   (not (and (singleton? type)
		     (subclass? (get-type (singleton.object type))
				<condition>))))
      (dylan-call dylan:error
		  "handler-bind -- not a <condition>" type))
  (implementation-specific:push-handler
   type function test description thunk))

(define (make-default-condition args type operator)
  ;; Handles one arg (a condition) or many args (format string and format
  ;; args).  Assumes type is a <simple-error> or <simple-warning>.
  (cond ((and (pair? args)
	      (subclass? (get-type (car args)) <condition>))
	 (if (null? (cdr args))
	     (car args)
	     (dylan-call dylan:error "extraneous args" operator (cdr args))))
	((and (pair? args)
	      (string? (car args)))
	 (dylan-call dylan:make
		     type
		     'FORMAT-STRING: (car args)
		     'FORMAT-ARGUMENTS: (cdr args)))
	(else (dylan-call dylan:error "bad first argument" operator args))))

(define (dylan:signal multiple-values next-method . args)
  next-method
  (let* ((condition (make-default-condition args <simple-warning> 'SIGNAL))
	 (condition-type (get-type condition)))
    (let frame-loop ((frames
		      (implementation-specific:get-dylan-handler-frames)))
      (if (pair? frames)
	  (let ((handler-type (caar frames))
		(handler-test (caddar frames)))
	    (if (and (or (and (singleton? handler-type)
			      (eq? condition (singleton.object handler-type)))
			 (subclass? condition-type handler-type))
		     (dylan-call handler-test condition))
		(let ((handler (cadar frames)))
		  (dylan-mv-call handler multiple-values
				 condition
				 (lambda (multiple-values next-method)
				   multiple-values next-method
				   (frame-loop (cdr frames)))))
		(frame-loop (cdr frames))))
	  (dylan-mv-call dylan:default-handler multiple-values condition)))))

;;; Full set of Operators for Signaling

(define dylan:error			; NOT continuable
  (make-dylan-callable
   (lambda args
     (dylan-call dylan:signal
		 (make-default-condition args <simple-error> 'ERROR))
     (dylan-call dylan:error "error -- attempt to return from error"))))

(define dylan:cerror			; OK to continue
  (make-dylan-callable
   (lambda (restart-description . others)
     (call-with-current-continuation
      (lambda (continue)
	(dylan::handler-bind
	 <simple-restart>
	 (make-dylan-callable		; Called if restart attempted
	  (lambda (condition next-handler)
	    condition next-handler
	    (continue #F)))
	 (make-dylan-callable		; Test: always ready to handle
	  (lambda (condition)
	    condition
	    #T))
	 restart-description
	 (lambda ()
	   (dylan-apply dylan:error others))))))))

(define dylan:break
  (make-dylan-callable
   (lambda args
     (call-with-current-continuation
      (lambda (continue)
	(dylan::handler-bind
	 <simple-restart>
	 (make-dylan-callable
	  (lambda (condition next-handler)
	    condition next-handler
	    (continue #F)))
	 (make-dylan-callable
	  (lambda (condition)
	    condition
	    #T))
	 "Continue from breakpoint."
	 (lambda ()
	   (implementation-specific:enter-debugger
	    (make-default-condition args <simple-error> 'BREAK))
	   #F)))))))

(define dylan:check-type
  (make-dylan-callable
   (lambda (value type)
     (if (not (dylan-call dylan:instance? value type))
	 (let ((condition (dylan-call dylan:make
				      <type-error> 'value: value 'type: type)))
	   (dylan-call dylan:signal condition))
	 value))
   2))

(define dylan:abort
  (lambda (multiple-values next-method)
    (dylan-full-call dylan:error multiple-values next-method
		     (dylan-call dylan:make <abort>))))

;;; Additional Operators for Handling

(define dylan:default-handler
  (dylan::generic-fn 'default-handler
    (make-param-list `((CONDITION ,<condition>)) #F #F #F)
    (lambda (condition) condition #F)))

(add-method
 dylan:default-handler
 (dylan::function->method
  (make-param-list `((CONDITION ,<serious-condition>)) #F #F #F)
  (lambda (serious)
    ;; Turn unhandled dylan condition into a Scheme condition unless it is
    ;; a dylan condition reflecting a Scheme condition, in which case, just
    ;; return so that the handler in dylan::catch-all-conditions will
    ;; return so that the remaining Scheme condition handlers may run.
    (let ((error-type (get-type serious)))
      (cond
       ((and (eq? error-type <simple-error>)
	     (implementation-specific:is-reflected-error?
	      (dylan-call dylan:condition-format-string serious)
	      (dylan-call dylan:condition-format-arguments serious)))
	(implementation-specific:let-scheme-handle-it serious))
       ((eq? error-type <simple-error>)
	(implementation-specific:induce-error
	 (dylan-call dylan:condition-format-string serious)
	 (dylan-call dylan:condition-format-arguments serious)))
       ((eq? error-type <type-error>)
	(let ((value (dylan-call dylan:type-error-value serious))
	      (expected-type (dylan-call dylan:type-error-expected-type
					 serious)))
	  (implementation-specific:induce-type-error
	   value (class.debug-name expected-type))))
       (else
	(implementation-specific:signal-unhandled-dylan-condition
	 serious)))))))

(add-method
 dylan:default-handler
 (dylan::function->method
  (make-param-list `((CONDITION ,<simple-warning>)) #F #F #F)
  (lambda (warning)
    (implementation-specific:warning
     (dylan-call dylan:condition-format-string warning)
     (dylan-call dylan:condition-format-arguments warning))
    #F)))

(add-method
 dylan:default-handler
 (dylan::function->method
  (make-param-list `((CONDITION ,<restart>)) #F #F #F)
  (lambda (restart)
    (dylan-call dylan:error
		"(default-handler <restart>) -- no handler established"
		restart))))

;;; Operators for Interactive Handling

(define dylan:restart-query
  (dylan::generic-fn 'restart-query
		     (make-param-list `((RESTART ,<restart>)) #F #F #F)
		     #F))

(add-method
 dylan:restart-query
 (dylan::dylan-callable->method
  (make-param-list `((RESTART ,<restart>)) #F #F #F)
  (lambda (multiple-values next-method restart)
    restart
    (dylan-full-call dylan:values multiple-values next-method))))

(define dylan:return-query
  (dylan::generic-fn 'return-query
		     (make-param-list `((CONDITION ,<condition>)) #F #F #F)
		     #F))

(add-method
 dylan:return-query
 (dylan::dylan-callable->method
  (make-param-list `((CONDITION ,<condition>)) #F #F #F)
  (lambda (multiple-values next-method condition)
    condition				; Ignored
    (display "RETURN-QUERY: please type in a list of values")
    (newline)
    (dylan-full-apply dylan:values multiple-values next-method (read)))))

;;; Operators for Introspection

(define dylan:do-handlers
  (make-dylan-callable
   (lambda (funarg)
     (do ((frames
	   (implementation-specific:get-dylan-handler-frames)
	   (cdr frames)))
	 ((null? frames))
       (let ((frame (car frames)))
	 (dylan-call funarg
		     (car frame) (cadr frame)
		     (caddr frame) (cadddr frame))))
     ;; No value???
     #F)
   1))

(define dylan:return-allowed?
  (dylan::generic-fn 'return-allowed?
    (make-param-list `((CONDITION ,<condition>)) #F #F #F)
    (lambda (condition) condition #F)))

(define dylan:return-description
  (dylan::generic-fn 'return-description
    (make-param-list `((CONDITION ,<condition>)) #F #F #F)
    (lambda (condition)
      (dylan-call dylan:error
		  "(return-description <condition>) -- not specialized"
		  condition))))

;;; Top-level wrapper for DYLAN code

(define catch-errors? #T)

(define (dylan::catch-all-conditions dylan-compiled-output)
  (if catch-errors?
      (implementation-specific:catch-all-errors
       dylan::scheme-condition-handler
       dylan-compiled-output)
      (dylan-compiled-output)))

;; A name for the condition handler...
(define (dylan::scheme-condition-handler condition)
  (dylan-call
   dylan:signal
   (dylan-call dylan:make <simple-error>
	       'FORMAT-STRING:
	       (implementation-specific:get-error-message condition)
	       'FORMAT-ARGUMENTS:
	       (implementation-specific:get-error-arguments condition))))
