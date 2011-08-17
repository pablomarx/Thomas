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

; $Id: runtime-methods.scm,v 1.25 1992/09/19 06:54:56 birkholz Exp $

;;;; Methods used in the Dylan environment

;;; Methods (not generically dispatched)

;; dylan::function->method has been moved to support.scm -- it takes in
;; a scheme function and converts it to a Dylan-callable procedure by
;; ignoring the multiple-values and next-method arguments that come from Dylan.

(define dylan:+
  (dylan::function->method
   only-rest-args
   (lambda rest-args
     (if (null? rest-args)
	 0
	 (let loop ((rest (cdr rest-args))
		    (sum (car rest-args)))
	   (if (null? rest)
	       sum
	       (loop (cdr rest)
		     (dylan-call dylan:binary+ sum (car rest)))))))))

(define dylan:*
  (dylan::function->method
   only-rest-args
   (lambda rest-args
     (if (null? rest-args)
	 1
	 (let loop ((rest (cdr rest-args))
		    (sum (car rest-args)))
	   (if (null? rest)
	       sum
	       (loop (cdr rest)
		     (dylan-call dylan:binary* sum (car rest)))))))))

(define dylan:-
  (dylan::function->method
   at-least-one-number
   (lambda (num . rest-num)
     (if (null? rest-num)
	 (- num)
	 (let loop ((rest rest-num)
		    (sum num))
	   (if (null? rest)
	       sum
	       (loop (cdr rest)
		     (dylan-call dylan:binary- sum (car rest)))))))))

(define dylan:/
  (dylan::function->method
   at-least-one-number
   (lambda (num . rest-num)
     (if (null? rest-num)
	 (/ num)
	 (let loop ((rest rest-num)
		    (sum num))
	   (if (null? rest)
	       sum
	       (loop (cdr rest)
		     (dylan-call dylan:binary/ sum (car rest)))))))))

(define dylan:identity (dylan::function->method one-object (lambda (x) x)))

(define dylan:=
  (dylan::function->method
   at-least-two-objects
   (lambda (obj1 obj2 . rest-objs)
     (if (dylan-call dylan:binary= obj1 obj2)
	 (let loop ((rest-objs rest-objs))
	   (if (null? rest-objs)
	       #T
	       (if (dylan-call dylan:binary= obj1 (car rest-objs))
		   (loop (cdr rest-objs))
		   #F)))
	 #F))))

(define dylan:/=
  (dylan::function->method
   two-objects
   (lambda (obj1 obj2) (not (dylan-call dylan:binary= obj1 obj2)))))

(define dylan:<
  (dylan::function->method
   at-least-two-objects
   (lambda (obj1 obj2 . rest-objs)
     (if (dylan-call dylan:binary< obj1 obj2)
	 (let loop ((rest-objs rest-objs)
		    (prev-obj obj2))
	   (if (null? rest-objs)
	       #T
	       (if (dylan-call dylan:binary< prev-obj (car rest-objs))
		   (loop (cdr rest-objs) (car rest-objs))
		   #F)))
	 #F))))

(define dylan:>=
  (dylan::function->method
   at-least-two-objects
   (lambda (obj1 obj2 . rest-objs)
     (if (not (dylan-call dylan:binary< obj1 obj2))
	 (let loop ((rest-objs rest-objs)
		    (prev-obj obj2))
	   (if (null? rest-objs)
	       #T
	       (if (not (dylan-call dylan:binary< prev-obj (car rest-objs)))
		   (loop (cdr rest-objs) (car rest-objs))
		   #F)))
	 #F))))

(define dylan:>
  (dylan::function->method
   at-least-two-objects
   (lambda (obj1 obj2 . rest-objs)
     (if (not (or (dylan-call dylan:binary< obj1 obj2)
		  (dylan-call dylan:binary= obj1 obj2)))
	 (let loop ((rest-objs rest-objs)
		    (prev-obj obj2))
	   (if (null? rest-objs)
	       #T
	       (if (not (or (dylan-call dylan:binary< prev-obj (car rest-objs))
			    (dylan-call dylan:binary=
					prev-obj (car rest-objs))))
		   (loop (cdr rest-objs) (car rest-objs))
		   #F)))
	 #F))))

(define dylan:<=
  (dylan::function->method
   at-least-two-objects
   (lambda (obj1 obj2 . rest-objs)
     (if (or (dylan-call dylan:binary< obj1 obj2)
	     (dylan-call dylan:binary= obj1 obj2))
	 (let loop ((rest-objs rest-objs)
		    (prev-obj obj2))
	   (if (null? rest-objs)
	       #T
	       (if (or (dylan-call dylan:binary< prev-obj (car rest-objs))
		       (dylan-call dylan:binary= prev-obj (car rest-objs)))
		   (loop (cdr rest-objs) (car rest-objs))
		   #F)))
	 #F))))

(define dylan:always
  (dylan::function->method
   one-object
   (lambda (obj)
     (lambda args
       args				; Ignored
       obj))))

(define dylan:id?
  (dylan::function->method
   at-least-two-objects
   (lambda (obj1 . others)
     (let loop ((rest others))
       (or (null? rest)
	   (and (eq? obj1 (car rest))
		(loop (cdr rest))))))))


(define dylan:min
  (dylan::function->method
   at-least-one-real
   (lambda (real1 . others)
     (let loop ((rest others)
		(min-so-far real1))
       (if (null? rest)
	   min-so-far
	   (loop (cdr rest)
		 (if (dylan-call dylan:binary< real1 (car rest))
		     real1
		     (car rest))))))))

(define dylan:max
  (dylan::function->method
   at-least-one-real
   (lambda (real1 . others)
     (let loop ((rest others)
		(max-so-far real1))
       (if (null? rest)
	   max-so-far
	   (loop (cdr rest)
		 (if (not (dylan-call dylan:binary< real1 (car rest)))
		     real1
		     (car rest))))))))


(define (reduce l fn init-value)
  (if (null? l)
      init-value
      (reduce (cdr l) fn (fn (car l) init-value))))

(define dylan:lcm
  (dylan::function->method
   only-rest-args
   (lambda args
     (reduce args (lambda (x) (dylan-call dylan:binary-lcm x)) 1))))

(define dylan:gcd
  (dylan::function->method
   only-rest-args
   (lambda args
     (reduce args (lambda (x) (dylan-call dylan:binary-gcd x)) 0))))

;;; Special functions

(define (dylan:values multiple-values? next-method . values)
  next-method				; Ignore
  (if (not multiple-values?)
      (if (null? values) #F (car values))
      (let ((last-loc (- (vector-length multiple-values?) 1)))
	(do ((index 0 (+ index 1))
	     (rest values (cdr rest)))
	    ((or (null? rest) (= index last-loc))
	     (vector-set! multiple-values? last-loc rest)
	     multiple-values?)		; Return vector itself. See BIND
	  (vector-set! multiple-values? index (car rest))))))

(define dylan:not (make-dylan-callable not 1))

;;; Generic functions

(define (dylan::generic-fn name param-list scheme-operation)
  ;; Scheme-Operation can be #F, meaning "no methods initially available"
  (let ((generic-function
	 (dylan::create-generic-function
	  name
	  (param-list.nrequired param-list)
	  (param-list.keys param-list)
	  (param-list.rest? param-list))))
    (if scheme-operation
	(add-method generic-function
		    (dylan::function->method param-list scheme-operation)))
    generic-function))

(define (dylan::make-<object> class . rest)
  (define (gather-from-slots slot-fn)
    (let loop ((keywords '())
	       (keys (map slot-fn (vector->list (class.slots class)))))
      (if (null? keys)
	  keywords
	  (loop (if (car keys) (cons (car keys) keywords) keywords)
		(cdr keys)))))
  (dylan::keyword-validate #F rest #T)
  (let ((instance-data (make-vector
			(class.instance-data-size class)))
	(slots (class.slots class)))
    (let ((required (gather-from-slots slot.required-init-keyword)))
      (for-each
       (lambda (k)
	 (dylan::find-keyword
	  rest k (lambda ()
		   (dylan-call dylan:error
			       "make -- missing required keyword" k rest))))
       required)
      (vector-iterate slots
		      (lambda (i slot)
			i		; unused
			(initialize-slot! slot rest instance-data '(INSTANCE))))
      (let ((result (make-instance class #F instance-data)))
	(add-to-population! (class.instances class) result)
	(dylan-apply dylan:initialize result rest)
	result))))

(define dylan:make
  (dylan::generic-fn 'make
   (make-param-list `((CLASS ,<class>)) #F #F #T)
   dylan::make-<object>))

(define dylan:initialize
  (dylan::generic-fn 'initialize
    (make-param-list `((OBJECT ,<object>)) #F #F #T)
    (lambda (instance . rest) rest instance)))

(define dylan:slot-initialized?
  (dylan::generic-fn 'slot-initialized?
   (make-param-list `((INSTANCE ,<object>) (GETTER ,<generic-function>))
		    #F #F #F)
   (lambda (instance getter)
     (let* ((class (instance.class instance))
	    (slots (class.slots class))
	    (the-slot (same-slot-getter-in-slot-vector->slot getter slots)))
       (if (not the-slot)
	   (dylan-call dylan:error
		       "slot-initialized? -- no such slot"
		       instance getter class))
       (not
	(eq? *the-uninitialized-slot-value*
	     (case (slot.allocation the-slot)
	       ((VIRTUAL CONSTANT) 'initialized)
	       ((CLASS) (let ((data-loc (slot.data-location the-slot)))
			  (vector-ref (class.class-data (car data-loc))
				      (cdr data-loc))))
		((EACH-SUBCLASS) (vector-ref (class.class-data class)
					     (slot.data-location the-slot)))
		((INSTANCE) (vector-ref (instance.data instance)
					(slot.data-location the-slot)))
		(else (dylan-call dylan:error
				  "slot-initialized? -- bad allocation"
				  (slot.allocation the-slot)
				  instance getter class)))))))))

;;; Arithmetic

(define dylan:odd? (dylan::generic-fn 'odd? one-integer odd?))
(define dylan:even? (dylan::generic-fn 'even? one-integer even?))
(define dylan:zero? (dylan::generic-fn 'zero? one-number zero?))
(define dylan:positive? (dylan::generic-fn 'positive? one-number positive?))
(define dylan:negative? (dylan::generic-fn 'negative? one-real negative?))
(define dylan:integral? (dylan::generic-fn 'integral? one-number integer?))

(define dylan:abs (dylan::generic-fn 'abs one-number abs))
(define dylan:sin (dylan::generic-fn 'sin one-number sin))
(define dylan:cos (dylan::generic-fn 'cos one-number cos))
(define dylan:tan (dylan::generic-fn 'tan one-number tan))
(define dylan:asin (dylan::generic-fn 'asin one-number asin))
(define dylan:acos (dylan::generic-fn 'acos one-number acos))
(define dylan:atan (dylan::generic-fn 'atan one-number atan))
(define dylan:atan2 (dylan::generic-fn 'atan2 two-numbers atan))
(define dylan:exp (dylan::generic-fn 'exp one-number exp))
(define dylan:log (dylan::generic-fn 'log one-number log))
(define dylan:expt (dylan::generic-fn 'expt one-number expt))
(define dylan:sqrt (dylan::generic-fn 'sqrt one-number sqrt))

(define dylan:modulo
  (dylan::generic-fn 'modulo two-reals
    (lambda (r1 r2)
      (let* ((multiple-values (vector #F #F '()))
	     (floor (dylan-mv-call dylan:floor/ multiple-values r1 r2)))
	floor				; Ignored
	(vector-ref multiple-values 0)))))

(define dylan:remainder
  (dylan::generic-fn 'remainder
   two-reals
   (lambda (real1 real2)
     (- real1 (* real2 (truncate (/ real1 real2)))))))

(define dylan:unary- (dylan::generic-fn 'unary- one-number -))
(define dylan:unary/ (dylan::generic-fn 'unary/ one-number /))

(define dylan:binary+ (dylan::generic-fn 'binary+ two-numbers +))
(define dylan:binary* (dylan::generic-fn 'binary* two-numbers *))
(define dylan:binary- (dylan::generic-fn 'binary- two-numbers -))
(define dylan:binary/ (dylan::generic-fn 'binary/ two-numbers /))

;;; Class stuff

(define dylan:all-superclasses
  (dylan::generic-fn 'all-superclasses
   one-class
   (lambda (class)
     (map-over-all-superclasses! class (lambda x x)))))

(define dylan:direct-superclasses
  (dylan::generic-fn 'direct-superclasses one-class class.superclasses))

(define dylan:direct-subclasses
  (dylan::generic-fn 'direct-subclasses one-class
    (lambda (class)
      (population->list (class.subclasses class)))))


(define dylan:instance?
  (dylan::generic-fn 'instance?
   (make-param-list `((OBJECT ,<object>) (CLASS ,<class>)) #F #F #F)
   (lambda (obj class)
     (subclass? (get-type obj) class))))

(define dylan:subclass?
  (dylan::generic-fn 'subclass?
   (make-param-list `((CLASS-1 ,<class>) (CLASS-2 ,<class>)) #F #F #F)
   subclass?))

(define dylan:object-class
  (dylan::generic-fn 'object-class one-object get-type))

(define dylan:slot-descriptors
  (dylan::generic-fn 'slot-descriptors one-class class.slots))

(define dylan:slot-getter
  (dylan::generic-fn 'slot-getter one-slot slot.getter))
(define dylan:slot-setter
  (dylan::generic-fn 'slot-setter one-slot slot.setter))
(define dylan:slot-type
  (dylan::generic-fn 'slot-type one-slot slot.type))
(define dylan:slot-allocation
  (dylan::generic-fn 'slot-allocation one-slot slot.allocation))

(define dylan:binary< (dylan::generic-fn 'binary< two-numbers <))

(define dylan:binary=
  ;; Use eq? if object not same class.
  (dylan::generic-fn 'binary= two-objects eq?))

(add-method dylan:binary= (dylan::function->method two-numbers =))


(define dylan:as-lowercase
  ;; Takes <character>s or <string>s.
  (dylan::generic-fn 'as-lowercase one-object #F))

(add-method
 dylan:as-lowercase
 (dylan::function->method
  one-char
  (lambda (char) (char-downcase char))))

(define dylan:as-uppercase
  ;; Takes <character>s or <string>s.
  (dylan::generic-fn 'as-uppercase one-object #F))

(add-method
 dylan:as-uppercase
 (dylan::function->method
  one-char
  (lambda (char) (char-upcase char))))


(define dylan:=hash (dylan::generic-fn '=hash one-integer (lambda (x) x)))

(add-method dylan:=hash			; ***** TEMP: for debugging tables
  (dylan::function->method
   one-real
   (lambda (real)
     (dylan-call dylan:as <integer> (dylan-call dylan:floor real)))))

(define dylan:floor (dylan::generic-fn 'floor one-real #F))

(add-method
 dylan:floor
 (dylan::dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    next-method
    (dylan-mv-call dylan:values multiple-values
		   (floor num) (- num (floor num))))))

(define dylan:ceiling (dylan::generic-fn 'ceiling one-real #F))

(add-method
 dylan:ceiling
 (dylan::dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    next-method
    (dylan-mv-call dylan:values multiple-values
		   (ceiling num) (- num (ceiling num))))))

(define dylan:truncate (dylan::generic-fn 'truncate one-real #F))

(add-method
 dylan:truncate
 (dylan::dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    next-method
    (dylan-mv-call dylan:values multiple-values
		   (truncate num) (- num (truncate num))))))

(define dylan:round (dylan::generic-fn 'round one-real #F))

(add-method
 dylan:round
 (dylan::dylan-callable->method
  one-real
  (lambda (multiple-values next-method num)
    next-method
    (dylan-mv-call dylan:values multiple-values
		   (round num) (- num (round num))))))

(define dylan:floor/ (dylan::generic-fn 'floor/ two-reals #F))

(add-method
 dylan:floor/
 (dylan::dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    next-method
    (let ((floor-div-result (floor (/ real1 real2))))
      (dylan-mv-call dylan:values multiple-values
		     floor-div-result
		     (- real1 (* real2 floor-div-result)))))))

(define dylan:ceiling/ (dylan::generic-fn 'ceiling/ two-reals #F))

(add-method
 dylan:ceiling/
 (dylan::dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    next-method
    (let ((ceiling-div-result (ceiling (/ real1 real2))))
      (dylan-mv-call dylan:values multiple-values
		     ceiling-div-result
		     (- real1 (* real2 ceiling-div-result)))))))

(define dylan:truncate/ (dylan::generic-fn 'truncate/ two-reals #F))

(add-method
 dylan:truncate/
 (dylan::dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    next-method
    (let ((truncate-div-result (truncate (/ real1 real2))))
      (dylan-mv-call dylan:values multiple-values
		     truncate-div-result
		     (- real1 (* real2 truncate-div-result)))))))

(define dylan:round/ (dylan::generic-fn 'round/ two-reals #F))

(add-method
 dylan:round/
 (dylan::dylan-callable->method
  two-reals
  (lambda (multiple-values next-method real1 real2)
    next-method
    (let ((round-div-result (round (/ real1 real2))))
      (dylan-mv-call dylan:values multiple-values
		     round-div-result
		     (- real1 (* real2 round-div-result)))))))

(define dylan:add-method
  (let* ((params
	  (make-param-list
	   `((GENERIC-FUNCTION ,<generic-function>) (METHOD ,<method>))
	   #F #F #F))
	 (generic-function (dylan::generic-fn 'add-method params #F)))
    (add-method
     generic-function
     (dylan::make-method
      params
      (lambda (multiple-values next-method generic-function method)
	next-method			; Ignored
	(add-method generic-function method
		    (lambda (new old)
		      (dylan-mv-call dylan:values multiple-values new old))))))
    generic-function))

(define dylan:shallow-copy
  (dylan::generic-fn 'shallow-copy
   one-object
   (lambda (obj)
     (dylan-call dylan:error
		 "shallow-copy -- not specialized for this object type" obj))))

(define dylan:binary-gcd
  (dylan::generic-fn 'binary-gcd two-integers gcd))
(define dylan:binary-lcm
  (dylan::generic-fn 'binary-lcm two-integers lcm))

(define dylan:denominator
  (dylan::generic-fn 'denominator one-real denominator))
(define dylan:numerator
  (dylan::generic-fn 'numerator one-real numerator))

(define dylan:angle
  (dylan::generic-fn 'angle one-number angle))
(define dylan:magnitude
  (dylan::generic-fn 'magnitude one-number magnitude))
(define dylan:imag-part
  (dylan::generic-fn 'imag-part one-number imag-part))
(define dylan:real-part
  (dylan::generic-fn 'real-part one-number real-part))
(define dylan:rationalize
  (dylan::generic-fn 'rationalize one-number rationalize))

(define dylan:init-function
  (dylan::generic-fn 'init-function one-slot slot.init-function))

(define dylan:init-keyword
  (dylan::generic-fn 'init-keyword one-slot slot.init-keyword))

(define dylan:init-value
  (dylan::generic-fn 'init-value one-slot #F))

(add-method
 dylan:init-value
 (dylan::dylan-callable->method
  one-slot
  (lambda (multiple-values next-method slot)
    next-method
    (if (slot.has-initial-value? slot)
	(dylan-mv-call dylan:values multiple-values
		       (slot.init-value slot) #T)
	(dylan-mv-call dylan:values multiple-values #F #F)))))

(define dylan:applicable-method?
  (dylan::generic-fn 'applicable-method?
    (make-param-list `((FN ,<function>)) #F #T #F)
    (lambda (fn . args)
      (cond
       ((dylan::generic-function? fn)
	(any? (lambda (method)
		(method-applicable? method args))
	      (generic-function.methods fn)))
       ((dylan::method? fn)
	(method-applicable? fn args))
       (else #F)))))

(define dylan:apply
  (dylan::generic-fn 'apply (make-param-list `((FN ,<function>)) #F #T #F) #F))

(add-method
 dylan:apply
 (dylan::dylan-callable->method
  (make-param-list `((FN ,<function>)) #F #T #F)
  (lambda (multiple-values next-method fn . args)
    (dylan-full-apply fn multiple-values next-method
		      (split-last
		       args
		       (lambda (early end)
			 (append early
				 (if (null? end)
				     '()
				     (iterate->list (lambda (x) x)
						    (car end))))))))))

(define dylan:as
  (dylan::generic-fn 'as
    (make-param-list `((CLASS ,<class>) (OBJECT ,<object>)) #F #F #F)
    (lambda (class obj)
      (if (dylan-call dylan:instance? obj class)
	  obj
	  (dylan-call dylan:error
		      "as -- not specialized for this class type"
		      class obj)))))
(begin
  ;; integer <-> character
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <integer>))
			(OBJECT ,<character>)) #F #F #F)
     (lambda (class object) class (char->integer object))))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <character>))
			(OBJECT ,<integer>)) #F #F #F)
     (lambda (class object) class (integer->char object))))
  ;; number conversions
  (define (no-change class object) class object)
  (define (->exact class object) class (inexact->exact object))
  (define (->inexact class object) class (exact->inexact object))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <number>))
			(OBJECT ,<number>)) #F #F #F)
     no-change))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <complex>))
			(OBJECT ,<number>)) #F #F #F)
     no-change))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <real>))
			(OBJECT ,<real>)) #F #F #F)
     no-change))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <rectangular-complex>))
			(OBJECT ,<number>)) #F #F #F)
     no-change))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <rational>))
			(OBJECT ,<number>)) #F #F #F)
     ->exact))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <float>))
			(OBJECT ,<number>)) #F #F #F)
     ->inexact))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <integer>))
			(OBJECT ,<number>)) #F #F #F)
     ->exact))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <ratio>))
			(OBJECT ,<number>)) #F #F #F)
     ->exact))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <single-float>))
			(OBJECT ,<number>)) #F #F #F)
     ->inexact))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <double-float>))
			(OBJECT ,<number>)) #F #F #F)
     ->inexact))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <extended-float>))
			(OBJECT ,<number>)) #F #F #F)
     ->inexact))
  ; symbols, strings, and keywords
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <string>))
			(SYMBOL ,<symbol>)) #F #F #F)
     (lambda (class symbol)
       class				; Unused
       (symbol->string symbol))))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <string>))
			(KEYWORD ,<keyword>)) #F #F #F)
     (lambda (class keyword)
       class				; Unused
       (let ((string (symbol->string keyword)))
	 (substring string 0 (- (string-length string) 1))))))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <symbol>))
			(STRING ,<string>)) #F #F #F)
     (lambda (class string)
       class				; Unused
       (new-name "" string ""))))
  (add-method dylan:as
    (dylan::function->method
     (make-param-list `((CLASS ,(dylan::make-singleton <keyword>))
			(STRING ,<string>)) #F #F #F)
     (lambda (class string)
       class				; Unused
       (new-name "" string ":"))))
  )

(define dylan:complement
  (dylan::function->method
    one-function
    (lambda (fn)
      (make-dylan-callable
       (lambda args
	 (not (dylan-apply fn args)))))))

(define dylan:compose
  (dylan::function->method
   at-least-one-function
   (lambda (fn . rest-fns)
     (if (null? rest-fns)
	 fn
	 (lambda (multiple-values next-method . args)
	   (define (compose fn rest-fns)
	     (if (null? rest-fns)
		 (dylan-apply fn args)
		 (dylan-call fn (compose (car rest-fns) (cdr rest-fns)))))
	   next-method			; Not used
	   (dylan-mv-call fn multiple-values
			  (compose (car rest-fns) (cdr rest-fns))))))))

(define dylan:disjoin
  (dylan::function->method
   at-least-one-function
   (lambda (fn . rest-fns)
     (if (null? rest-fns)
	 fn
	 (lambda (multiple-values next-method . args)
	   next-method
	   (let loop ((fn fn)
		      (rest-fns rest-fns))
	     (if (null? rest-fns)
		 (dylan-mv-apply fn multiple-values args)
		 (let ((value (dylan-apply fn args)))
		   (if value
		       value
		       (loop (car rest-fns) (cdr rest-fns)))))))))))

(define dylan:conjoin
  (dylan::function->method
   at-least-one-function
   (lambda (fn . rest-fns)
     (if (null? rest-fns)
	 fn
	 (lambda (multiple-values next-method . args)
	   next-method
	   (let loop ((fn fn)
		      (rest-fns rest-fns))
	     (if (null? rest-fns)
		 (dylan-mv-apply fn multiple-values args)
		 (if (dylan-apply fn args)
		     (loop (car rest-fns) (cdr rest-fns))
		     #F))))))))

(define dylan:curry
  (dylan::function->method
   function-and-arguments
   (lambda (fn . curried-args)
     (lambda (multiple-values next-method . args)
       next-method
       (dylan-mv-apply fn multiple-values (append curried-args args))))))

(define dylan:rcurry
 (dylan::function->method
  function-and-arguments
  (lambda (fn . curried-args)
    (lambda (multiple-values next-method . args)
      next-method
      (dylan-mv-apply fn multiple-values (append args curried-args))))))
