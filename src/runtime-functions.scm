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

; $Id: runtime-functions.scm,v 1.25 1992/09/11 21:23:40 jmiller Exp $

;;;; Miscellaneous Dylan Functions

;;;
;;; Specialized MAKE for some built in classes
;;;
(add-method dylan:make
  (dylan::function->method
    (make-param-list `((CLASS ,(dylan::make-singleton <generic-function>)))
		     #F #F '(debug-name: required: rest?: key?:))
    (lambda (class . rest)
      class				; ignored
      (let ((name (dylan::find-keyword rest 'debug-name:
				       (lambda ()
					 "Anonymous Generic Function")))
	    (nrequired
	     (dylan::find-keyword
	      rest 'required:
	      (lambda ()
		(dylan-call dylan:error "(make (singleton <generic-function>)) -- required: not supplied" class rest))))
	    (rest? (dylan::find-keyword rest 'rest?: (lambda () #F)))
	    (keys (dylan::find-keyword rest 'key?: (lambda () #F))))
	(dylan::create-generic-function name nrequired keys rest?)))))

(add-method dylan:make
  (dylan::function->method
    (make-param-list
     `((CLASS ,(dylan::make-singleton <singleton>))) #F #F '(object:))
    (lambda (class . rest)
      class				; ignored
      (let ((object (dylan::find-keyword
		     rest 'object:
		     (lambda ()
		       (dylan-call dylan:error "(make (singleton <singleton>)) -- object not supplied" class rest)))))
	(dylan::make-singleton object)))))

(add-method dylan:make
  (dylan::function->method
    (make-param-list `((CLASS ,(dylan::make-singleton <complex>)))
		     #F #F '(real: imag: magnitude: angle:))
    (lambda (class . rest)
      class				; Ignored
      (let ((real (dylan::find-keyword rest 'real: (lambda () #F)))
	    (imag (dylan::find-keyword rest 'imag: (lambda () #F)))
	    (mag (dylan::find-keyword rest 'magnitude: (lambda () #F)))
	    (angle (dylan::find-keyword rest 'angle: (lambda () #F))))
	(cond ((and (not real) (not imag) (not mag) (not angle))
	       (dylan-call dylan:make-rectangular 0 0))
	      ((and real (not imag) (not mag) (not angle))
	       (dylan-call dylan:make-rectangular real 0))
	      ((and (not real) imag (not mag) (not angle))
	       (dylan-call dylan:make-rectangular 0 imag))
	      ((and real imag (not mag) (not angle))
	       (dylan-call dylan:make-rectangular real imag))
	      ((and (not real) (not imag) mag (not angle))
	       (dylan-call dylan:make-polar mag 0))
	      ((and (not real) (not imag) (not mag) angle)
	       (dylan-call dylan:make-polar 0 angle))
	      ((and (not real) (not imag) mag angle)
	       (dylan-call dylan:make-polar mag angle))
	      (else
	       (dylan-call dylan:error "(make (singleton <complex>)) -- invalid keyword combination" class rest)))))))


;;;
;;; Misc Generic Functions
;;;
(define dylan:find-method
  (dylan::generic-fn 'find-method
    (make-param-list `((GENERIC-FUNCTION ,<generic-function>)
		       (SPECIALIZERS ,<list>)) #F #F #F)
    find-method))

(define dylan:generic-function-methods
  (dylan::generic-fn 'generic-function-methods
    (make-param-list `((GENERIC-FUNCTION ,<generic-function>)) #F #F #F)
    (lambda (generic-function)
      (generic-function.methods generic-function))))

(define dylan:acosh
  (dylan::generic-fn 'acosh
    one-number
    (lambda (z)
      (log (+ z (* (+ z 1) (sqrt (/ (- z 1) (+ z 1)))))))))

(define dylan:asinh
  (dylan::generic-fn 'asinh
    one-number
    (lambda (z)
      (log (+ z (sqrt (+ 1 (* z z))))))))

(define dylan:atanh
  (dylan::generic-fn 'atanh
    one-number
    (lambda (z)
      (log (* (+ 1 z) (sqrt (/ 1 (- 1 (* z z)))))))))

(define dylan:cosh
  (dylan::generic-fn 'cosh
    one-number
    (lambda (z)
      (/ (+ (exp z) (exp (- z))) 2))))

(define dylan:sinh
  (dylan::generic-fn 'sinh
     one-number
     (lambda (z)
       (/ (- (exp z) (exp (- z))) 2))))

(define dylan:tanh
  (dylan::generic-fn 'tanh
     one-number
     (lambda (z)
       (/ (- (exp z) (exp (- z)))
	  (+ (exp z) (exp (- z)))))))

(define dylan:add-slot
  (dylan::generic-fn 'add-slot
   (make-param-list `((SLOT-OWNER ,<object>))
		    #F #F '(getter: setter: type: init-value:
				    init-function: init-keyword:
				    required-init-keyword: debug-name:
				    allocation:))
   add-slot))

(define dylan:freeze-methods
  (dylan::generic-fn 'freeze-methods
    (make-param-list `((GENERIC-FUNCTION ,<generic-function>)) #F #T #F)
    (lambda (generic-function . specializers)
      specializers			; Ignored for now
      generic-function)))

(define dylan:function-arguments
  (dylan::generic-fn 'function-arguments
		     (make-param-list `((FN ,<function>)) #F #F #F)
		     #F))
(begin
  (add-method dylan:function-arguments
    (dylan::dylan-callable->method
     (make-param-list `((METHOD ,<method>)) #F #F #F)
     (lambda (multiple-values next-method method)
       (dylan-full-call dylan:values multiple-values next-method
			(method.nrequired method)
			(method.rest? method)
			(method.keys method)))))
  (add-method dylan:function-arguments
    (dylan::dylan-callable->method
     (make-param-list `((GENERIC-FUNCTION ,<generic-function>)) #F #F #F)
     (lambda (multiple-values next-method generic-function)
       (dylan-full-call dylan:values multiple-values next-method
			(generic-function.nrequired generic-function)
			(generic-function.rest? generic-function)
			(generic-function.keys generic-function))))))

(define dylan:make-polar
  (dylan::generic-fn 'make-polar two-reals
    (lambda (magnitude angle)
      (if (zero? angle)
	  magnitude
	  (+ (* magnitude (cos angle))
	     (* magnitude (sin angle) (get-+i)))))))

(define dylan:make-rectangular
  (dylan::generic-fn 'make-rectangular two-reals
    (lambda (real imaginary)
      (if (zero? imaginary)
	  real
	  (+ real (* (get-+i) imaginary))))))

(define dylan:method-specializers
  (dylan::generic-fn 'method-specializers
    (make-param-list `((METHOD ,<method>)) #F #F #F)
    (lambda (method) (method.specializers method))))

(define dylan:singleton
  (dylan::generic-fn 'singleton one-object dylan::make-singleton))

(define dylan:sorted-applicable-methods
  (dylan::generic-fn 'sorted-applicable-methods
    (make-param-list `((GENERIC-FUNCTION ,<generic-function>)) #F #T #F)
    (lambda (generic-function . args)
      (sorted-applicable-methods
       (generic-function.methods generic-function)
       args))))

(define dylan:remove-method
  (dylan::generic-fn 'remove-method
    (make-param-list `((GENERIC-FUNCTION ,<generic-function>)
		       (METHOD ,<method>)) #F #F #F)
    (lambda (generic-function method)
      (if (generic-function.read-only? generic-function)
	  (dylan-call dylan:error
		      "remove-method -- generic function is read-only"
		      generic-function method))
      (delete-method! generic-function method))))

(define dylan:slot-descriptor
  ;; I'm ignoring the problem of making singletons of
  ;; Scheme-native immutable objects, and adding slots to the
  ;; singleton.
  (dylan::generic-fn 'slot-descriptor
    (make-param-list `((INSTANCE ,<object>)
		       (GETTER ,<generic-function>)) #F #F #F)
    (lambda (obj getter)
      (or (and (instance? obj)
	       (instance.singleton obj)
	       (same-slot-getter-in-slot-vector->slot
		getter
		(singleton.extra-slot-descriptors
		 (instance.singleton obj))))
	  (same-slot-getter-in-slot-vector->slot
	   getter
	   (class.slots (get-type obj)))))))

(define dylan:slot-value
  (dylan::generic-fn 'slot-value
    (make-param-list `((INSTANCE ,<object>)
		       (SLOT-DESC ,<slot-descriptor>)) #F #F #F)
    (lambda (instance slot-desc)
      (let ((allocation (slot.allocation slot-desc))
	    (loc (slot.data-location slot-desc)))
	(case allocation
	  ((INSTANCE) (vector-ref instance loc))
	  ((CLASS) (vector-ref (class.class-data (car loc)) (cdr loc)))
	  ((EACH-SUBCLASS) (vector-ref (get-type instance) loc))
	  ((VIRTUAL) ((slot.getter slot-desc) instance))
	  ((CONSTANT) loc)
	  (else (dylan-call dylan:error
			    "slot-value -- internal error"
			    slot-desc allocation)))))))

(define dylan:setter/slot-value/
  (dylan::generic-fn 'slot-value
    (make-param-list `((INSTANCE ,<object>)
		       (SLOT-DESC ,<slot-descriptor>)
		       (NEW-VALUE ,<object>)) #F #F #F)
    (lambda (instance slot-desc new-value)
      (if (not (match-specializer? new-value (slot.type slot-desc)))
	  (dylan-call dylan:error
		      "(setter slot-value) -- type conflict"
		      instance (slot.type slot-desc)
		      slot-desc new-value))
      (let ((allocation (slot.allocation slot-desc))
	    (loc (slot.data-location slot-desc)))
	(case allocation
	  ((INSTANCE) (vector-set! instance loc new-value))
	  ((CLASS)
	   (vector-set! (class.class-data (car loc)) (cdr loc) new-value))
	  ((EACH-SUBCLASS) (vector-set! (get-type instance) loc new-value))
	  ((VIRTUAL)
	   (let ((setter (slot.setter slot-desc)))
	     (if (not setter)
		 (dylan-call dylan:error
			     "(setter slot-value) -- no setter for virtual slot"
			     instance slot-desc new-value))
	     (setter instance new-value))
	   ((slot.setter slot-desc) instance))
	  ((CONSTANT)
	   (dylan-call dylan:error
		       "(setter slot-value) -- can't set a constant slot"
		       instance slot-desc new-value))
	  (else (dylan-call dylan:error
			    "(setter slot-value) -- internal error"
			    slot-desc allocation)))))))

(define dylan:make-read-only
  (dylan::generic-fn 'make-read-only one-class
    (lambda (class)
      (set-class.read-only?! class #T)
      class)))
(add-method dylan:make-read-only
 (dylan::function->method
  (make-param-list `((GENERIC-FUNCTION ,<generic-function>)) #F #F #F)
  (lambda (generic-function)
    (set-generic-function.read-only?! generic-function #T))))

(define dylan:seal
  (dylan::generic-fn 'seal one-class
    (lambda (class)
      (set-class.sealed?! class #T)
      class)))

(define dylan:remove-slot #F)

;;;;;;;;;;;; CRL additions

(define dylan:display (make-dylan-callable display 1))
(define dylan:newline (make-dylan-callable newline 0))
(define dylan:write-line (make-dylan-callable write-line 1))
(define dylan:print (make-dylan-callable write-line 1))
