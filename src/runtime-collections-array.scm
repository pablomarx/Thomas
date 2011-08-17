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

; $Id: runtime-collections-array.scm,v 1.17 1992/09/08 11:15:05 birkholz Exp $

;;;;; RUNTIME-COLLECTIONS-ARRAY.SCM

;;;;; This file contains all the specializations for array type

(add-method dylan:shallow-copy
  (dylan::function->method
    (make-param-list `((ARRAY ,<array>)) #F #F #F)
    (lambda (array)
      (let* ((dimensions (dylan-call dylan:dimensions array))
	     (new-array
	      (dylan-call dylan:make <array> 'dimensions: dimensions))
	     (key-sequence (dylan-call dylan:key-sequence array)))
	(do ((state (dylan-call dylan:initial-state key-sequence)
		    (dylan-call dylan:next-state key-sequence state)))
	    ((not state)
	     (dylan-call dylan:as
			 (dylan-call dylan:class-for-copy array)
			 new-array))
	  (let ((key (dylan-call dylan:current-element key-sequence state)))
	    (dylan-call dylan:setter/element/
			new-array
			key
			(dylan-call dylan:element array key))))))))


(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <array>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class
     (if (dylan-call dylan:instance? collection <array>)
	 collection
	 (let* ((size (dylan-call dylan:size collection))
		(new-array
		 (dylan-call dylan:make <array> 'dimensions: (list size)))
		(vector-value (dylan-call dylan:get-array-value new-array)))
	   (do ((state (dylan-call dylan:initial-state collection)
		       (dylan-call dylan:next-state collection state))
		(index 0 (+ index 1)))
	       ((not state) new-array)
	     (vector-set! vector-value index
			  (dylan-call
			   dylan:current-element collection state))))))))


;;;
;;; ARRAY SPECIALIZED MAKE
;;;

;; All subclasses of ARRAY have slots for the data ("value") and a
;; list of dimensions ("dimensions").

(define dylan:get-array-value "define dylan:get-array-value")
(define dylan:get-array-dimensions "define dylan:get-array-dimensions")
(define dylan:set-array-value! "define dylan:set-array-value!")
(define dylan:set-array-dimensions! "define dylan:set-array-dimensions!")
(create-private-slot <array> <simple-object-vector>
		     "internal-array-value"
 (lambda (set get)
   (set! dylan:set-array-value! set)
   (set! dylan:get-array-value get)))
(create-private-slot <array> <list>
		     "internal-array-dimensions"
 (lambda (set get)
   (set! dylan:set-array-dimensions! set)
   (set! dylan:get-array-dimensions get)))

;; These four generic operations must be specialized for the special
;; cases of <byte-string> and <simple-object-vector>

(add-method dylan:get-array-value
  (one-arg 'STRING <byte-string> (lambda (string) string)))
(add-method dylan:set-array-value!
  (dylan::function->method
   (make-param-list `((STRING ,<byte-string>) (VALUE ,<object>)) #F #F #F)
    (lambda (string value)
      (dylan-call dylan:error
		  "set-array-value! -- internal error on string"
		  string value))))
(add-method dylan:get-array-dimensions
  (one-arg 'STRING <byte-string>
    (lambda (string) (list (string-length string)))))
(add-method dylan:set-array-dimensions!
  (dylan::function->method
   (make-param-list `((STRING ,<byte-string>) (VALUE ,<object>)) #F #F #F)
    (lambda (string value)
      (dylan-call dylan:error
		  "set-array-dimensions! -- internal error on string"
		  string value))))

(add-method dylan:get-array-value
  (one-arg 'VECTOR <simple-object-vector>
    (lambda (vector) vector)))
(add-method dylan:set-array-value!
  (dylan::function->method
   (make-param-list `((VECTOR ,<simple-object-vector>)
		      (VALUE ,<object>)) #F #F #F)
    (lambda (vector value)
      (dylan-call dylan:error
		  "set-array-value! -- internal error on simple-object-vector"
		  vector value))))
(add-method dylan:get-array-dimensions
  (one-arg 'VECTOR <simple-object-vector>
    (lambda (vector) (list (vector-length vector)))))
(add-method dylan:set-array-dimensions!
  (dylan::function->method
   (make-param-list `((VECTOR ,<simple-object-vector>)
		      (VALUE ,<object>)) #F #F #F)
    (lambda (vector value)
      (dylan-call
       dylan:error
       "set-array-dimensions! -- internal error on simple-object-vector"
       vector value))))

(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((ARRAY ,(dylan::make-singleton <array>)))
		   #F #F '(dimensions: fill:))
  (lambda (multiple-values next-method class . rest)
    (define (make-multi-dimensional-array dimensions fill)
      (if (null? dimensions)
	  (dylan-call dylan:error
		      "make -- 0-dimensional arrays not allowed"))
      (let ((result (make-vector (car dimensions) fill)))
	(if (null? (cdr dimensions))
	    result
	    (do ((n 0 (+ n 1)))
		((= n (car dimensions)) result)
	      (vector-set! result n
			   (make-multi-dimensional-array
			    (cdr dimensions)
			    fill))))))
    multiple-values			; Ignored
    class				; Ignored
    (dylan::keyword-validate next-method rest '(dimensions: fill:))
    (let ((dimensions
	   (dylan::find-keyword rest 'dimensions:
				(lambda ()
				  (dylan-call dylan:error
					      "make -- array needs dimensions"
					      class rest))))
	  (fill-value (dylan::find-keyword rest 'fill: (lambda () #F))))
      (if (not (subclass? (get-type dimensions) <sequence>))
	  (dylan-call dylan:error
		      "make -- array dimensions not a sequence"
		      class dimensions))
      (let ((instance (dylan::make-<object> <array>))
	    (dim-list
	     (iterate->list
	      (lambda (elem)
		(if (not (and (integer? elem)
			      (positive? elem)))
		    (dylan-call
		     dylan:error
		     "make -- dimension elements not all positive integers"
		     class dimensions elem)
		    elem))
	      dimensions)))
	(dylan-call dylan:set-array-value!
		    instance
		    (make-multi-dimensional-array dim-list fill-value))
	(dylan-call dylan:set-array-dimensions! instance dim-list)
	instance)))))

;;;;
;;;; Operations on Arrays (page 113 )
;;;;
(define dylan:aref
  (dylan::generic-fn 'aref
    (make-param-list `((ARRAY ,<array>)) #F #T #F)
    (lambda (array-instance . init-indices)
      (if (null? init-indices)
	  (dylan-call dylan:error
		      "aref -- no indices given" array-instance))
      (let loop ((array (dylan-call dylan:get-array-value array-instance))
		 (indices init-indices))
	(if (vector? array)
	    (let ((size (vector-length array))
		  (index (car indices)))
	      (if (>= index size)
		  (dylan-call dylan:error
			      "aref -- subscript out of range"
			      array-instance init-indices array index))
	      (if (null? (cdr indices))
		  (vector-ref array index)
		  (loop (vector-ref array index) (cdr indices))))
	    (dylan-call dylan:error
			"aref -- too many subscripts"
			array-instance init-indices indices))))))

(define dylan:setter/aref/
  (dylan::generic-fn 'aref
    (make-param-list `((ARRAY ,<array>)) #F #T #F)
    (lambda (array-instance . indices-and-new-value)
      (if (null? indices-and-new-value)
	  (dylan-call dylan:error
		      "(setter aref) -- no indices and new-value given"
		      array-instance))
      (let ((new-value (list-ref indices-and-new-value
				 (- (length indices-and-new-value) 1)))
	    (indices (but-last indices-and-new-value)))
	(if (not (pair? indices))
	    (dylan-call dylan:error
			"(setter aref) -- no indices given"
			array-instance indices-and-new-value))
	(let loop ((array (dylan-call dylan:get-array-value array-instance))
		   (indices indices))
	  (if (vector? array)
	      (let ((size (vector-length array))
		    (index (car indices)))
		(if (>= index size)
		    (dylan-call dylan:error
				"(setter aref) -- subscript out of range"
				array-instance indices-and-new-value
				array index))
		(if (null? (cdr indices))
		    (if (vector? (vector-ref array index))
			(dylan-call
			 dylan:error
			 "(setter aref) -- indices need to point to an element"
			 array-instance indices-and-new-value array index)
			(begin
			  (vector-set! array index new-value)
			  new-value))
		    (loop (vector-ref array index) (cdr indices))))
	      (dylan-call dylan:error
			  "(setter aref) -- too many subscripts"
			  array-instance indices-and-new-value indices)))))))

(define dylan:dimensions
  (dylan::generic-fn 'dimensions
    (make-param-list `((ARRAY ,<array>)) #F #F #F)
    (lambda (array)
      (dylan-call dylan:get-array-dimensions array))))

(add-method
 dylan:element
 (dylan::dylan-callable->method
  (make-param-list `((ARRAY ,<array>) (INDEX ,<sequence>)) #F #F '(default:))
  (lambda (multiple-values next-method array-instance init-indices . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(default:))
    (let* ((default (dylan::find-keyword rest 'default: (lambda () #F)))
	   (error-report (lambda args (if default
					  default
					  (apply dylan:error args)))))
      (if (dylan-call dylan:empty? init-indices)
	  (error-report "element -- no indices given" array-instance)
	  (let loop ((array
		      (dylan-call dylan:get-array-value array-instance))
		     (index-state
		      (dylan-call dylan:initial-state init-indices)))
	    (let ((index (dylan-call dylan:current-element
				     init-indices index-state)))
	      (if (vector? array)
		  (cond ((>= index (vector-length array))
			 (error-report "element -- subscript out of range"
				       array-instance init-indices array index))
			((not
			  (dylan-call dylan:next-state
				      init-indices index-state))
			 (vector-ref array index))
			(else (loop (vector-ref array index)
				    (dylan-call dylan:next-state
						init-indices index-state))))
		  (error-report "element -- too many subscripts"
				array-instance init-indices index)))))))))

(add-method dylan:current-key
  (dylan::function->method
   (make-param-list `((ARRAY ,<array>) (STATE ,<object>)) #F #F #F)
   (lambda (array state)
     array				; Ignored
     state)))

;;;
;;; Mutable Collections
;;;

(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((ARRAY ,<array>) (STATE ,<object>) (new-value ,<object>)) #F #F #F)
    (lambda (array state new-value)
      array				; Ignored
      (let loop ((vectors (dylan-call dylan:get-array-value array))
		 (state (vector->list state)))
	(if (= (length state) 1)
	    (begin
	      (vector-set! vectors (car state) new-value)
	      new-value)
	    (loop (vector-ref vectors (car state))
		  (cdr state)))))))
