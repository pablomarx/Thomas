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

; $Id: runtime-collections-vector.scm,v 1.13 1992/08/31 05:35:35 birkholz Exp $

;;;;; This file contains all the specializations for vector,
;;;;; simple-object-vector, and stretchy-vector types

(add-method dylan:shallow-copy		; To override <array> handling
  (dylan::function->method
    (make-param-list `((vector ,<vector>)) #F #F #F)
    (lambda (seq)
      (dylan-call dylan:copy-sequence seq))))

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <vector>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class				; Ignored
     (if (dylan-call dylan:instance? collection <vector>)
	 collection
	 (dylan-call dylan:as <simple-object-vector> collection)))))

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <simple-object-vector>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class				; Ignored
     (if (dylan-call dylan:instance? collection <simple-object-vector>)
	 collection
	 (let* ((size (dylan-call dylan:size collection))
		(new-vector (make-vector size)))
	   (do ((state (dylan-call dylan:initial-state collection)
		       (dylan-call dylan:next-state collection state))
		(index 0 (+ index 1)))
	       ((not state) new-vector)
	     (vector-set!
	      new-vector index
	      (dylan-call dylan:current-element collection state))))))))

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <stretchy-vector>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class				; Ignored
     (if (dylan-call dylan:instance? collection <stretchy-vector>)
	 collection
	 (let* ((size (dylan-call dylan:size collection))
		(new-s-vector
		 (dylan-call dylan:make <stretchy-vector> 'size: size))
		(vector-value (dylan-call dylan:get-array-value new-s-vector)))
	   (do ((state (dylan-call dylan:initial-state collection)
		       (dylan-call dylan:next-state collection state))
		(index 0 (+ index 1)))
	       ((not state) new-s-vector)
	     (vector-set!
	      vector-value index
	      (dylan-call dylan:current-element collection state))))))))

;;;
;;; VECTOR SPECIALIZED MAKE
;;; <vector> ... like the book says, this yields a
;;; <simple-object-vector>
;;;
(add-method dylan:make
  (dylan::function->method
   (make-param-list `((VECTOR ,(dylan::make-singleton <vector>)))
		    #F #F #T)
   (lambda (class . rest)
     class				; Ignored
     (dylan-apply dylan:make <simple-object-vector> rest))))



;;;
;;; SIMPLE-OBJECT-VECTOR SPECIALIZED MAKE
;;; <simple-object-vector> generates a Scheme vector
;;;
(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((SOV ,(dylan::make-singleton
			    <simple-object-vector>)))
		   #F #F '(size: fill:))
  (lambda (multiple-values next-method class . rest)
    multiple-values class		; Not used
    (dylan::keyword-validate next-method rest '(size: fill:))
    (let* ((size (dylan::find-keyword rest 'size: (lambda () 0)))
	   (fill (dylan::find-keyword rest 'fill: (lambda () #F))))
      (if (or (not (integer? size)) (negative? size))
	  (dylan-call
	   dylan:error
	   "(make (singleton <simple-object-vector>)) -- size: invalid" size))
      (make-vector size fill)))))



;;;
;;; STRETCHY-VECTOR SPECIALIZED MAKE
;;; <stretchy-vector> has one slot, for the vector itself.  I'm using
;;; the slot that is inherited from <array> for this purpose.
;;; Dimensions here is a list of one number.
;;;
(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((SV ,(dylan::make-singleton <stretchy-vector>)))
		   #F #F '(size: fill:))
  (lambda (multiple-values next-method class . rest)
    multiple-values class		; Not used
    (dylan::keyword-validate next-method rest '(size: fill:))
    (let* ((size (dylan::find-keyword rest 'size: (lambda () 0)))
	   (fill (dylan::find-keyword rest 'fill: (lambda () #F))))
      (if (or (not (integer? size)) (negative? size))
	  (dylan-call dylan:error
		      "(make (singleton <stretchy-vector>)) size: invalid"
		      size))
      (let ((instance (dylan::make-<object> <stretchy-vector>)))
	(dylan-call dylan:set-array-value!
		    instance (make-vector size fill))
	(dylan-call dylan:set-array-dimensions! instance (list size))
	instance)))))

;;;
;;; Functions for collections
;;;
(add-method dylan:size
  (one-arg 'SOV <vector>
    (lambda (vect) (vector-length (dylan-call dylan:get-array-value vect)))))

;;;
;;; Functions for sequences
;;;
(add-method dylan:add
 (dylan::function->method one-vector-and-an-object
   (lambda (vector new-element)
     (let ((new-vector (dylan-call dylan:make <vector>))
	   (size (car (dylan-call dylan:get-array-dimensions vector))))
       (dylan-call dylan:set-array-value!
		   new-vector
		   (list->vector (cons new-element (vector->list vector))))
       (dylan-call dylan:set-array-dimensions! new-vector (list (+ size 1)))
       new-vector))))

(add-method dylan:add
  (dylan::function->method one-simple-object-vector-and-an-object
    (lambda (sov new-element)
      (list->vector (cons new-element (vector->list sov))))))

(add-method dylan:add
  (dylan::function->method one-stretchy-vector-and-an-object
    (lambda (s-vector new-element)
      (let ((new-vector (dylan-call dylan:make <stretchy-vector>))
	    (size (car (dylan-call dylan:get-array-dimensions s-vector))))
	(dylan-call dylan:set-array-value!
		    new-vector
		    (list->vector (cons new-element (vector->list s-vector))))
	(dylan-call dylan:set-array-dimensions!
		    new-vector (list (+ size 1)))
	new-vector))))

(add-method dylan:add!
  (dylan::function->method
   one-stretchy-vector-and-an-object
   (lambda (s-vector new-element)
     (let* ((vector (dylan-call dylan:get-array-value s-vector))
	    (size (car (dylan-call dylan:get-array-dimensions s-vector)))
	    (new-vector (make-vector (+ size 1))))
       (do ((count 0 (+ count 1)))
	   ((= count size) 'done)
	 (vector-set! new-vector count (vector-ref vector count)))
       (vector-set! new-vector size new-element)
       (dylan-call dylan:set-array-value! s-vector new-vector)
       (dylan-call dylan:set-array-dimensions! s-vector (list (+ size 1)))
       s-vector))))

(add-method dylan:concatenate
  (dylan::function->method
   (make-param-list `((SOV ,<simple-object-vector>)) #F 'REST #F)
   (lambda (vector-1 . rest)
     (let loop ((result (vector->list vector-1))
		(rest-vectors (map (lambda (seq)
				     (dylan-call dylan:as
						 <simple-object-vector> seq))
				   rest)))
       (if (null? rest-vectors)
	   (list->vector result)
	   (loop (append result (vector->list (car rest-vectors)))
		 (cdr rest-vectors)))))))

(add-method dylan:concatenate
  (dylan::function->method
   (make-param-list `((VECTOR ,<vector>)) #F 'REST #F)
   (lambda (vector-1 . rest)
     (dylan-call dylan:apply dylan:concatenate vector-1 rest))))


(add-method
 dylan:remove!
 (dylan::dylan-callable->method
  (make-param-list `((STRETCHY-VECTOR ,<stretchy-vector>) (VALUE ,<object>))
		   #F #F '(test: count:))
  (lambda (multiple-values next-method s-vector value . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: count:))
    (let* ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?)))
	   (count (dylan::find-keyword
		   rest 'count:
		   (lambda ()
		     (car (dylan-call dylan:get-array-dimensions s-vector)))))
	   (old-vector (dylan-call dylan:get-array-value s-vector))
	   (new-vector (dylan-call dylan:remove
				   old-vector value
				   'test: test? 'count: count)))
      (dylan-call dylan:set-array-value! s-vector new-vector)
      (dylan-call dylan:set-array-dimensions!
		  s-vector (list (vector-length new-vector)))
      s-vector))))


(add-method
 dylan:remove-duplicates!
 (dylan::dylan-callable->method
  (make-param-list `((STRETCHY-VECTOR ,<stretchy-vector>)) #F #F '(test:))
  (lambda (multiple-values next-method s-vector . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?))))
      (let ((new-vector (dylan-call dylan:remove-duplicates
				    (dylan-call dylan:get-array-value
						s-vector)
				    'test: test?)))
	(dylan-call dylan:set-array-value! s-vector new-vector)
	(dylan-call dylan:set-array-dimensions!
		    s-vector (list (vector-length new-vector)))
	s-vector)))))

(add-method dylan:reverse
  (dylan::function->method one-simple-object-vector
    (lambda (vector-1)
      (let ((result (make-vector (vector-length vector-1))))
	(do ((from (- (vector-length vector-1) 1) (- from 1))
	     (to 0 (+ to 1)))
	    ((< from 0) result)
	  (vector-set! result to (vector-ref vector-1 from)))
	result))))


(add-method dylan:reverse
  (dylan::function->method one-stretchy-vector
    (lambda (s-vector)
      (let* ((vector-1 (dylan-call dylan:get-array-value s-vector))
	     (result (make-vector (vector-length vector-1)))
	     (result-s-vector (dylan-call dylan:make <stretchy-vector>)))
	(do ((from (- (vector-length vector-1) 1) (- from 1))
	     (to 0 (+ to 1)))
	    ((< from 0) result)
	  (vector-set! result to (vector-ref vector-1 from)))
	(dylan-call dylan:set-array-value! result-s-vector result)
	(dylan-call dylan:set-array-dimensions!
		    result-s-vector (list (vector-length result)))
	result-s-vector))))

(add-method dylan:reverse!
  (dylan::function->method one-simple-object-vector
    (lambda (vector-1)
      (do ((from (- (vector-length vector-1) 1) (- from 1))
	   (to 0 (+ to 1)))
	  ((<= from to) vector-1)
	(let ((to-element (vector-ref vector-1 to)))
	  (vector-set! vector-1 to (vector-ref vector-1 from))
	  (vector-set! vector-1 from to-element))))))


(add-method dylan:reverse!
  (dylan::function->method one-stretchy-vector
    (lambda (s-vector)
      (let ((vector-1 (dylan-call dylan:get-array-value s-vector)))
	(do ((from (- (vector-length vector-1) 1) (- from 1))
	     (to 0 (+ to 1)))
	    ((<= from to) s-vector)
	  (let ((to-element (vector-ref vector-1 to)))
	    (vector-set! vector-1 to (vector-ref vector-1 from))
	    (vector-set! vector-1 from to-element)))))))


(add-method
 dylan:sort!
 (dylan::dylan-callable->method
  (make-param-list `((STRETCHY-VECTOR ,<stretchy-vector>))
		   #F #F '(test: stable:))
  (lambda (multiple-values next-method s-vector . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: stable:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:<)))
	  (stable (dylan::find-keyword rest 'stable: (lambda () #F))))
      stable				; Ignored
      (dylan-call dylan:set-array-value!
		  s-vector
		  (dylan-call dylan:as
			      <simple-object-vector>
			      (sort (dylan-call dylan:as <pair> s-vector)
				    (lambda (x y)
				      (dylan-call test? x y)))))))))

(add-method dylan:first
  (dylan::function->method one-vector
    (lambda (vector)
      (if (= (vector-length vector) 0)
	  (dylan-call dylan:error "(first <vector>) -- vector is empty" vector)
	  (vector-ref vector 0)))))

(add-method dylan:second
  (dylan::function->method one-vector
    (lambda (vector)
      (if (< (vector-length vector) 2)
	  (dylan-call dylan:error
		      "(second <vector>) -- vector doesn't have 2 elements"
		      vector)
	  (vector-ref vector 1)))))

(add-method dylan:third
  (dylan::function->method one-vector
    (lambda (vector)
      (if (< (vector-length vector) 3 )
	  (dylan-call dylan:error
		      "(third <vector>) -- vector doesn't have 3 elements"
		      vector)
	  (vector-ref vector 2)))))

(add-method dylan:last
  (dylan::function->method one-vector
    (lambda (vector)
      (let* ((vector-value (dylan-call dylan:get-array-value vector))
	     (vl (vector-length vector-value)))
	(if (zero? vl)
	    (dylan-call dylan:error "(last <vector>) -- vector is empty" vector)
	    (vector-ref vector-value (- vl 1)))))))

(define dylan:vector
  (dylan::function->method
    (make-param-list '() #F 'REST-ARGS #F)
    (lambda args
      (if (null? args)
	  (vector)
	  (apply vector args)))))

(add-method dylan:current-key
  (dylan::function->method
   (make-param-list `((VECTOR ,<vector>) (STATE ,<object>)) #F #F #F)
   (lambda (vector state)
     vector				; Ignored
     (vector-ref state 0))))

;;;
;;; Collection Keys
;;;

(add-method
 dylan:element
 (dylan::dylan-callable->method
  (make-param-list `((VECTOR ,<vector>) (INDEX ,<integer>)) #F #F '(default:))
  (lambda (multiple-values next-method vector index . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(default:))
    (let ((vector-value (dylan-call dylan:get-array-value vector)))
      (let ((size (vector-length vector-value)))
	(if (and (>= index 0) (< index size))
	    (vector-ref vector-value index)
	    (dylan::find-keyword
	     rest '(default:)
	     (lambda ()
	       (dylan-call dylan:error "(element <vector> <integer>) -- invalid index with no default value" vector-value index)))))))))

;;;
;;; Mutable Collections
;;;

(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((SOV ,<simple-object-vector>) (STATE ,<object>) (new-value ,<object>))
       #F #F #F)
    (lambda (sov state new-value)
      (vector-set! sov (vector-ref state 0) new-value)
      new-value)))

(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list `((STRETCHY-VECTOR ,<stretchy-vector>)
		       (STATE ,<object>)
		       (new-value ,<object>))
     #F #F #F)
    (lambda (st-vector state new-value)
      (vector-set! (dylan-call dylan:get-array-value st-vector)
		   (vector-ref state 0) new-value)
      new-value)))

(add-method dylan:setter/element/
  (dylan::function->method
    (make-param-list
     `((VECTOR ,<vector>) (INDEX ,<object>) (NEW-VALUE ,<object>)) #F #F #F)
    (lambda (vector-instance index new-value)
      (let ((vector (dylan-call dylan:get-array-value vector-instance)))
	(vector-set! vector index new-value)
	new-value))))
