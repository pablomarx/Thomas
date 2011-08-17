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

; $Id: runtime-collections-generic.scm,v 1.29 1992/08/31 04:49:45 birkholz Exp $

;;;; This file contains the implementation of most of the generic functions
;;;; and methods that deal with collections and their subtypes.  The
;;;; organization follows the Dylan manual.  The specializations of method
;;;; based on collection type are in the file: runtime-collections-xxx.scm
;;;; (where xxx is collection type)


;;;
;;; Misc. functions
;;;
(add-method dylan:binary=
  (dylan::function->method
    two-collections
    (lambda (coll-1 coll-2)
      (let ((key-sequence-1 (dylan-call dylan:key-sequence coll-1))
	    (key-sequence-2 (dylan-call dylan:key-sequence coll-2))
	    (size-1 (dylan-call dylan:size coll-1))
	    (size-2 (dylan-call dylan:size coll-2)))
	(cond ((not (and size-1 size-2)) #F)
	      ; if either one is unbound, then false, since specific methods
	      ; will handle infinite ranges and circular lists
	      ((not (= size-1 size-2)) #F)
	      (else
	       (let loop ((state
			   (dylan-call dylan:initial-state key-sequence-1)))
		 (if (not state)
		     #T
		     (let ((key (dylan-call
				 dylan:current-element key-sequence-1 state)))
		       (if (dylan-call dylan:member? key key-sequence-2
				       'test:
				       (make-dylan-callable
					(lambda (a b)
					  (dylan-call dylan:= a b))))
			   (if (dylan-call
				dylan:=
				(dylan-call dylan:element coll-1 key)
				(dylan-call dylan:element coll-2 key))
			       (loop
				(dylan-call dylan:next-state key-sequence-1
					    state))
			       #F)
			   #F))))))))))



(add-method dylan:binary=
  (dylan::function->method
   two-sequences
   (lambda (seq-1 seq-2)
     (let ((size-1 (dylan-call dylan:size seq-1))
	   (size-2 (dylan-call dylan:size seq-2)))
       (cond ((not (and size-1 size-2)) #F)
	     ; if either one is unbound, then false, since specific methods
	     ; will handle infinite ranges and circular lists
	     ((not (= size-1 size-2)) #F)
	     (else
	      (do ((state-1 (dylan-call dylan:initial-state seq-1)
			    (dylan-call dylan:next-state seq-1 state-1))
		   (state-2 (dylan-call dylan:initial-state seq-2)
			    (dylan-call dylan:next-state seq-2 state-2)))
		  ((or (or (not state-1) (not state-2))
		       (not (dylan-call dylan:id?
					(dylan-call dylan:current-element
						    seq-1 state-1)
					(dylan-call dylan:current-element
						    seq-2 state-2))))
		   (if (or state-1 state-2) #F #T)))))))))

(add-method dylan:shallow-copy
  (dylan::function->method
    (make-param-list `((COLLECTION ,<collection>)) #F #F #F)
    (lambda (coll)
      (dylan-call dylan:error
		  "shallow-copy -- not specialized for this collection type"
		  coll))))

(add-method dylan:shallow-copy
  (dylan::function->method
    (make-param-list `((SEQUENCE ,<sequence>)) #F #F #F)
    (lambda (seq)
      (dylan-call dylan:copy-sequence seq))))

(add-method dylan:as
  ;; This requires specialization of AS for classes that are not
  ;; superclasses of list (such as array, table, ...)
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <collection>))
		      (COLLECTION ,<collection>)) #F #F #F)
    (lambda (class collection)
      class
      (dylan-call dylan:as <list> collection))))

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <explicit-key-collection>))
		      (COLLECTION ,<collection>)) #F #F #F)
    (lambda (class collection)
      class
      (dylan-call dylan:as <vector> collection))))
;;;;
;;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;;
(define dylan:size
  (dylan::generic-fn 'size
    one-collection
    (lambda (collection)
      (let ((index 0))
	(iterate-until (lambda (elem)
			 elem (set! index (+ index 1)) #F)
		       collection)
	index))))


(define dylan:class-for-copy
  (dylan::generic-fn 'class-for-copy one-object
     (lambda (x) (dylan-call dylan:object-class x))))

(define dylan:empty?
  (dylan::generic-fn 'empty?
   (make-param-list `((COLLECTION ,<collection>)) #F #F #F)
   (lambda (collection)
     (if (dylan-call dylan:initial-state collection)
	 #F
	 #T))))

(define dylan:do
  (dylan::generic-fn 'do
   (make-param-list
    `((FUNCTION ,<function>) (COLLECTION ,<collection>)) #F #T #F)
   (lambda (fn . collections)
     (collections-iterate fn (lambda (val)
			       val		; Ignored
			       #F)
			  #F
			  collections))))

(define dylan:map
  (dylan::generic-fn 'map
     procedure-and-at-least-one-collection
     (lambda (proc li . rest)
       (let ((all-lists (cons li rest)))
	 (let loop ((result '())
		    (next-states (map (lambda (li)
					(dylan-call dylan:initial-state li))
				      all-lists)))
	   (if (any? (lambda (state) (not state)) next-states)
	       (dylan-call dylan:as
			   (dylan-call dylan:class-for-copy li)
			   (reverse result))
	       (let ((next-value
		      (dylan-call dylan:apply
				  proc (map (lambda (li st)
					      (dylan-call dylan:current-element
							  li st))
					    all-lists next-states))))
		 (loop (cons next-value result)
		       (map (lambda (li st)
			      (dylan-call dylan:next-state li st))
			    all-lists next-states)))))))))

(define dylan:map-as
  (dylan::generic-fn 'map-as
    (make-param-list `((CLASS ,<class>)
		       (PROC ,<function>)
		       (COLLECTION ,<collection>))
		     #F #T #F)
    (lambda (class proc collection . rest)
      (let ((result (dylan-call dylan:apply
				dylan:map
				(cons proc
				      (cons collection rest)))))
	(dylan-call dylan:as class result)))))


(define dylan:map-into
  (dylan::generic-fn 'map-into
    (make-param-list `((MUTABLE-COLLECTION ,<mutable-collection>)
		       (PROCEDURE ,<function>)
		       (COLLECTION ,<collection>))
		      #F #T #F)
     (lambda (mut-coll proc coll-1 . rest)
       (let ((current-state (dylan-call dylan:initial-state mut-coll)))
	 (collections-iterate
	  proc
	  (lambda (new-value)
	    (dylan-call dylan:setter/current-element/ mut-coll
			current-state new-value)
	    (set! current-state
		  (dylan-call dylan:next-state mut-coll current-state))
	    #F)
	  mut-coll
	  `(,mut-coll ,coll-1 ,@rest))))))

(define dylan:any?
  (dylan::generic-fn 'any?
    (make-param-list
     `((PROCEDURE ,<function>) (COLLECTION ,<collection>)) #F #T #F)
    (lambda (fn . collections)
      (collections-iterate fn
			   (lambda (val) (if val (lambda () val) #F))
			   #F
			   collections))))

(define dylan:every?
  (dylan::generic-fn 'every?
    (make-param-list `((PROCEDURE ,<function>)
		       (COLLECTION ,<collection>))
		     #F #T #F)
    (lambda (fn . collections)
      (collections-iterate fn
			   (lambda (val) (if val #F (lambda () #F)))
			   #T
			   collections))))

(define dylan:reduce
  (dylan::generic-fn 'reduce
    (make-param-list `((PROCEDURE ,<function>)
		       (INIT-VALUE ,<object>)
		       (COLLECTION ,<collection>))
		     #F #F #F)
    (lambda (proc init-value collection)
      (if (dylan-call dylan:empty? collection)
	  init-value
	  (let loop ((cur-value init-value)
		     (state (dylan-call dylan:initial-state collection)))
	    (if state
		(let ((next-value (dylan-call proc
					      cur-value
					      (dylan-call dylan:current-element
							  collection state))))
		  (loop next-value (dylan-call dylan:next-state
					       collection state)))
		cur-value))))))

(define dylan:reduce1
  (dylan::generic-fn 'reduce1
    (make-param-list `((PROCEDURE ,<function>)
		       (COLLECTION ,<collection>)) #F #F #F)
    (lambda (proc collection)
      (if (dylan-call dylan:empty? collection)
	  (dylan-call dylan:error
		      "reduce1 -- collection argument is empty" proc collection)
	  (let loop ((cur-value
		      (dylan-call dylan:current-element
				  collection
				  (dylan-call dylan:initial-state collection)))
		     (state
		      (dylan-call dylan:next-state
				  collection
				  (dylan-call dylan:initial-state collection))))
	    (if state
		(let ((next-value (dylan-call proc
					      cur-value
					      (dylan-call dylan:current-element
							  collection state))))
		  (loop next-value (dylan-call dylan:next-state
					       collection state)))
		cur-value))))))


(define dylan:member?
  (dylan::generic-fn
   'member?
   (make-param-list `((VALUE ,<object>) (COLLECTION ,<collection>))
		    #F #F '(test:))
   #F))

(add-method
 dylan:member?
 (dylan::dylan-callable->method
  (make-param-list `((VALUE ,<object>) (COLLECTION ,<collection>))
		   #F #F '(test:))
  (lambda (multiple-values next-method value collection . keys)
    multiple-values
    (dylan::keyword-validate next-method keys '(test:))
    (let ((test (dylan::find-keyword keys 'test: (lambda () dylan:id?))))
      (call-with-current-continuation
       (lambda (return-value)
	 (iterate!
	  (lambda (obj)
	    (let ((test-value (dylan-call test value obj)))
	      (if test-value (return-value test-value))))
	  collection)
	 (return-value #F)))))))

(define dylan:find-key
  (dylan::generic-fn
   'find-key
   (make-param-list `((COLLECTION ,<collection>) (PREDICATE ,<function>))
		    #F #F '(skip: failure:))
   #F))

(add-method
 dylan:find-key
 (dylan::dylan-callable->method
  (make-param-list `((COLLECTION ,<collection>) (PREDICATE ,<function>))
		   #F #F '(skip: failure:))
  (lambda (multiple-values next-method collection predicate . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(skip: failure:))
    (let ((skip (dylan::find-keyword rest 'skip: (lambda () 0)))
	  (failure (dylan::find-keyword rest 'failure: (lambda () #F)))
	  (key-sequence (dylan-call dylan:key-sequence collection)))
      (let loop ((key-state (dylan-call dylan:initial-state key-sequence))
		 (satisfied 0))
	(cond ((not key-state) failure)
	      ((dylan-call predicate
			   (dylan-call dylan:element
				       collection
				       (dylan-call dylan:current-element
						   key-sequence key-state)))
	       (if (> satisfied (- skip 1))
		   (dylan-call dylan:current-element key-sequence key-state)
		   (loop (dylan-call dylan:next-state key-sequence key-state)
			 (+ satisfied 1))))
	      (else (loop (dylan-call dylan:next-state key-sequence key-state)
			  satisfied))))))))

(define dylan:replace-elements!
  (dylan::generic-fn
   'replace-elements!
   (make-param-list `((COLLECTION ,<mutable-collection>)
		      (PREDICATE ,<function>)
		      (NEW-VAL-FN ,<function>))
		    #F #F '(count:))
   #F))

(add-method
 dylan:replace-elements!
 (dylan::dylan-callable->method
  (make-param-list `((COLLECTION ,<mutable-collection>)
		     (PREDICATE ,<function>)
		     (NEW-VAL-FN ,<function>))
		   #F #F '(count:))
  (lambda (multiple-values next-method coll predicate new-val-fn . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(count:))
    (let* ((size (dylan-call dylan:size coll))
	   (count (dylan::find-keyword rest 'count: (lambda () size))))
      (let loop ((state (dylan-call dylan:initial-state coll))
		 (num-changed 0))
	(cond ((or (>= num-changed count) (not state)) coll)
	      ((dylan-call predicate
			   (dylan-call dylan:current-element coll state))
	       (dylan-call
		dylan:setter/current-element/
		coll state (dylan-call new-val-fn
				       (dylan-call dylan:current-element
						   coll state)))
	       (loop (dylan-call dylan:next-state coll state)
		     (+ num-changed 1)))
	      (else (loop (dylan-call dylan:next-state coll state)
			  num-changed))))))))


(define dylan:fill!
  (dylan::generic-fn
   'fill!
   (make-param-list `((MUTABLE-COLLECTION ,<mutable-collection>)
		      (VALUE ,<object>))
		    #F #F '(start: end:))
   #F))

(add-method
 dylan:fill!
 (dylan::dylan-callable->method
  (make-param-list `((MUTABLE-COLLECTION ,<mutable-collection>)
		     (VALUE ,<object>))
		   #F #F '(start: end:))
  (lambda (multiple-values next-method collection new-value . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(start: end:))
    (let ((start (dylan::find-keyword rest 'start: (lambda () 0)))
	  (end (dylan::find-keyword
		rest 'end: (lambda () (dylan-call dylan:size collection)))))
      (do ((state (dylan-call dylan:initial-state collection)
		  (dylan-call dylan:next-state collection state))
	   (index 0 (+ index 1)))
	  ((or (not state) (>= index end)) collection)
	(if (and state (>= index start))
	    (dylan-call dylan:setter/current-element/
			collection state new-value)))))))


;;;;
;;;; Functions for Sequences (page 104)
;;;;
(define dylan:add
  (dylan::generic-fn 'add one-sequence-and-an-object
    (lambda rest
      (dylan-call dylan:error
		  "add -- generic method not specialized for this collection"
		  rest))))

(define dylan:add!
  (dylan::generic-fn 'add!
      one-sequence-and-an-object
      (lambda (seq obj)
	(dylan-call dylan:add seq obj))))	; Defaults to ADD


(define dylan:add-new
  (dylan::generic-fn
   'add-new
   (make-param-list `((SEQUENCE ,<sequence>) (OBJECT ,<object>))
		    #F #F '(test:))
   #F))

(add-method
 dylan:add-new
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>) (OBJECT ,<object>))
		   #F #F '(test:))
  (lambda (multiple-values next-method seq object . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test-fn (dylan::find-keyword rest 'test:
					(lambda () dylan:id?))))
      (if (iterate-until (lambda (x) (dylan-call test-fn x object)) seq)
	  seq
	  (dylan-call dylan:add seq object))))))

(define dylan:add-new!
  (dylan::generic-fn
   'add-new
   (make-param-list `((SEQUENCE ,<sequence>) (OBJECT ,<object>))
		    #F #F '(test:))
   #F))

(add-method
 dylan:add-new!
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>) (OBJECT ,<object>))
		   #F #F '(test:))
  (lambda (multiple-values next-method seq object . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test-fn (dylan::find-keyword rest 'test:
					(lambda () dylan:id?))))
      (if (iterate-until (lambda (x) (dylan-call test-fn x object)) seq)
	  seq
	  (dylan-call dylan:add! seq object))))))

(define dylan:remove
  (dylan::generic-fn
   'remove
   (make-param-list `((SEQUENCE ,<sequence>) (VALUE ,<object>))
		    #F #F '(test: count:))
   #F))

(add-method
 dylan:remove
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>) (VALUE ,<object>))
		   #F #F '(test: count:))
  (lambda (multiple-values next-method seq value . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: count:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?)))
	  (count (dylan::find-keyword rest 'count: (lambda () -1))))
      (let loop ((state (dylan-call dylan:initial-state seq))
		 (result (dylan-call dylan:make
				     (dylan-call dylan:class-for-copy seq)))
		 (changed 0))
	(if state
	    (let ((cur-element (dylan-call dylan:current-element seq state)))
	      (if (and (or (negative? count)
			   (< changed count))
		       (dylan-call test? cur-element value))
		  (loop (dylan-call dylan:next-state seq state)
			result
			(+ changed 1))
		  (loop (dylan-call dylan:next-state seq state)
			(dylan-call dylan:add result cur-element)
			changed)))
	    (dylan-call dylan:reverse result)))))))

(define dylan:remove!
  (dylan::generic-fn
   'remove!
   (make-param-list `((SEQUENCE ,<sequence>) (VALUE ,<object>))
		    #F #F '(test: count:))
   #F))

(add-method
 dylan:remove!
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>) (VALUE ,<object>))
		   #F #F '(test: count:))
  (lambda (multiple-values next-method seq value . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: count:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?)))
	  (count (dylan::find-keyword
		  rest 'count:
		  (lambda () (dylan-call dylan:size seq)))))
      (dylan-call dylan:remove seq value 'test: test? 'count: count)))))

(define dylan:choose
  (dylan::generic-fn 'choose
    (make-param-list `((PREDICATE ,<function>) (SEQUENCE ,<sequence>)) #F #F #F)
    (lambda (test? seq)
      (let loop ((state (dylan-call dylan:initial-state seq))
		 (result (dylan-call dylan:make
				     (dylan-call dylan:class-for-copy seq))))
	(if state
	    (let ((cur-element (dylan-call dylan:current-element seq state)))
	      (loop (dylan-call dylan:next-state seq state)
		    (if (dylan-call test? cur-element)
			(dylan-call dylan:add result cur-element)
			result)))
	    (dylan-call dylan:reverse result))))))

(define dylan:choose-by
  (dylan::generic-fn 'choose-by
    (make-param-list `((PREDICATE ,<function>)
		       (TEST-SEQUENCE ,<sequence>)
		       (VALUE-SEQUENCE ,<sequence>)) #F #F #F)
    (lambda (test? test-sequence value-sequence)
      (let loop ((test-state (dylan-call dylan:initial-state test-sequence))
		 (value-state (dylan-call dylan:initial-state value-sequence))
		 (result (dylan-call dylan:make
				     (dylan-call dylan:class-for-copy
						 value-sequence))))
	(if (and test-state value-state)
	    (let ((test-element
		   (dylan-call dylan:current-element
			       test-sequence test-state)))
	      (loop (dylan-call dylan:next-state test-sequence test-state)
		    (dylan-call dylan:next-state value-sequence value-state)
		    (if (dylan-call test? test-element)
			(dylan-call dylan:add
				    result
				    (dylan-call dylan:current-element
						value-sequence value-state))
			result)))
	    (dylan-call dylan:reverse result))))))

(define dylan:intersection
  ;; Does intersection result in a set whose elements are unique?
  ;; This implementation may result in a multi-set...
  (dylan::generic-fn
   'intersection
   (make-param-list `((SEQUENCE-1 ,<sequence>) (SEQUENCE-2 ,<sequence>))
		    #F #F '(test:))
   #F))

(add-method
 dylan:intersection
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE-1 ,<sequence>) (SEQUENCE-2 ,<sequence>))
		   #F #F '(test:))
  (lambda (multiple-values next-method seq-1 seq-2 . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?))))
      (let loop ((state (dylan-call dylan:initial-state seq-1))
		 (result (dylan-call
			  dylan:make
			  (dylan-call dylan:class-for-copy seq-1))))
	(if state
	    (let ((a (dylan-call dylan:current-element seq-1 state)))
	      (loop (dylan-call dylan:next-state seq-1 state)
		    (if (dylan-call
			 dylan:any?
			 (make-dylan-callable (lambda (b)
						(dylan-call test? a b))
					      1)
			 seq-2)
			(dylan-call dylan:add result a)
			result)))
	    (dylan-call dylan:reverse result)))))))

(define dylan:union
  (dylan::generic-fn
   'union
   (make-param-list
    `((SEQUENCE-1 ,<sequence>) (SEQUENCE-2 ,<sequence>)) #F #F '(test:))
   #F))

(add-method
 dylan:union
 (dylan::dylan-callable->method
  (make-param-list
   `((SEQUENCE-1 ,<sequence>) (SEQUENCE-2 ,<sequence>)) #F #F '(test:))
  (lambda (multiple-values next-method seq-1 seq-2 . rest)
    multiple-values			; Ignored
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?))))
      (dylan-call dylan:remove-duplicates
		  (dylan-call dylan:concatenate
			      (dylan-call dylan:as
					  (dylan-call dylan:class-for-copy
						      seq-1)
					  seq-1)
			      (dylan-call dylan:as
					  (dylan-call dylan:class-for-copy
						      seq-1)
					  seq-2))
		  'test: test?)))))

(define dylan:remove-duplicates
  (dylan::generic-fn
   'remove-duplicates
   (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test:))
   #F))

(add-method
 dylan:remove-duplicates
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test:))
  (lambda (multiple-values next-method seq . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?))))
      (let loop ((state (dylan-call dylan:initial-state seq))
		 (result (dylan-call dylan:make
				     (dylan-call dylan:class-for-copy seq))))
	(if state
	    (let ((cur-element (dylan-call dylan:current-element seq state))
		  (result-size (dylan-call dylan:size result)))
	      (do ((state-2 (dylan-call dylan:initial-state seq)
			    (dylan-call dylan:next-state seq state-2))
		   (count 0 (+ count 1)))
		  ((or (>= count result-size)
		       (dylan-call test?
				   cur-element
				   (dylan-call dylan:current-element
					       seq state-2)))
		   (loop (dylan-call dylan:next-state seq state)
			 (if (>= count result-size)
			     (dylan-call dylan:add result cur-element)
			     result)))))
	    (dylan-call dylan:reverse result)))))))


(define dylan:remove-duplicates!
  (dylan::generic-fn
   'remove-duplicates!
   (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test:))
   #F))

(add-method
 dylan:remove-duplicates!
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test:))
  (lambda (multiple-values next-method seq . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?))))
      (dylan-call dylan:remove-duplicates seq 'test: test?)))))

(define dylan:copy-sequence
  (dylan::generic-fn
   'copy-sequence
   (make-param-list `((SOURCE ,<sequence>)) #F #F '(start: end:))
   #F))

(add-method
 dylan:copy-sequence
 (dylan::dylan-callable->method
  (make-param-list `((SOURCE ,<sequence>)) #F #F '(start: end:))
  (lambda (multiple-values next-method source . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(start: end:))
    (let ((start (dylan::find-keyword rest 'start: (lambda () 0)))
	  (end (dylan::find-keyword
		rest 'end: (lambda () (dylan-call dylan:size source)))))
      (let loop ((state (dylan-call dylan:initial-state source))
		 (result (dylan-call dylan:make
				     (dylan-call
				      dylan:class-for-copy source)))
		 (index 0))
	(if (and state (<= index (- end 1)))
	    (loop (dylan-call dylan:next-state source state)
		  (if (>= index start)
		      (dylan-call dylan:add
				  result
				  (dylan-call dylan:current-element
					      source state))
		      result)
		  (+ index 1))
	    (dylan-call dylan:reverse result)))))))


(define dylan:concatenate-as
  (dylan::generic-fn 'concatenate-as
    (make-param-list `((CLASS ,<class>) (SEQUENCE ,<sequence>)) #F 'REST #F)
    (lambda (class seq-1 . rest)
      (if (not (subclass? class <mutable-sequence>))
	  (dylan-call dylan:error
		      "concatenate-as -- target class not a mutable sequence"
		      class seq-1 rest))
      (dylan-call dylan:as
		  class (dylan-call dylan:apply
				    dylan:concatenate (cons seq-1 rest))))))

(define dylan:concatenate
  (dylan::generic-fn 'concatenate
    at-least-one-sequence
    (lambda (seq-1 . rest)
      (dylan-call dylan:error
		  "concatenate -- not specialized for argument" seq-1 rest))))

(define dylan:replace-subsequence!
  (dylan::generic-fn
   'replace-subsequence!
   (make-param-list `((MUTABLE-SEQUENCE ,<mutable-sequence>)
		      (INSERT-SEQUENCE ,<sequence>))
		    #F #F '(start:))
   #F))

(add-method
 dylan:replace-subsequence!
 (dylan::dylan-callable->method
  (make-param-list `((MUTABLE-SEQUENCE ,<mutable-sequence>)
		     (INSERT-SEQUENCE ,<sequence>))
		   #F #F '(start:))
  (lambda (multiple-values next-method mutable insert . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(start:))
    (let ((start (dylan::find-keyword rest 'start: (lambda () 0)))
	  (m-state (dylan-call dylan:initial-state mutable)))
      (if (< (- (dylan-call dylan:size mutable) start)
	     (dylan-call dylan:size insert))
	  (dylan-call dylan:error
		      "replace-subsequence! -- not enough elements in target"
		      mutable insert start))
      (if (negative? start)
	  (dylan-call dylan:error
		      "replace-subsequence! -- index cannot be negative"
		      mutable insert start))
      (do ((count 0 (+ count 1)))
	  ((= count start) 'done)
	(set! m-state (dylan-call dylan:next-state mutable m-state)))
      (let loop ((i-state (dylan-call dylan:initial-state insert))
		 (m-state m-state))
	(if i-state
	    (begin
	      (dylan-call
	       dylan:setter/current-element/
	       mutable m-state
	       (dylan-call dylan:current-element insert i-state))
	      (loop (dylan-call dylan:next-state insert i-state)
		    (dylan-call dylan:next-state mutable m-state)))
	    mutable))))))


(define dylan:reverse
  (dylan::generic-fn 'reverse
    one-sequence
    (lambda (seq-1)
      (dylan-call dylan:error
		  "reverse -- not defined for this sequence type" seq-1))))


(define dylan:reverse!
  (dylan::generic-fn 'reverse!
    one-sequence
    (lambda (seq-1)
      (dylan-call dylan:reverse seq-1))))


(define dylan:sort
  (dylan::generic-fn
   'sort
   (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test: stable:))
   #F))

(add-method
 dylan:sort
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test: stable:))
  (lambda (multiple-values next-method seq . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: stable:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:<)))
	  (stable (dylan::find-keyword rest 'stable: (lambda () #F))))
      stable			; Ignored
      (dylan-call dylan:as
		  (dylan-call dylan:class-for-copy seq)
		  (sort (dylan-call dylan:as <list> seq)
			(lambda (x y)
			  (dylan-call test? x y))))))))

(define dylan:sort!
  (dylan::generic-fn
   'sort!
   (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test: stable:))
   #F))

(add-method
 dylan:sort!
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>)) #F #F '(test: stable:))
  (lambda (multiple-values next-method seq . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: stable:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:<)))
	  (stable (dylan::find-keyword rest 'stable: (lambda () #F))))
      (dylan-call dylan:sort seq 'test: test? 'stable: stable)))))

(define dylan:first
  (dylan::generic-fn 'first
    one-sequence
    (lambda (sequence-1)
      (let ((state (dylan-call dylan:initial-state sequence-1)))
	(if state
	    (dylan-call dylan:current-element sequence-1 state)
	    (dylan-call dylan:error
			"first -- no element in sequence" sequence-1))))))

(define dylan:second
  (dylan::generic-fn 'second
    one-sequence
    (lambda (sequence-1)
      (let ((state (dylan-call dylan:get-state sequence-1 1)))
	(if state
	    (dylan-call dylan:current-element sequence-1 state)
	    (dylan-call dylan:error
			"second -- sequence size < 2" sequence-1))))))

(define dylan:third
  (dylan::generic-fn 'third
    one-sequence
    (lambda (sequence-1)
      (let ((state (dylan-call dylan:get-state sequence-1 2)))
	(if state
	    (dylan-call dylan:current-element sequence-1 state)
	    (dylan-call dylan:error
			"third -- sequence size < 3" sequence-1))))))


(define dylan:setter/first/
  (dylan::generic-fn 'setter/first/
    one-mutable-sequence-and-an-object
    (lambda (sequence-1 new-value)
      (let ((state (dylan-call dylan:initial-state sequence-1)))
	(if state
	    (begin
	      (dylan-call
	       dylan:setter/current-element/ sequence-1 state new-value)
	      new-value)
	    (dylan-call dylan:error
			"(setter first) -- sequence is empty"
			sequence-1 new-value))))))

(define dylan:setter/second/
  (dylan::generic-fn 'setter/first/
    one-mutable-sequence-and-an-object
    (lambda (sequence-1 new-value)
      (let ((size (dylan-call dylan:size sequence-1)))
	(if (or (not size) (>= size 2))
	    (begin
	      (dylan-call dylan:setter/current-element/
			  sequence-1
			  (dylan-call dylan:get-state sequence-1 1)
			  new-value)
	      new-value)
	    (dylan-call dylan:error
			"(setter second) -- sequence size < 2"
			sequence-1 new-value))))))

(define dylan:setter/third/
  (dylan::generic-fn 'setter/first/
    one-mutable-sequence-and-an-object
    (lambda (sequence-1 new-value)
      (let ((size (dylan-call dylan:size sequence-1)))
	(if (or (not size) (>= size 3))
	    (begin
	      (dylan-call dylan:setter/current-element/
			  sequence-1
			  (dylan-call dylan:get-state sequence-1 2)
			  new-value)
	      new-value)
	    (dylan-call dylan:error
			"(setter third) -- sequence size < 3"
			sequence-1 new-value))))))

(define dylan:last
  (dylan::generic-fn 'last
    one-sequence
    (lambda (sequence-1)
      (let ((prev-state #F))
	(do ((state (dylan-call dylan:initial-state sequence-1)
		    (dylan-call dylan:next-state sequence-1 state)))
	    ((not state)
	     (if prev-state
		 (dylan-call dylan:current-element sequence-1 prev-state)
		 (dylan-call dylan:error
			     "last -- sequence is empty" sequence-1)))
	  (set! prev-state state))))))

(define (check-subsequence test? big big-state pattern pattern-state)
  (define (check-loop big-state pattern-state)
    (if (not pattern-state)
	#T
	(if (and big-state
		 (dylan-call test?
			     (dylan-call dylan:current-element big big-state)
		  (dylan-call dylan:current-element pattern pattern-state)))
	    (check-loop
	     (dylan-call dylan:next-state big big-state)
	     (dylan-call dylan:next-state pattern pattern-state))
	    #F)))
  (check-loop (dylan-call dylan:copy-state big big-state)
	      (dylan-call dylan:copy-state pattern pattern-state)))

(define dylan:subsequence-position
  (dylan::generic-fn
   'subsequence-position
   (make-param-list
    `((BIG ,<sequence>) (PATTERN ,<sequence>)) #F #F '(test: count:))
   #F))

(add-method
 dylan:subsequence-position
 (dylan::dylan-callable->method
  (make-param-list
   `((BIG ,<sequence>) (PATTERN ,<sequence>)) #F #F '(test: count:))
  (lambda (multiple-values next-method big pattern . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: count:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?)))
	  (count (dylan::find-keyword rest 'count: (lambda () 1)))
	  (first-of-pattern (dylan-call dylan:first pattern))
	  (init-state-pattern (dylan-call dylan:initial-state pattern)))
      (let loop ((state (dylan-call dylan:initial-state big))
		 (num-found 0)
		 (index 0))
	(if state
	    (if (and (dylan-call test?
				 (dylan-call dylan:current-element big state)
				 first-of-pattern)
		     (check-subsequence test? big state
					pattern init-state-pattern))
		(if (>= num-found (- count 1))
		    index
		    (loop (dylan-call dylan:next-state big state)
			  (+ num-found 1)
			  (+ index 1)))
		(loop (dylan-call dylan:next-state big state)
		      num-found
		      (+ index 1)))
	    #F))))))			; not found


;;;
;;; MUTABLE COLLECTIONS (p. 127)
;;;
(define dylan:setter/current-element/
  (dylan::generic-fn 'setter/current-element/
    (make-param-list `((MUTABLE-COLLECTION ,<mutable-collection>)
		       (STATE ,<object>)
		       (NEW-VALUE ,<object>))
		     #F #F #F)
    (lambda (mutable-collection state new-value)
      (dylan-call dylan:error
		  "(setter current-element) -- cannot set! this collection type"
		  mutable-collection state new-value))))

(define dylan:setter/element/
  (dylan::generic-fn 'setter/element/
    (make-param-list `((MUTABLE-COLLECTION ,<mutable-collection>)
		       (KEY ,<object>)
		       (NEW-VAL ,<object>))
		     #F #F #F)
    (lambda (collection key new-value)
      (dylan-call dylan:error
		  "(setter element) -- not defined for this collection type"
		  collection key new-value))))

(add-method dylan:setter/element/
  (dylan::function->method
   (make-param-list `((MUTABLE-SEQUENCE ,<mutable-sequence>)
		      (KEY ,<integer>)
		      (NEW-VALUE ,<object>))
		    #F #F #F)
   (lambda (mut-seq key new-value)
     (do ((state (dylan-call dylan:initial-state mut-seq)
		 (dylan-call dylan:next-state mut-seq state))
	  (k 0 (+ k 1)))
	 ((or (not state) (= k key))
	  (if state
	      (begin
		(dylan-call dylan:setter/current-element/
			    mut-seq state new-value)
		new-value)
	      (dylan-call dylan:error
			  "(setter element) -- key not found"
			  mut-seq key new-value)))))))

(add-method dylan:setter/element/
  (dylan::function->method
   (make-param-list
    `((MUTABLE-EXPLICIT-KEY-COLLECTION ,<mutable-explicit-key-collection>)
      (KEY ,<object>)
      (NEW-VALUE ,<object>))
    #F #F #F)
   (lambda (mut-seq key new-value)
     (do ((state (dylan-call dylan:initial-state mut-seq)
		 (dylan-call dylan:next-state mut-seq state)))
	 ((or (not state) (dylan-call
			   dylan:=
			   (dylan-call dylan:current-key mut-seq state)
			   key))
	  (if state
	      (begin
		(dylan-call dylan:setter/current-element/
			    mut-seq state new-value)
		new-value)
	      (dylan-call dylan:error
			  "(setter element) -- key not found"
			  mut-seq key new-value)))))))
