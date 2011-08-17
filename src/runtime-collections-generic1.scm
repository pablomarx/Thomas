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

; $Id: runtime-collections-generic1.scm,v 1.30 1992/09/18 23:46:36 birkholz Exp $

;;; This file contains the implementation of most of the generic functions
;;; and methods that deal with collections and their subtypes.  The
;;; organization follows the Dylan manual.  The specializations of method
;;; based on collection type are in the file: runtime-collections-xxx.scm
;;; (where xxx is collection type)

;;;;
;;;; Misc. functions
;;;;

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
