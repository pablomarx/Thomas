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

; $Id: runtime-collections-generic2.scm,v 1.1 1992/09/18 23:45:58 birkholz Exp $

;;; This file is a continuation of runtime-collections-generic, which had
;;; to be split because of a limitation in the Gambit compiler.

;;;;
;;;; FUNCTIONS FOR SEQUENCES (page 104)
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

;;;;
;;;; MUTABLE COLLECTIONS (p. 127)
;;;;

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
