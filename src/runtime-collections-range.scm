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

; $Id: runtime-collections-range.scm,v 1.24 1992/08/31 05:16:08 birkholz Exp $

;;;; Specializations for Range Type

(add-method dylan:as
  (dylan::function->method
   (make-param-list
    `((CLASS ,(dylan::make-singleton <range>)) (COLLECTION ,<collection>))
    #F #F #F)
   (lambda (class collection)
     (if (dylan-call dylan:instance? collection <range>)
	 collection
	 (let ((size (dylan-call dylan:size collection)))
	   (cond ((= size 0) (dylan-call dylan:make <range>))
		 ((= size 1) (dylan-call
			      dylan:range 'from:
			      (dylan-call dylan:current-element collection
					  (dylan-call
					   dylan:initial-state collection))
			      'size: 1))
		 ((negative? size)
		  (dylan-call dylan:error "(as (singleton <range>) <collection>) -- internal error.size retured negative number" size class collection))
		 (else
		  (let* ((state (dylan-call dylan:initial-state collection))
			 (first (dylan-call
				 dylan:current-element collection state))
			 (state-2
			  (dylan-call dylan:next-state collection state))
			 (second (dylan-call
				  dylan:current-element collection state-2)))
		    (if (and (dylan-call dylan:subclass?
					 (get-type first) <number>)
			     (dylan-call dylan:subclass?
					 (get-type second) <number>))
			(let ((step (dylan-call dylan:- second first)))
			  (let loop ((state (dylan-call
					     dylan:next-state
					     collection state-2))
				     (prev second))
			    (if state
				(let ((cur (dylan-call
					    dylan:current-element
					    collection state)))
				  (if (dylan-call
				       dylan:= step
				       (dylan-call dylan:- cur prev))
				      (loop (dylan-call dylan:next-state
							collection state)
					    (dylan-call
					     dylan:current-element
					     collection state))
				      (dylan-call dylan:error "(as (singleton <range>) <collection>) -- numbers not in sync" class collection)))
				(dylan-call dylan:range 'from: first 'by: step
					    'through: prev))))
			(dylan-call dylan:error "(as (singleton <range>) <collection>) -- elements are not numbers" class collection))))))))))


;;;
;;; RANGE SPECIALIZED MAKE
;;; <range> has three slots: start, step, end
;;;   #F for end means unbounded range.
;;; Ranges can't be made by calling make, only range
;;;
(define dylan:get-range-start "define dylan:get-range-start")
(define dylan:get-range-step "define dylan:get-range-step")
(define dylan:get-range-end "define dylan:get-range-end")
(define dylan:set-range-start! "define dylan:set-range-start!")
(define dylan:set-range-step! "define dylan:set-range-step!")
(define dylan:set-range-end! "define dylan:set-range-end!")

(create-private-slot <range> <object> "internal-range-start"
  (lambda (set get)
    (set! dylan:set-range-start! set)
    (set! dylan:get-range-start get)))
(create-private-slot <range> <object> "internal-range-step"
  (lambda (set get)
    (set! dylan:set-range-step! set)
    (set! dylan:get-range-step get)))
(create-private-slot <range> <object> "internal-range-end"
  (lambda (set get)
    (set! dylan:set-range-end! set)
    (set! dylan:get-range-end get)))

(add-method dylan:make
  (dylan::function->method
   (make-param-list `((RANGE ,(dylan::make-singleton <range>)))
		    #F #F #T)
   (lambda (range . rest)
     range				; Not used
     rest				; Not used
     (dylan-call dylan:range 'from: 0 'through: -1 'by: 1))))

;;;
;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;
(add-method dylan:size
  (one-arg 'RANGE <range>
   (lambda (range)
     (let ((start (dylan-call dylan:get-range-start range))
	   (end (dylan-call dylan:get-range-end range))
	   (step (dylan-call dylan:get-range-step range)))
       (if end
	   (if ((if (positive? step) < >) end start)
	       0
	       (+ (abs (/ (- end start) step)) 1))
	   #F)))))

(add-method dylan:class-for-copy
  (dylan::function->method one-range (lambda (x) x <list>)))


(add-method
 dylan:member?
 (dylan::dylan-callable->method
  (make-param-list `((VALUE ,<object>) (RANGE ,<range>)) #F #F '(test:))
  (lambda (multiple-values next-method val range . keys)
    multiple-values
    (dylan::keyword-validate next-method keys '(test:))
    (let ((test (dylan::find-keyword keys 'test: (lambda () #F))))
      (if test
	  (dylan-call
	   dylan:error
	   "(member? <object> <range>) -- test: keyword argument not supported"
	   test)))
    (let ((start (dylan-call dylan:get-range-start range))
	  (end (dylan-call dylan:get-range-end range))
	  (step (dylan-call dylan:get-range-step range)))
      (if (not (number? val))
	  #F
	  (and ((if (negative? step) <= >=) val start)
	       (or (not end)
		   ((if (negative? step)
			>=
			<=)
		    val end))
	       (= 0 (remainder (- val start) step))))))))

;;;;
;;;; Functions for Sequences (page 104)
;;;;
(add-method dylan:add
 (dylan::function->method one-range-and-an-object
   (lambda (the-range new-element)
     (if (not (dylan-call dylan:subclass?
			  (dylan-call dylan:object-class new-element)
			  <number>))
	 (dylan-call dylan:error
		     "(add <range> <object>) -- new element not a number"
		     the-range new-element))
     (let ((size (dylan-call dylan:size the-range))
	   (start (dylan-call dylan:get-range-start the-range))
	   (end (dylan-call dylan:get-range-end the-range))
	   (step (dylan-call dylan:get-range-step the-range)))
       (cond ((not size)
	      (if (= start (+ new-element step))
		  (dylan-call dylan:range
			      'from: new-element
			      'by: step)
		  (dylan-call dylan:error "(add <range> <object>) -- cannot add this number to unbound range" the-range new-element)))
	     ((= size 0) (dylan-call dylan:range
				     'from: new-element
				     'through: new-element
				     'by: 1))
	     ((= size 1) (dylan-call dylan:range
				     'from: new-element
				     'through: start
				     'by: (- start new-element)))
	     (else
	      (if (= start (+ new-element step))
		  (dylan-call dylan:range 'from: new-element
			      'through: end
			      'by: step)
		  (if (= end (- new-element step))
		      (dylan-call dylan:range
				  'from: start
				  'through: new-element
				  'by: step)
		      (dylan-call dylan:error "(add <range> <object>) -- cannot add this number to range" the-range new-element)))))))))

(add-method dylan:add!
  (dylan::function->method one-range-and-an-object
   (lambda (the-range new-element)
     (if (not (dylan-call dylan:subclass?
			  (dylan-call dylan:object-class new-element)
			  <number>))
	 (dylan-call dylan:error
		     "(add! <range> <object>) -- new element not a number"
		     the-range new-element))
     (let ((size (dylan-call dylan:size the-range))
	   (start (dylan-call dylan:get-range-start the-range))
	   (end (dylan-call dylan:get-range-end the-range))
	   (step (dylan-call dylan:get-range-step the-range)))
       (cond ((not size)
	      (if (= start (+ new-element step))
		  (dylan-call dylan:set-range-start! the-range new-element)
		  (dylan-call dylan:error "(add <range> <object>) -- cannot add this number to unbound range" the-range new-element)))
	     ((= size 0)
	      (dylan-call dylan:set-range-start! the-range new-element)
	      (dylan-call dylan:set-range-end! the-range new-element)
	      (dylan-call dylan:set-range-step! the-range 1))
	     ((= size 1)
	      (dylan-call dylan:set-range-start! the-range new-element)
	      (dylan-call dylan:set-range-end! the-range start)
	      (dylan-call dylan:set-range-step! the-range
			  (- start new-element)))
	     (else
	      (if (= start (+ new-element step))
		  (dylan-call dylan:set-range-start! the-range new-element)
		  (if (= end (- new-element step))
		      (dylan-call dylan:set-range-end! the-range new-element)
		      (dylan-call dylan:error "(add <range> <object>) -- cannot add this number to range" the-range new-element)))))
       the-range))))			; Return the modified range


(add-method
 dylan:remove!
 (dylan::dylan-callable->method
  (make-param-list `((RANGE ,<range>) (VALUE ,<object>)) #F #F '(count: test:))
  (lambda (multiple-values next-method range value . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(count: test:))
    (let ((test (dylan::find-keyword rest 'test: (lambda () dylan:id?)))
	  (count (dylan::find-keyword rest 'count:
				      (lambda ()
					(dylan-call dylan:size range)))))
      (if count
	  (dylan-call dylan:remove range value 'test: test 'count: count)
	  (dylan-call dylan:remove range value 'test: test))))))


(add-method
 dylan:remove-duplicates!
 (dylan::dylan-callable->method
  (make-param-list `((RANGE ,<range>)) #F #F '(test:))
  (lambda (multiple-values next-method range . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test (dylan::find-keyword rest 'test: (lambda () dylan:id?))))
      (dylan-call dylan:remove-duplicates range 'test: test)))))


(add-method
 dylan:copy-sequence
 (dylan::dylan-callable->method
  (make-param-list `((RANGE ,<range>)) #F #F '(start: end:))
  (lambda (multiple-values next-method range . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(start: end:))
    (let ((start (dylan::find-keyword
		  rest 'start:
		  (lambda () (dylan-call dylan:get-range-start range))))
	  (end (dylan::find-keyword
		rest
		'end: (lambda () (dylan-call dylan:get-range-end range))))
	  (step (dylan-call dylan:get-range-step range)))
      (if (and end (not (= 0 (remainder (- end start) step))))
	  (dylan-call
	   dylan:error
	   "(copy-subsequence <range>) -- bad start and/or end parameters"
	   range start end))
      (dylan-call dylan:range 'from: start 'through: end 'by: step)))))


(add-method dylan:concatenate
  (dylan::function->method
    (make-param-list `((RANGE ,<range>)) #F 'REST #F)
    (lambda (range-1 . rest)
      (let ((end-1 (dylan-call dylan:get-range-end range-1))
	    (step-1 (dylan-call dylan:get-range-step range-1))
	    (result (dylan-call dylan:copy-sequence range-1)))
	(if (not end-1)
	    (dylan-call dylan:error
			"(concatenate <range> !rest) -- range is unbound"
			range-1 rest))
	(let loop ((rest-ranges (map
				 (lambda (collection)
				   (dylan-call dylan:as <range> collection))
				 rest)))
	  (if (null? rest-ranges)
	      result
	      (let* ((next-range (car rest-ranges))
		     (start-2 (dylan-call dylan:get-range-start next-range))
		     (end-2 (dylan-call dylan:get-range-end next-range))
		     (step-2 (dylan-call dylan:get-range-step next-range))
		     (size (dylan-call dylan:size next-range)))
		(if (not end-2)
		    (dylan-call
		     dylan:error
		     "(concatenate <range> !rest) -- range is unbound"
		     next-range range-1 rest))
		(if (positive? size)
		    (if (and (= start-2 (+ end-1 step-1))
			     (or (= step-1 step-2) (= size 1)))
			(begin
			  (dylan-call dylan:set-range-end! result end-2)
			  (set! end-1 end-2)
			  (loop (cdr rest-ranges)))
			(dylan-call
			 dylan:error
			 "(concatenate <range> !rest) -- incompatible ranges"
			 next-range result range-1 rest))
		    (loop (cdr rest-ranges))))))))))


(add-method dylan:reverse
  (dylan::function->method one-range
    (lambda (range)
      (let ((start (dylan-call dylan:get-range-start range))
	    (end (dylan-call dylan:get-range-end range))
	    (step (dylan-call dylan:get-range-step range)))
	(if end
	    (dylan-call dylan:range 'from: end 'through: start 'by: (- step))
	    (dylan-call dylan:error
			"(reverse <range>) -- can't reverse unbounded range"
			range))))))

(add-method dylan:reverse!
  (dylan::function->method one-range
    (lambda (range)
      (let ((start (dylan-call dylan:get-range-start range))
	    (end (dylan-call dylan:get-range-end range))
	    (step (dylan-call dylan:get-range-step range)))
	(if end
	    (begin
	      (dylan-call dylan:set-range-start! range end)
	      (dylan-call dylan:set-range-end! range start)
	      (dylan-call dylan:set-range-step! range (- step))
	      range)
	    (dylan-call dylan:error
			"(reverse! <range>) -- can't reverse unbounded range"
			range))))))

(add-method dylan:last
  (dylan::function->method one-range
    (lambda (range)
      (let ((end (dylan-call dylan:get-range-end range)))
	(or end
	    (dylan-call dylan:error
			"(last <range>) -- no end in sight for this range"
			range))))))


;;;;
;;;; Operations on Ranges (page 118)
;;;;

(define dylan:range
  (dylan::generic-fn 'range only-rest-args #F))

(add-method
 dylan:range
 (dylan::dylan-callable->method
  (make-param-list '() #F #F '(by: through: from: up-to: size:))
  (lambda (multiple-values next-method . rest)
    multiple-values
    (dylan::keyword-validate next-method rest
			     '(by: through: from: up-to: size:))
    (let* ((from (dylan::find-keyword rest 'from: (lambda () 0)))
	   (up-to (dylan::find-keyword rest 'up-to: (lambda () #F)))
	   (through (dylan::find-keyword rest 'through: (lambda () #F)))
	   (by (dylan::find-keyword rest 'by: (lambda () 1)))
	   (size (dylan::find-keyword rest 'size: (lambda () #F)))
	   (by-negative? (negative? by))
	   (not-in-range (- from by))
	   (up-to->through (if up-to
			       (+ from
				  (* (ceiling (- (/ (- up-to from) by) 1))
				     by))
			       not-in-range))
	   (through->through (if through
				 (+ from
				    (* (floor (/ (- through from) by))
				       by))
				 not-in-range))
	   (size->through (if (and size (positive? size))
			      (+ from (* (- size 1) by))
			      not-in-range))
	   (max-through ((if by-negative? min max)
			 up-to->through through->through size->through)))
      (let ((range (dylan::make-<object> <range>)))
	(dylan-call dylan:set-range-start! range from)
	(dylan-call dylan:set-range-step! range by)
	(if (not (or up-to through size))
	    (dylan-call dylan:set-range-end! range #F)
	    (dylan-call dylan:set-range-end! range max-through))
	range)))))

(add-method dylan:binary=
  (dylan::function->method
     two-ranges
     (lambda (range-1 range-2)
       (let ((start-1 (dylan-call dylan:get-range-start range-1))
	     (end-1 (dylan-call dylan:get-range-end range-1))
	     (step-1 (dylan-call dylan:get-range-step range-1))
	     (start-2 (dylan-call dylan:get-range-start range-2))
	     (end-2 (dylan-call dylan:get-range-end range-2))
	     (step-2 (dylan-call dylan:get-range-step range-2))
	     (size-1 (dylan-call dylan:size range-1))
	     (size-2 (dylan-call dylan:size range-2)))
	 (cond ((or (and size-1 (not size-2))
		    (and (not size-1) size-2)) #F) ; One is unbound
	       ((not (and size-1 size-2))          ; Both unbound
		(and (= start-1 start-2) (= step-1 step-2)))
	       ((= 0 size-1 size-2) #T)            ; Empty ranges are binary=
	       (else (and (= start-1 start-2)
			  (= step-1 step-2)
			  (= end-1 end-2))))))))

;;
;; Find-First-Intersection: Given two ranges, and a start value which is in
;;                          the first range, find the first common number to
;;                          both ranges from start, if any?
;;
(define (find-first-intersection start range1 range2)
  (let* ((by-lcm (lcm (dylan-call dylan:get-range-step range1)
		      (dylan-call dylan:get-range-step range2)))
	 (step1 (dylan-call dylan:get-range-start range1))
	 (done-value ((if (negative? step1) - +) start by-lcm)))
    (let loop ((state start))		; Assumes state is the actual number
      (display (list 'loop 'state state 'done done-value)) (newline)
      (if state
	  (let ((cur-element
		 (dylan-call dylan:current-element range1 state)))
	    (cond ((dylan-call dylan:member? cur-element range2)
		   cur-element)
		  ((= cur-element done-value) #F)
		  (else
		   (loop
		    (dylan-call dylan:next-state range1 state)))))
	  #F))))			; No intersection

(add-method
 dylan:intersection
 (dylan::dylan-callable->method
  (make-param-list `((RANGE-1 ,<range>) (RANGE-2 ,<range>)) #F #F '(test:))
  (lambda (multiple-values next-method range-1 range-2 . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test (dylan::find-keyword rest 'test: (lambda () #F))))
      (if test
	  (dylan-call dylan:error
		      "(intersection <range> <range>) -- test: keyword argument not supported" test)))
    (let* ((start-1 (dylan-call dylan:get-range-start range-1))
	   (end-1 (dylan-call dylan:get-range-end range-1))
	   (step-1 (dylan-call dylan:get-range-step range-1))
	   (start-2 (dylan-call dylan:get-range-start range-2))
	   (end-2 (dylan-call dylan:get-range-end range-2))
	   (step-2 (dylan-call dylan:get-range-step range-2))
	   (by-lcm (lcm step-1 step-2)))

      (cond ((or (zero? step-1) (zero? step-2))
	     (if (= start-1 start-2)
		 range-1
		 (dylan-call dylan:make <range>)))
	    ((and (positive? step-1) (positive? step-2))
	     (let ((first-intersection
		    (find-first-intersection (max start-1 start-2)
					     range-1 range-2)))
	       (if first-intersection
		   (if (or end-1 end-2)
		       (dylan-call dylan:range
				   'from: first-intersection
				   'through: (if (and end-1 end-2)
						 (min end-1 end-2)
						 (or end-1 end-2))
				   'by:  by-lcm)
		       (dylan-call dylan:range
				   'from: first-intersection
				   'by:  by-lcm))
		   (dylan-call dylan:make <range>))))
	    ((and (negative? step-1) (negative? step-2)) ; *****
	     '...)
	    ((and (positive? step-1) (negative? step-2)) ; *****
	     '...)
	    ((and (negative? step-1) (positive? step-2)) ; *****
	     '...)
	    (else
	     (dylan-call dylan:error
			 "(intersection <range> <range>) -- internal error"
			 range-1 range-2 rest)))))))


;;;
;;; Mutable Collection
;;;

(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((RANGE ,<range>) (STATE ,<object>) (new-value ,<object>))
     #F #F #F)
    (lambda (range state new-value)
      (dylan-call dylan:error
		  "((setter current-element) <range>) -- range not mutable"
		  range state new-value))))
