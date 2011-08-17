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

; $Id: runtime-collections-deque.scm,v 1.13 1992/08/31 04:31:56 birkholz Exp $

;;;; Specializations for Deque Type

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <deque>))
		      (OBJECT ,<collection>)) #F #F #F)
   (lambda (class object)
     (if (not (subclass? (dylan-call dylan:object-class object) <collection>))
	 (dylan-call dylan:error
		     "as -- cannot convert a non-collection to a collection"
		     class object (dylan-call dylan:object-class object)))
     (let* ((new-deque (dylan-call dylan:make <deque>)))
       (do ((state (dylan-call dylan:initial-state object)
		   (dylan-call dylan:next-state object state))
	    (index 0 (+ index 1)))
	   ((not state) new-deque)
	 (dylan-call dylan:push-last
		     new-deque
		     (dylan-call dylan:current-element object state)))))))

;;;
;;; DEQUE SPECIALIZED MAKE
;;; Two slots: front and end of deque
;;;

(define dylan:get-deque-front "define dylan:get-deque-front")
(define dylan:get-deque-last "define dylan:get-deque-last")
(define dylan:set-deque-front! "define dylan:set-deque-front!")
(define dylan:set-deque-last! "define dylan:set-deque-last!")
(create-private-slot <deque> <object> "internal-deque-front"
  (lambda (set get)
    (set! dylan:set-deque-front! set)
    (set! dylan:get-deque-front get)))
(create-private-slot <deque> <object> "internal-deque-last"
  (lambda (set get)
    (set! dylan:set-deque-last! set)
    (set! dylan:get-deque-last get)))

(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((DEQUE ,(dylan::make-singleton <deque>)))
		   #F #F '(size: fill:))
  (lambda (multiple-values next-method class . rest)
    multiple-values class		; Not used
    (dylan::keyword-validate next-method rest '(size: fill:))
    (let* ((size (dylan::find-keyword rest 'size: (lambda () 0)))
	   (fill (dylan::find-keyword rest 'fill: (lambda () #F))))
      (if (or (not (integer? size)) (negative? size))
	  (dylan-call dylan:error
		      "make -- deque size invalid" size))
      (let ((instance (dylan::make-<object> <deque>)))
	(dylan-call dylan:set-deque-front! instance '())
	(dylan-call dylan:set-deque-last! instance '())
	(do ((n 0 (+ n 1)))
	    ((= n size) instance)
	  (dylan-call dylan:push instance fill)))))))

;;;;
;;;; Functions for Sequences (page 104)
;;;;

(add-method dylan:add
  (dylan::function->method one-deque-and-an-object
    (lambda (deque new-element)
      (dylan-call dylan:push deque new-element))))


(define dylan::push
  (lambda (deque new-value)
    (let* ((old-deque-front (dylan-call dylan:get-deque-front deque))
	   (new-entry (make-deque-entry #F old-deque-front new-value)))
      (if old-deque-front
	  (set-deque-entry.previous! old-deque-front new-entry)
	  (dylan-call dylan:set-deque-last! deque new-entry))
      (dylan-call dylan:set-deque-front! deque new-entry)
      deque)))

(add-method dylan:add!
  (dylan::function->method one-deque-and-a-value dylan::push))


(add-method
 dylan:remove!
 (dylan::dylan-callable->method
  (make-param-list `((DEQUE ,<deque>) (VALUE ,<object>)) #F #F '(test: count:))
  (lambda (multiple-values next-method deque value . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: count:))
    (let ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?)))
	  (count (dylan::find-keyword
		  rest 'count: (lambda () (dylan-call dylan:size deque))))
	  (num-changed 0)
	  (front-entry (dylan-call dylan:get-deque-front deque)))
      (cond ((null? front-entry) front-entry)
	    ((null? (deque-entry.next front-entry))
	     (if (and (> count 0) (dylan-call test?
					      (deque-entry.value front-entry)
					      value))
		 (begin
		   (dylan-call dylan:set-deque-front! deque #F)
		   (dylan-call dylan:set-deque-last! deque #F)))
	     deque)
	    (else
	     (let loop ((front-entry front-entry))
	       (if (and (> count num-changed)
			(dylan-call test?
				    (deque-entry.value front-entry)
				    value))
		   (let ((next-entry (deque-entry.next front-entry)))
		     (set! num-changed (+ num-changed 1))
		     (set-deque-entry.previous! next-entry #F)
		     (dylan-call dylan:set-deque-front! deque next-entry)
		     (if next-entry
			 (loop next-entry)
			 deque))))
	     (let loop ((current-entry
			 (deque-entry.next
			  (dylan-call dylan:get-deque-front deque)))
			(num-changed num-changed))
	       (if (or (null? current-entry) (>= num-changed count))
		   deque
		   (if (dylan-call test?
				   (deque-entry.value current-entry)
				   value)
		       (let ((prev (deque-entry.previous current-entry))
			     (next (deque-entry.next current-entry)))
			 (if prev
			     (set-deque-entry.next! prev next)
			     (dylan-call dylan:set-deque-front! deque next))
			 (if next
			     (begin
			       (set-deque-entry.previous! next prev)
			       (loop next (+ num-changed 1)))
			     (begin
			       (dylan-call dylan:set-deque-last! deque prev)
			       deque)))
		       (loop (deque-entry.next current-entry)
			     num-changed))))))))))


(add-method
 dylan:remove-duplicates!
 (dylan::dylan-callable->method
  (make-param-list `((DEQUE ,<deque>)) #F #F '(test:))
  (lambda (multiple-values next-method old-deque . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let* ((test? (dylan::find-keyword rest 'test: (lambda () dylan:id?)))
	   (new-deque
	    (dylan-call dylan:remove-duplicates old-deque 'test: test?)))
      (do ((count (dylan-call dylan:size old-deque) (- count 1)))
	  ((<= count 0) 'done)
	(dylan-call dylan:pop old-deque))
      (do ((count (dylan-call dylan:size new-deque) (- count 1)))
	  ((<= count 0) old-deque)
	(dylan-call dylan:push-last
		    old-deque (dylan-call dylan:pop new-deque)))))))


(add-method dylan:concatenate
  (dylan::function->method
    (make-param-list `((DEQUE ,<deque>)) #F #T #F)
    (lambda (deque-1 . rest)
      (let loop ((result-deque (dylan-call dylan:make <deque>))
		 (all-collections (cons deque-1 rest)))
	(for-each (lambda (collection)
		    (iterate! (lambda (element)
				(dylan-call dylan:push-last
					    result-deque element))
			      collection))
		  all-collections)
	result-deque))))

(add-method dylan:reverse
  (dylan::function->method one-deque
    (lambda (deque-1)
      (let ((result (dylan-call dylan:make <deque>)))
	(do ((state (dylan-call dylan:initial-state deque-1)
		    (dylan-call dylan:next-state deque-1 state)))
	    ((not state) result)
	  (dylan-call dylan:push
		      result
		      (dylan-call dylan:current-element deque-1 state)))))))

(add-method dylan:reverse!
  (dylan::function->method one-deque
    (lambda (deque-1)
      (let ((tmp-deque (dylan-call dylan:make <deque>))
	    (size (dylan-call dylan:size deque-1)))
	(if (<= size 0)
	    deque-1
	    (begin
	      (do ((i 1 (+ i 1)))
		  ((= i size) 'continue)
		(dylan-call dylan:push
			    tmp-deque (dylan-call dylan:pop deque-1)))
	      (do ((i 1 (+ i 1)))
		  ((= i size) deque-1)
		(dylan-call dylan:push-last
			    deque-1 (dylan-call dylan:pop tmp-deque)))))))))

(add-method
 dylan:sort!
 (dylan::dylan-callable->method
  (make-param-list `((DEQUE ,<deque>)) #F #F '(test: stable:))
  (lambda (multiple-values next-method deque . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test: stable:))
    (let* ((test? (dylan::find-keyword rest 'test: (lambda () dylan:<)))
	   (stable (dylan::find-keyword rest 'stable: (lambda () #F)))
	   (deque-list (sort (iterate->list (lambda (x) x) deque)
			     (lambda (x y)
			       (dylan-call test? x y)))))
      stable				; Ignored
      (dylan::empty-deque! deque)
      (for-each (lambda (x) (dylan-call dylan:push-last deque x))
		deque-list)
      deque))))



;;;;
;;;; OPERATIONS ON DEQUES (page 114)
;;;;

(define deque-entry-type
  (make-record-type
   'deque-entry
   '(previous				; Pointer to previous entry
     next				; Pointer to next entry
     value				; Value of this entry
     )))
(define deque-entry? (record-predicate deque-entry-type))
(define make-deque-entry (record-constructor deque-entry-type))
(define deque-entry.previous (record-accessor deque-entry-type 'previous))
(define deque-entry.next (record-accessor deque-entry-type 'next))
(define deque-entry.value (record-accessor deque-entry-type 'value))
(define set-deque-entry.previous! (record-updater deque-entry-type 'previous))
(define set-deque-entry.next! (record-updater deque-entry-type 'next))
(define set-deque-entry.value! (record-updater deque-entry-type 'value))

(define (dylan::empty-deque! deque)
  (let ((size (dylan-call dylan:size deque)))
    (do ((i 0 (+ i 1)))
	((= i size) deque)
      (dylan-call dylan:pop deque))))

(define dylan:push
  (dylan::generic-fn 'push one-deque-and-a-value dylan::push))

(define dylan:pop
  (dylan::generic-fn 'pop
    one-deque
    (lambda (deque)
      (let ((old-front-entry (dylan-call dylan:get-deque-front deque)))
	(if old-front-entry
	    (let ((old-front-value (deque-entry.value old-front-entry))
		  (second-entry (deque-entry.next old-front-entry)))
	      (if second-entry
		  (begin
		    (dylan-call dylan:set-deque-front! deque second-entry)
		    (set-deque-entry.previous! second-entry #F))
		  (begin
		    (dylan-call dylan:set-deque-front! deque #F)
		    (dylan-call dylan:set-deque-last! deque #F)))
	      old-front-value)
	    (dylan-call dylan:error "pop -- deque is empty" deque))))))

(define dylan:push-last
  (dylan::generic-fn 'push-last
    one-deque-and-a-value
    (lambda (deque new-value)
      (let* ((old-deque-last (dylan-call dylan:get-deque-last deque))
	     (new-entry (make-deque-entry old-deque-last #F new-value)))
	(if old-deque-last
	    (set-deque-entry.next! old-deque-last new-entry)
	    (dylan-call dylan:set-deque-front! deque new-entry))
	(dylan-call dylan:set-deque-last! deque new-entry)
	deque))))

(define dylan:pop-last
  (dylan::generic-fn 'pop-last
    one-deque
    (lambda (deque)
      (let ((old-last-entry (dylan-call dylan:get-deque-last deque)))
	(if old-last-entry
	    (let ((old-last-value (deque-entry.value old-last-entry))
		  (previous-entry (deque-entry.previous old-last-entry)))
	      (if previous-entry
		  (begin
		    (dylan-call dylan:set-deque-last! deque previous-entry)
		    (set-deque-entry.next! previous-entry #F))
		  (begin
		    (dylan-call dylan:set-deque-last! deque #F)
		    (dylan-call dylan:set-deque-front! deque #F)))
	      old-last-value)
	    (dylan-call dylan:error
			"pop-last -- deque is empty" deque))))))

;  (add-method dylan:initialize
;    (dylan::generic-fn
;     'initialize
;     (make-param-list `((DEQUE ,<deque>)) #T #F #F)
;     (lambda (next obj)
;       '...))))


;;;
;;; Mutable Collection
;;;


(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((DEQUE ,<deque>) (STATE ,<object>) (new-value ,<object>))
     #F #F #F)
    (lambda (deque state new-value)
      deque
      (set-deque-entry.value! state new-value)
      new-value)))
