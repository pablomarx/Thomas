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

; $Id: runtime-collections-iterate.scm,v 1.20 1992/09/07 04:16:53 jmiller Exp $

;;;;; Handles the iteration protocol of collections, including the method
;;;;; specializations for all collection types.

;;;;
;;;; THE ITERATION PROTOCOL (page 122)
;;;; The specializations occur immediately after the definition of the
;;;; generic function.


  ;; Implementation of iteration state:
  ;;
  ;; <EMPTY-LIST>: no states available
  ;; <PAIR> and <LIST>: the object itself, with #F terminating
  ;; <ARRAY>: array of index values, incrementing "odometer style"
  ;; <TABLE>: pair of index into hash table and pointer to current
  ;;          entry in bucket.  Pointer is actually the list whose car is the
  ;;          key/element pair.   [state == (list hash-index bucket-left)]
  ;; <BYTE-STRING>: inherits from <array>
  ;; <STRING>: not handled, any user-specified subclass must supply
  ;;           operations
  ;; <DEQUE>: pointer to current entry
  ;; <RANGE>: current value

(define dylan:next-state "define dylan:next-state")
(define dylan:initial-state "define dylan:initial-state")
(define dylan:current-element "define dylan:current-element")
(define dylan:copy-state "define dylan:copy-state")
(define dylan:final-state "define dylan:final-state")
(define dylan:previous-state "define dylan:previous-state")

(let ()
  ;;
  ;; INITIAL-STATE
  ;;
  (set! dylan:initial-state
    (dylan::generic-fn 'initial-state
      one-collection
      (lambda (collection)
	(dylan-call
	 dylan:error
	 "initial-state -- not specialized for this type of collection"
	 collection))))
  (add-method dylan:initial-state
    (one-arg 'NULL <empty-list> (lambda (null) null #F)))
  (add-method dylan:initial-state
    (one-arg 'LIST <list> (lambda (list) list)))
  (add-method dylan:initial-state
    (one-arg 'ARRAY <array>
      (lambda (array)
	(let ((dimensions (dylan-call dylan:dimensions array)))
	  (if (all? positive? (iterate->list (lambda (x) x) dimensions))
	      (make-vector (length dimensions) 0)
	      #F)))))
  (add-method dylan:initial-state
    (one-arg 'DEQUE <deque>
      (lambda (deque)
	(dylan-call dylan:get-deque-front deque))))
  (add-method dylan:initial-state
    (one-arg 'RANGE <range>
      (lambda (range)
	(let ((start (dylan-call dylan:get-range-start range))
	      (end (dylan-call dylan:get-range-end range))
	      (step (dylan-call dylan:get-range-step range)))
	  (if end
	      (if ((if (negative? step) >= <=) start end)
		  start
		  #F)
	      start)))))
  (add-method dylan:initial-state
    (one-arg 'TABLE <table>
      (lambda (table)
	(let* ((hash-table (dylan-call dylan:get-hash-table table))
	       (next-bucket (find-next-non-empty-bucket hash-table -1)))
	  (if next-bucket
	      (list next-bucket (vector-ref hash-table next-bucket))
	      #F)))))

  ;;
  ;; FINAL-STATE
  ;;
  (set! dylan:final-state
    (dylan::generic-fn 'final-state one-object #F))
  (add-method dylan:final-state
    (one-arg 'EMPTY-LIST <empty-list>
      (lambda (emp-list)
	emp-list
	#F)))
;  (add-method dylan:final-state		; Not specified in the manual
;    (one-arg 'STRING <byte-string>             ; Only for efficiency
;      (lambda (string)
;	(let ((length (string-length string)))
;	  (if (zero? length) #F (- length 1))))))
  (add-method dylan:final-state		; Not specified in the manual
    (one-arg 'ARRAY <array>
      (lambda (array)
	(if (dylan-call dylan:empty? array)
	    #F
	    (map (lambda (index)
		   (- index 1))
		 (dylan-call dylan:dimensions array))))))
  (add-method dylan:final-state
    (one-arg 'SOV <simple-object-vector>
      (lambda (vec)
	(- (vector-length vec) 1))))
  (add-method dylan:final-state
    (one-arg 'DEQUE <deque>
      (lambda (deque)
	(dylan-call dylan:get-deque-last deque))))

  ;;
  ;; NEXT-STATE
  ;;
  (set! dylan:next-state
    (dylan::generic-fn 'next-state one-collection-and-a-state
		       #F))

  (add-method dylan:next-state
    (dylan::function->method
     (make-param-list `((LIST ,<list>) (STATE ,<object>)) #F #F #F)
     (lambda (list state)
       list				; Ignored
       (if (not (pair? state))
	   #F				; If dotted list, and reached end
	   (let ((left (cdr state)))
	     (cond ((null? left) #F)
				; For now: If not a pair, return the atom
		   ((not (pair? left)) left)
		   (else left)))))))

  (add-method dylan:next-state		; Array => "Odometer style"
    (dylan::function->method
     (make-param-list `((ARRAY ,<array>) (STATE ,<simple-object-vector>))
		      #F #F #F)
     (lambda (array state)
       (let ((dimensions (dylan-call dylan:dimensions array)))
	 (let loop ((next-state-indices (list->vector
					 (vector->list state)))
		    (dim-index (- (length dimensions) 1)))
	   (if (negative? dim-index)
	       #F
	       (cond ((= (+ (vector-ref next-state-indices dim-index) 1)
			 (list-ref dimensions dim-index))
		      (loop next-state-indices (- dim-index 1)))
		     ((> (+ (vector-ref next-state-indices dim-index) 1)
			 (list-ref dimensions dim-index))
		      (dylan-call dylan:error
				  "next-state -- invalid state" state))
		     (else
		      (do ((i (+ dim-index 1) (+ i 1)))
			  ((>= i (vector-length next-state-indices)) 'done)
			(vector-set! next-state-indices i 0))
		      (vector-set! next-state-indices dim-index
				   (+ (vector-ref next-state-indices dim-index)
				      1))
		      next-state-indices))))))))

  (add-method dylan:next-state
    (dylan::function->method
     (make-param-list `((NULL ,<empty-list>) (STATE ,<object>)) #F #F #F)
     (lambda (emp-list state)
       emp-list
       state
       #F)))

  (add-method dylan:next-state
    (dylan::function->method
     (make-param-list `((DEQUE ,<deque>) (STATE ,<object>)) #F #F #F)
     (lambda (deque state)
       deque				; not used
       (deque-entry.next state))))

  (add-method dylan:next-state
    (dylan::function->method
     (make-param-list `((RANGE ,<range>) (STATE ,<object>)) #F #F #F)
     (lambda (range state)
       (let* ((end (dylan-call dylan:get-range-end range))
	      (step (dylan-call dylan:get-range-step range))
	      (next (+ state step)))
	 (if end
	     (if ((if (negative? step) < >) next end)
		 #F
		 next)
	     next)))))			; Unbounded range

  (add-method dylan:next-state
    (dylan::function->method
      (make-param-list `((TABLE ,<table>) (STATE ,<object>)) #F #F #F)
      (lambda (table state)
	(let ((hash-index (car state))
	      (bucket-left (cadr state))
	      (hash-table (dylan-call dylan:get-hash-table table)))
	(cond ((null? bucket-left)
	       (dylan-call dylan:error
			   "(next-state <table> <object>) -- bad state"
			   table state))
	      ((null? (cdr bucket-left))
	       (let ((next-bucket (find-next-non-empty-bucket hash-table
							      hash-index)))
		 (if next-bucket
		     (list next-bucket (vector-ref hash-table next-bucket))
		     #F)))
	      (else (list hash-index (cdr bucket-left))))))))

  ;;
  ;; PREVIOUS-STATE
  ;;
  (set! dylan:previous-state
    (dylan::generic-fn 'previous-state one-collection-and-a-state #F))

  (add-method dylan:previous-state	; Array => "Odometer style"
    (dylan::function->method
     (make-param-list `((ARRAY ,<array>) (STATE ,<sequence>)) #F #F #F)
     (lambda (array state)
       (let ((dimensions (dylan-call dylan:dimensions array)))
	 (let loop ((previous-state-indices state)
		    (dim-index (- (length dimensions) 1)))
	   (if (negative? dim-index)
	       #F
	       (if (< (vector-ref previous-state-indices dim-index) 1)
		   (loop previous-state-indices (- dim-index 1))
		   (begin
		     (vector-set! previous-state-indices dim-index
				  (- (vector-ref previous-state-indices dim-index)
				     1))
		     previous-state-indices))))))))
  (add-method dylan:previous-state
    (dylan::function->method
     (make-param-list `((NULL ,<empty-list>) (STATE ,<object>)) #F #F #F)
     (lambda (emp-list state) emp-list state #F)))

;  (add-method dylan:previous-state	; FOR EFFICIENCY ONLY!!
;    (dylan::function->method
;     (make-param-list `((SOV ,<simple-object-vector>) (STATE ,<sequence>)
;						      #F #F #F))
;     (lambda (vect state)
;       (cond ((not (pair? state))
;	       (dylan-call dylan:error
;			   "previous-state -- invalid state" vect state))
;	     ((positive? (car state)) (list (- (car state) 1)))
;	     ((zero? (car state)) #F)
;	     (else (dylan-call dylan:error
;			       "previous-state -- invalid state"
;			       vect state))))))

  (add-method dylan:previous-state
    (dylan::function->method
     (make-param-list `((DEQUE ,<deque>) (STATE ,<object>)) #F #F #F)
     (lambda (deque state)
       deque				; not used
       (deque-entry.previous state))))
  (add-method dylan:previous-state	; Not defined in manual
     (dylan::function->method
      (make-param-list `((RANGE ,<range>) (STATE ,<object>)) #F #F #F)
      (lambda (range state)
	(let* ((start (dylan-call dylan:get-range-start range))
	       (step (dylan-call dylan:get-range-step range))
	       (prev (- state step)))
	 (if ((if (negative? step) > <) prev start)
	     #F
	     prev)))))

  ;;
  ;; CURRENT-ELEMENT
  ;;
  (set! dylan:current-element
    (dylan::generic-fn 'current-element one-collection-and-a-state
       (lambda (collection state)
	 (dylan-call
	  dylan:error
	  "current-element -- not specialized for this collection type"
	  collection state))))

  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list `((ARRAY ,<array>) (STATE ,<sequence>)) #F #F #F)
     (lambda (array state)
       (dylan-call dylan:element array state))))
  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list
      `((SOV ,<simple-object-vector>) (STATE ,<sequence>)) #F #F #F)
     (lambda (vec state)
       (vector-ref vec (vector-ref state 0)))))
  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list `((LIST ,<list>) (STATE ,<object>)) #F #F #F)
     (lambda (list state)
       list				; Ignored
       (if (pair? state)
	   (car state)
	   state))))			; If reached dotted list end...
  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list `((NULL ,<empty-list>) (STATE ,<sequence>)) #F #F #F)
     (lambda (emp-list state) emp-list state #F)))
  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list
      `((BYTE-STRING ,<byte-string>) (STATE ,<sequence>)) #F #F #F)
     (lambda (string state)
       (string-ref string (vector-ref state 0)))))
  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list `((DEQUE ,<deque>) (STATE ,<object>)) #F #F #F)
     (lambda (deque state)
       deque				; not used
       (deque-entry.value state))))
  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list `((RANGE ,<range>) (STATE ,<object>)) #F #F #F)
     (lambda (range state)
       range				; not used
       state)))
  (add-method dylan:current-element
    (dylan::function->method
     (make-param-list `((TABLE ,<table>) (STATE ,<object>)) #F #F #F)
     (lambda (table state)
       table				; Ignored
       (cadr (car (cadr state))))))

  (set! dylan:copy-state
    (dylan::generic-fn 'copy-state one-collection-and-a-state
      (lambda (collection state)
	collection			; unused
	state)))
  )					; End of Iteration Functions




;; Iterate-Until: given a fn and a collection, iterate until
;; the collection runs out of elements or fn returns a non-#F value.
(define (iterate-until fn collection)
  (let loop ((state (dylan-call dylan:initial-state collection)))
    (cond ((not state) #F)
	  ((fn (dylan-call dylan:current-element collection state)))
	  (else (loop
		 (dylan-call dylan:next-state collection state))))))

(define (iterate->list fn collection)
  (let loop ((state (dylan-call dylan:initial-state collection))
	     (value '()))
    (if state
	(let ((new-value
	       (fn (dylan-call dylan:current-element collection state))))
	  (loop (dylan-call dylan:next-state collection state)
		(cons new-value value)))
	(reverse value))))

;;;; The Iteration Protocol (page 122)
;;;; Actually, these are internal procedures used elsewhere to iterate
;;;; over collections or sets of collections

(define (collections-iterate fn done? default-value collections)
  ;; FN is a Dylan function to be applied to parallel elements from
  ;;    each collection.
  ;; DONE? is a scheme function applied to the result of FN to test
  ;;    for loop completion.  It returns #F to continue the iteration,
  ;;    or a (Scheme) thunk to return the value.
  ;; DEFAULT-VALUE is returned if any collection runs out before the
  ;;    DONE? test causes an exit.
  (if (not (all?
	    (lambda (collection)
	      (subclass? (get-type collection) <collection>))
	    collections))
      (dylan-call dylan:error
		  "do -- not all arguments are collections" collections))
  (let loop ((states
	      (map (lambda (collection)
		     (dylan-call dylan:initial-state collection))
		   collections)))
    (if (any? (lambda (x) (not x)) states)
	default-value
	(let ((ins (map (lambda (collection state)
			  (dylan-call dylan:current-element collection state))
			collections states)))
	  (let* ((next-val (dylan-apply fn ins))
		 (result? (done? next-val)))
	    (if result?
		(result?)
		(loop (map (lambda (collection state)
			     (dylan-call dylan:next-state
					 collection
					 state))
			   collections states))))))))

(define (iterate! fn collection)
  (let loop ((state (dylan-call dylan:initial-state collection)))
    (if state
	(begin
	  (fn (dylan-call dylan:current-element collection state))
	  (loop (dylan-call dylan:next-state collection state))))))

(define (find-next-non-empty-bucket hash-table index)
  (let ((table-length (vector-length hash-table)))
    (let loop ((i (+ index 1)))
      (cond ((>= i table-length) #F)
	    ((null? (vector-ref hash-table i)) (loop (+ i 1)))
	    (else i)))))

;;
;; DYLAN:GET-STATE: given a collection and a state index, return the
;;                  corresponding state.
;;                  initial-state = 0
;;                  If no corresponding state, return #F
;;
(define dylan:get-state
  (dylan::generic-fn 'get-state
    (make-param-list `((COLLECTION ,<collection>) (INDEX ,<number>)) #F #F #F)
    (lambda (collection index)
      (do ((i 0 (+ i 1))
	   (state (dylan-call dylan:initial-state collection)
		  (dylan-call dylan:next-state collection state)))
	  ((or (= i index) (not state)) state)))))


;;;;
;;;; Collection Keys (page 123 )
;;;;

(define dylan:element
  (dylan::generic-fn
   'element
   (make-param-list `((COLLECTION ,<collection>) (KEY ,<object>))
		    #F #F '(default:))
   #F))

(add-method
 dylan:element
 (dylan::dylan-callable->method
  (make-param-list `((COLLECTION ,<collection>) (KEY ,<object>))
		   #F #F '(default:))
  (lambda (multiple-values next-method coll key . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(default:))
    (dylan-call dylan:error
		"element -- not specialized for this type" coll key rest))))

(add-method
 dylan:element
 (dylan::dylan-callable->method
  (make-param-list `((EXPLICIT-KEY-COLLECTION ,<explicit-key-collection>)
		     (KEY ,<object>))
		   #F #F '(default:))
  (lambda (multiple-values next-method exp-coll key . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(default:))
    (let* ((default-marker (cons 1 2))
	   (default (dylan::find-keyword rest 'default (lambda ()
							 default-marker))))
      (do ((state (dylan-call dylan:initial-state exp-coll)
		  (dylan-call dylan:next-state exp-coll state)))
	  ((or (not state)
	       (dylan-call dylan:=
			   (dylan-call dylan:current-key exp-coll state)
			   key))
	   (cond (state (dylan-call dylan:current-element exp-coll state))
		 ((eq? default default-marker)
		  (dylan-call dylan:error
			      "element -- no such element in collection"
			      exp-coll key rest))
		 (else default))))))))

(add-method
 dylan:element
 (dylan::dylan-callable->method
  (make-param-list `((SEQUENCE ,<sequence>) (KEY ,<integer>))
		   #F #F '(default:))
  (lambda (multiple-values next-method seq key . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(default:))
    (let* ((default-marker (cons 1 2))
	   (default (dylan::find-keyword rest 'default (lambda ()
							 default-marker))))
      (do ((state (dylan-call dylan:initial-state seq)
		  (dylan-call dylan:next-state seq state))
	   (k 0 (+ k 1)))
	  ((or (not state) (= k key))
	   (cond (state (dylan-call dylan:current-element seq state))
		 ((eq? default default-marker)
		  (dylan-call dylan:error
			      "element -- no such element in collection"
			      seq key rest))
		 (else default))))))))

(define dylan:key-sequence
  (dylan::generic-fn 'key-sequence
    one-collection
    (lambda (collection)
      (dylan-call dylan:error
		  "key-sequence -- not defined for this collection type"
		  collection))))

(add-method dylan:key-sequence
  (dylan::function->method
    (make-param-list
     `((EXPLICIT-KEY-COLLECTION ,<explicit-key-collection>)) #F #F #F)
    (lambda (exp-coll)
      (do ((state (dylan-call dylan:initial-state exp-coll)
		  (dylan-call dylan:next-state exp-coll state))
	   (keys '() (cons (dylan-call dylan:current-key exp-coll state)
			   keys)))
	  ((not state) (reverse keys))))))

(add-method dylan:key-sequence
  (dylan::function->method
    one-sequence
    (lambda (seq)
      (let ((size (dylan-call dylan:size seq)))
	(if (not size)
	    seq				; Must be unbounded range
	    (dylan-call dylan:range
			'from: 0
			'through: (- size 1)
			'by: 1))))))

(define dylan:current-key
  (dylan::generic-fn 'current-key
    (make-param-list
     `((COLLECTION ,<explicit-key-collection>) (STATE ,<object>)) #F #F #F)
    (lambda (collection state)
      (dylan-call dylan:error
		  "current-key -- not implemented for this collection type"
		  collection state))))


(add-method dylan:current-key
  (dylan::function->method
    (make-param-list `((TABLE ,<table>) (STATE ,<object>)) #F #F #F)
    (lambda (table state)
      table
      (car (car (cadr state))))))
