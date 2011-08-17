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

; $Id: runtime-collections-list.scm,v 1.17 1992/08/31 05:00:58 birkholz Exp $

;;;; This file contains all the specializations for list, pair, and
;;;; empty-list type.


(add-method dylan:binary=
  (dylan::function->method
   two-lists
   (lambda (list-1 list-2)
     (let ((size-1 (dylan-call dylan:size list-1))
	   (size-2 (dylan-call dylan:size list-2)))
       (if (not (= size-1 size-2))
	   #F
	   (do ((state-1 (dylan-call dylan:initial-state list-1)
			 (dylan-call dylan:next-state list-1 state-1))
		(state-2 (dylan-call dylan:initial-state list-2)
			 (dylan-call dylan:next-state list-2 state-2)))
	       ((or (or (not state-1) (not state-2))
		    (not (dylan-call dylan:id?
				     (dylan-call dylan:current-element
						 list-1 state-1)
				     (dylan-call dylan:current-element
						 list-2 state-2))))
		(if (or state-1 state-2) #F #T))))))))

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <list>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class
     (if (dylan-call dylan:instance? collection <list>)
	 collection
	 (let loop ((state (dylan-call dylan:initial-state collection))
		    (result '()))
	   (if state
	       (loop (dylan-call dylan:next-state collection state)
		     (cons (dylan-call dylan:current-element collection state)
			   result))
	       (reverse result)))))))


(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <pair>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class
     (if (dylan-call dylan:instance? collection <pair>)
	 collection
	 (let loop ((state (dylan-call dylan:initial-state collection))
		    (result '()))
	   (if state
	       (loop (dylan-call dylan:next-state collection state)
		     (cons (dylan-call dylan:current-element collection state)
			   result))
	       (reverse result)))))))


;;;
;;; LIST SPECIALIZED MAKE
;;; supports size: and fill:
;;;
(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((LIST ,(dylan::make-singleton <list>)))
		   #F #F '(size: fill:))
  (lambda (multiple-values next-method class . rest)
    multiple-values class				; Not used
    (dylan::keyword-validate next-method rest '(size: fill:))
    (let* ((size (dylan::find-keyword rest 'size: (lambda () 0)))
	   (have-fill? #T)
	   (fill (dylan::find-keyword rest 'fill:
				      (lambda () (set! have-fill? #F) #F))))
      (if (or (not (integer? size)) (negative? size))
	  (dylan-call dylan:error
		      "make -- list size: invalid" size))
      (vector->list (make-vector size fill))))))
;;;
;;; PAIR SPECIALIZED MAKE
;;;
(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((PAIR ,(dylan::make-singleton <pair>)))
		   #F #F '(size: fill:))
  (lambda (multiple-values next-method class . rest)
    multiple-values class				; Not used
    (dylan::keyword-validate next-method rest '(size: fill:))
    (let* ((size (dylan::find-keyword rest 'size: (lambda () 0)))
	   (have-fill? #T)
	   (fill (dylan::find-keyword rest 'fill:
				      (lambda () (set! have-fill? #F) #F))))
      (if (or (not (integer? size)) (negative? size))
	  (dylan-call dylan:error "make -- list size invalid" size))
      (vector->list (make-vector size fill))))))
;;;
;;; EMPTY-LIST SPECIALIZED MAKE
;;;
(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((EMPTY-LIST ,(dylan::make-singleton <empty-list>)))
		   #F #F '(size: fill:))
  (lambda (multiple-values next-method class . rest)
    multiple-values class		; Not used
    (dylan::keyword-validate next-method rest '(size: fill:))
    '())))


;;;
;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;
(add-method dylan:size
  (one-arg 'LIST <list>
    (lambda (list)
      (let ((length (dylan-list-length list)))
	(if (= length -1)
	    #F
	    length)))))

(add-method dylan:size
  (one-arg 'PAIR <empty-list>
   (lambda (pair) pair 0)))

(add-method
 dylan:member?
 (dylan::dylan-callable->method
  (make-param-list `((VALUE ,<object>) (COLLECTION ,<list>))
		   #F #F '(test:))
  (lambda (multiple-values next-method value list . keys)
    multiple-values
    (dylan::keyword-validate next-method keys '(test:))
    (let ((test (dylan::find-keyword keys 'test:
				     (lambda () dylan:id?))))
      (let loop ((list list))
	(cond ((null? list) #F)
	      ((dylan-call test value (car list)) list)
	      (else (loop (cdr list)))))))))

;;;;
;;;; Functions for Sequences (page 104)
;;;;

(add-method dylan:add
 (dylan::function->method one-list-and-an-object
  (lambda (the-list new-element)
    (apply list new-element the-list))))

(add-method dylan:add!
 (dylan::function->method
  one-list-and-an-object
  (lambda (list-1 val)
    (dylan-call dylan:cons val list-1)))) ; Like the book (p. 117)

(add-method dylan:concatenate
  (dylan::function->method at-least-one-list
    (lambda (list-1 . rest)
      (let ((rest-lists (map (lambda (seq)
			       (dylan-call dylan:as <pair> seq))
			     rest)))
	(apply append (cons list-1 rest-lists))))))

(add-method dylan:reverse
  (dylan::function->method one-list reverse))

(add-method dylan:first
  (dylan::function->method one-list
    (lambda (list)
      (if (null? list)
	  (dylan-call dylan:error "first -- list is empty" list)
	  (car list)))))

(add-method dylan:second
  (dylan::function->method one-list
    (lambda (list)
      (if (or (null? list) (null? (cdr list)))
	  (dylan-call dylan:error "second -- list doesn't have 2 elements" list)
	  (cadr list)))))

(add-method dylan:third
  (dylan::function->method one-list
    (lambda (list)
      (if (or (null? list) (null? (cdr list))
	      (null? (cddr list)))
	  (dylan-call dylan:error "third -- list doesn't have 3 elements" list)
	  (caddr list)))))

(add-method dylan:last
  (dylan::function->method one-list
    (lambda (list)
      (if (null? list)
	  (dylan-call dylan:error "last -- list is empty")
	  (last list)))))

;;;;
;;;; Operations on LISTS (page 115)
;;;;
(define dylan:cons
  (dylan::function->method two-objects cons))

(define dylan:list
  (dylan::function->method only-rest-args list))

(define dylan:car
  (dylan::function->method
   one-list
   (lambda (list) (if (null? list) '() (car list)))))

(define dylan:cdr
  (dylan::function->method
   one-list
   (lambda (list) (if (null? list) '() (cdr list)))))

(define dylan:caar
  (dylan::function->method
   one-list
   (lambda (list) (dylan-call dylan:car (dylan-call dylan:car list)))))

(define dylan:cadr
  (dylan::function->method
   one-list
   (lambda (list) (dylan-call dylan:car (dylan-call dylan:cdr list)))))

(define dylan:cdar
  (dylan::function->method
   one-list
   (lambda (list) (dylan-call dylan:cdr (dylan-call dylan:car list)))))

(define dylan:cddr
  (dylan::function->method
   one-list
   (lambda (list) (dylan-call dylan:cdr (dylan-call dylan:cdr list)))))

(define dylan:caaar
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:car
		 (dylan-call dylan:car (dylan-call dylan:car list))))))

(define dylan:caadr
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:car
		 (dylan-call dylan:car (dylan-call dylan:cdr list))))))

(define dylan:cadar
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:car
		 (dylan-call dylan:cdr (dylan-call dylan:car list))))))

(define dylan:caddr
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:car
		 (dylan-call dylan:cdr (dylan-call dylan:cdr list))))))

(define dylan:cdaar
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:cdr
		 (dylan-call dylan:car (dylan-call dylan:car list))))))

(define dylan:cdadr
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:cdr
		 (dylan-call dylan:car (dylan-call dylan:cdr list))))))

(define dylan:cddar
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:cdr
		 (dylan-call dylan:cdr (dylan-call dylan:car list))))))

(define dylan:cdddr
  (dylan::function->method
   one-list
   (lambda (list)
     (dylan-call dylan:cdr
		 (dylan-call dylan:cdr (dylan-call dylan:cdr list))))))

(define dylan:setter/car/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object) (set-car! list object) object)))

(define dylan:setter/cdr/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object) (set-cdr! list object) object)))

(define dylan:setter/caar/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-car! (dylan-call dylan:car list) object)
     object)))

(define dylan:setter/cadr/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-car! (dylan-call dylan:cdr list) object)
     object)))

(define dylan:setter/cdar/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-cdr! (dylan-call dylan:car list) object)
     object)))

(define dylan:setter/cddr/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-cdr! (dylan-call dylan:cdr list) object)
     object)))

(define dylan:setter/caaar/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-car! (dylan-call dylan:car (dylan-call dylan:car list)) object)
     object)))

(define dylan:setter/caadr/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-car! (dylan-call dylan:car (dylan-call dylan:cdr list)) object)
     object)))

(define dylan:setter/cadar/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-car! (dylan-call dylan:cdr (dylan-call dylan:car list)) object)
     object)))

(define dylan:setter/caddr/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-car! (dylan-call dylan:cdr (dylan-call dylan:cdr list)) object)
     object)))

(define dylan:setter/cdaar/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-cdr! (dylan-call dylan:car (dylan-call dylan:car list)) object)
     object)))

(define dylan:setter/cdadr/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-cdr! (dylan-call dylan:car (dylan-call dylan:cdr list)) object)
     object)))

(define dylan:setter/cddar/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-cdr! (dylan-call dylan:cdr (dylan-call dylan:car list)) object)
     object)))

(define dylan:setter/cdddr/
  (dylan::function->method
   one-list-and-an-object
   (lambda (list object)
     (set-cdr! (dylan-call dylan:cdr (dylan-call dylan:cdr list)) object)
     object)))

(define dylan:list*
  (dylan::function->method
   only-rest-args
   (let ()
     (define (list* arg . args)
       (if (null? args)
	   arg
	   (cons arg (apply list* args))))
     (lambda args
       (if (null? args)
	   '()				; Very peculiar ?
	   (apply list* args))))))

(define dylan:append
  (dylan::generic-fn 'append at-least-one-list append))

(define dylan:find-pair
  (dylan::generic-fn
   'find-pair
   (make-param-list `((ITEM ,<object>) (LIST ,<list>)) #F #F '(test:))
   #F))

(add-method
 dylan:find-pair
 (dylan::dylan-callable->method
  (make-param-list `((ITEM ,<object>) (LIST ,<list>)) #F #F '(test:))
  (lambda (multiple-values next-method item list . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(test:))
    (let ((test-fn (dylan::find-keyword rest 'test:
					(lambda () dylan:id?))))
      (let loop ((rest-list list))
	(cond ((null? rest-list) #F)
	      ((not (pair? (car list)))
	       (dylan-call dylan:error
			   "find-pair -- list element not a pair"
			   (car rest-list)))
	      ((dylan-call test-fn item (caar rest-list))
	       (car rest-list))
	      (else (loop (cdr rest-list)))))))))



;;;
;;; Collection Keys
;;;

(add-method
 dylan:element
 (dylan::dylan-callable->method
  (make-param-list `((LIST ,<list>) (INDEX ,<integer>)) #F #F '(default:))
  (lambda (multiple-values next-method list-1 index . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(default:))
    (let ((size (length list-1)))
      (if (and (>= index 0) (< index size))
	  (list-ref list-1 index)
	  (dylan::find-keyword
	   rest
	   'default:
	   (lambda ()
	     (dylan-call dylan:error
			 "element -- invalid index with no default value"
			 list-1 index))))))))

;;;
;;; Mutable Collection
;;;

(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((LIST ,<list>) (STATE ,<object>) (new-value ,<object>)) #F #F #F)
    (lambda (list state new-value)
      list				; Ignored
      (set-car! state new-value)
      new-value)))


(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((EMPTY-LIST ,<empty-list>) (STATE ,<object>) (new-value ,<object>))
     #F #F #F)
    (lambda (empty-list state new-value)
      (dylan-call dylan:error
		  "setter/current-element/ for <empty-list>: not possible"
		  empty-list state new-value))))
