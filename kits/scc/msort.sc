; #| -*-Scheme-*-
; 
; $Header: msort.scm,v 14.1 88/06/13 11:47:52 GMT cph Rel $
; 
; Copyright (c) 1988 Massachusetts Institute of Technology
; 
; This material was developed by the Scheme project at the Massachusetts
; Institute of Technology, Department of Electrical Engineering and
; Computer Science.  Permission to copy this software, to redistribute
; it, and to use it for any purpose is granted, subject to the following
; restrictions and understandings.
; 
; 1. Any copy made of this software must include this copyright notice
; in full.
; 
; 2. Users of this software agree to make their best efforts (a) to
; return to the MIT Scheme project any improvements or extensions that
; they make, so that these may be included in future releases; and (b)
; to inform MIT of noteworthy uses of this software.
; 
; 3. All materials developed as a consequence of the use of this
; software shall duly acknowledge such use, in accordance with the usual
; standards of acknowledging credit in academic research.
; 
; 4. MIT has made no warrantee or representation that the operation of
; this software will be error-free, and MIT is under no obligation to
; provide any services, by way of maintenance, update, or otherwise.
; 
; 5. In conjunction with products arising from the use of this material,
; there shall be no use of the name of the Massachusetts Institute of
; Technology nor of any adaptation thereof in any advertising,
; promotional, or sales literature without prior written consent from
; MIT in each case. |#

;;;; Merge Sort
;;; package: ()

; (declare (usual-integrations))

;; Functional and unstable

(define (sort obj pred)
  (define (loop l)
    (if (and (pair? l) (pair? (cdr l)))
	(split l '() '())
	l))

  (define (split l one two)
    (if (pair? l)
	(split (cdr l) two (cons (car l) one))
	(merge (loop one) (loop two))))

  (define (merge one two)
    (cond ((null? one) two)
	  ((pred (car two) (car one))
	   (cons (car two)
		 (merge (cdr two) one)))
	  (else
	   (cons (car one)
		 (merge (cdr one) two)))))

  (cond ((or (pair? obj) (null? obj))
	 (loop obj))
	((vector? obj)
	 (sort! (vector-copy obj) pred))
	(else
	 (error "sort: argument should be a list or vector" obj))))

;; This merge sort is stable for partial orders (for predicates like
;; <=, rather than like <).

(define (sort! v pred)
  (define (sort-internal! vec temp low high)
    (if (< low high)
	(let* ((middle (quotient (+ low high) 2))
	       (next (+ 1 middle)))
	  (sort-internal! temp vec low middle)
	  (sort-internal! temp vec next high)
	  (let loop ((p low) (p1 low) (p2 next))
	    (if (not (> p high))
		(cond ((> p1 middle)
		       (vector-set! vec p (vector-ref temp p2))
		       (loop (+ 1 p) p1 (+ 1 p2)))
		      ((or (> p2 high)
			   (pred (vector-ref temp p1)
				 (vector-ref temp p2)))
		       (vector-set! vec p (vector-ref temp p1))
		       (loop (+ 1 p) (+ 1 p1) p2))
		      (else
		       (vector-set! vec p (vector-ref temp p2))
		       (loop (+ 1 p) p1 (+ 1 p2)))))))))

  (if (not (vector? v))
      (error "sort!: argument not a vector" v))

  (sort-internal! v
		  (vector-copy v)
		  0
		  (- (vector-length v) 1))
  v)
