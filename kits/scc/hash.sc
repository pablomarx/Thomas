;#| -*-Scheme-*-
;
; $Header: prop1d.scm,v 14.4 89/09/15 17:16:35 GMT jinx Exp $
;
; Copyright (c) 1988, 1989 Massachusetts Institute of Technology
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

;;;; One Dimensional Property Tables
;;; package: (runtime 1d-property)

(define (initialize-oned-table-package!)
  (set! population-of-oned-tables (make-population))
  (after-gc 'oned-table gc-oned-tables!))

(define population-of-oned-tables #f)

(define (gc-oned-tables!)
  (map-over-population! population-of-oned-tables oned-table/clean!))

(define (make-oned-table)
  (let ((table (list oned-table-tag)))
    (add-to-population! population-of-oned-tables table)
    table))

(define (oned-table? object)
  (and (pair? object)
       (eq? (car object) oned-table-tag)))

(define oned-table-tag
  "1D table")

(define false-key
  "false key")

(define (weak-assq key table)
  (let loop ((previous table) (alist (cdr table)))
    (and (not (null? alist))
	 (let ((entry (car alist))
	       (next (cdr alist)))
	   (let ((key* (car entry)))
	     (cond ((not key*)
		    (set-cdr! previous next)
		    (loop previous next))
		   ((eq? key* key)
		    entry)
		   (else
		    (loop alist next))))))))

(define (oned-table/get table key default)
  (let ((entry (weak-assq (or key false-key) table)))
    (if entry
	(cdr entry)
	default)))

(define (oned-table/lookup table key if-found if-not-found)
  (let ((entry (weak-assq (or key false-key) table)))
    (if entry
	(if-found (cdr entry))
	(if-not-found))))

(define (oned-table/put! table key value)
  (let ((key (or key false-key)))
    (let ((entry (weak-assq key table)))
      (if entry
	  (set-cdr! entry value)
	  (set-cdr! table
		    (cons (weak-cons key value)
			  (cdr table))))
      #f)))

(define (oned-table/remove! table key)
  (let ((key (or key false-key)))
    (let loop ((previous table) (alist (cdr table)))
      (if (not (null? alist))
	  (let ((key* (car (car alist)))
		(next (cdr alist)))
	    (loop (if (or (not key*) (eq? key* key))
		      ;; Might as well clean whole list.
		      (begin
			(set-cdr! previous next)
			previous)
		      alist)
		  next))))))

(define (oned-table/clean! table)
  (let loop ((previous table) (alist (cdr table)))
    (if (not (null? alist))
	(let ((next (cdr alist)))
	  (loop (if (car (car alist))
		    alist
		    (begin
		      (set-cdr! previous next)
		      previous))
		next)))))

(define (oned-table/alist table)
  (let loop ((previous table) (alist (cdr table)) (result '()))
    (if (null? alist)
	result
	(let ((entry (car alist))
	      (next (cdr alist)))
	  (let ((key (car entry)))
	    (if (not key)
		(begin
		  (set-cdr! previous next)
		  (loop previous next result))
		(loop alist
		      next
		      (cons (cons (and (not (eq? key false-key)) key)
				  (cdr entry))
			    result))))))))

(define (oned-table/for-each proc table)
  (let loop ((previous table) (alist (cdr table)))
    (if (not (null? alist))
	(let ((entry (car alist))
	      (next (cdr alist)))
	  (let ((key (car entry)))
	    (if key
		(begin
		  (proc (and (not (eq? key false-key)) key)
			(cdr entry))
		  (loop alist next))
		(begin
		  (set-cdr! previous next)
		  (loop previous next))))))))

(initialize-oned-table-package!)
