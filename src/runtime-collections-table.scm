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

; $Id: runtime-collections-table.scm,v 1.16 1992/08/31 05:30:12 birkholz Exp $

;;;; Specializations for tables.

(add-method dylan:shallow-copy
  (dylan::function->method
    (make-param-list `((TABLE ,<table>)) #F #F #F)
    (lambda (table)
      (let* ((new-table (dylan-call dylan:make <table>))
	     (key-sequence (dylan-call dylan:key-sequence table)))
	(do ((state (dylan-call dylan:initial-state key-sequence)
		    (dylan-call dylan:next-state key-sequence state)))
	    ((not state)
	     (dylan-call dylan:as
			 (dylan-call dylan:class-for-copy table)
			 new-table))
	  (let ((key (dylan-call dylan:current-element key-sequence state)))
	    (dylan-call dylan:setter/element/
			new-table
			key
			(dylan-call dylan:element table key))))))))
(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <table>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class
     (if (dylan-call dylan:instance? collection <table>)
	 collection
	 (let ((table (dylan-call dylan:make <table>))
	       (key-sequence (dylan-call dylan:key-sequence collection)))
	   (do ((state (dylan-call dylan:initial-state key-sequence)
		       (dylan-call dylan:next-state key-sequence state)))
	       ((not state) table)
	     (let ((cur-element
		    (dylan-call dylan:current-element key-sequence state)))
	       (dylan-call dylan:setter/element/ table cur-element
			   (dylan-call
			    dylan:element collection cur-element)))))))))



;;;
;;; TABLE SPECIALIZED MAKE
;;; <table> creates an empty hash table
;;;
(define *HASH-TABLE-SIZE* 500)

(define dylan:get-hash-table "define dylan:get-hash-table")
(define dylan:set-hash-table! "define dylan:set-hash-table!")
(create-private-slot <table> <vector> "internal-hash-table"
  (lambda (set get)
    (set! dylan:set-hash-table! set)
    (set! dylan:get-hash-table get)))
(add-method dylan:make
  (dylan::function->method
   (make-param-list `((TABLE ,(dylan::make-singleton <table>))) #F #F #T)
   (lambda (class . rest)
     class				; ignored
     rest				; ignored
     (let ((instance (dylan::make-<object> <table>)))
       (dylan-call dylan:set-hash-table! instance
	      (make-vector *hash-table-size* '()))
       instance))))

;;;;
;;;; Operations on Tables (page 120)
;;;;
(define (dylan-assoc key alist)		; Use dylan:binary= to compare keys
  (let loop ((rest-alist alist))
    (if rest-alist
	(if (dylan-call dylan:binary= key (caar rest-alist))
	    (car rest-alist)
	    (loop (cdr rest-alist)))
	#F)))

(define dylan:remove-key!
  (dylan::generic-fn 'remove-key!
    (make-param-list `((TABLE ,<table>) (KEY ,<object>)) #F #F #F)
    (lambda (table key)
      (let* ((hash-table (dylan-call dylan:get-hash-table table))
	     (hash-index (dylan-call dylan:=hash key))
	     (hash-entry (vector-ref hash-table hash-index))
	     (match (dylan-assoc key hash-entry)))
	(if match
	    (vector-set! hash-table hash-index
			 (dylan-call dylan:remove hash-entry match))
	    'no-match)))))


(add-method dylan:setter/element/
  (dylan::function->method
    (make-param-list
     `((TABLE ,<table>) (KEY ,<object>) (NEW-VALUE ,<object>)) #F #F #F)
    (lambda (table key new-value)
      (let ((hash-index (remainder (dylan-call dylan:=hash key)
				   *HASH-TABLE-SIZE*))
	    (hash-table (dylan-call dylan:get-hash-table table)))
	(if (>= hash-index (vector-length hash-table))
	    (dylan-call dylan:error "((setter element) <table> <object> <object>) -- internal error, size out of sync" table hash-index key new-value)
	    (let* ((hash-entry (vector-ref hash-table hash-index))
		   (match (dylan-assoc key hash-entry)))
	      (if match
		  (set-cdr! match (list new-value))
		  (vector-set! hash-table hash-index
			       (cons (list key new-value)
				     hash-entry)))
	      new-value))))))



(define (grow-vector-by v increase)
  (let* ((n-old-values (vector-length v))
	 (new-v (make-vector (+ n-old-values increase))))
    (vector-iterate v
      (lambda (i entry) (vector-set! new-v i entry)))
      new-v))


(add-method dylan:map-into
  (dylan::function->method
    (make-param-list
     `((TABLE ,<table>) (PROCEDURE ,<function>) (COLLECTION ,<collection>))
     #F 'REST #F)
    (lambda (table proc coll-1 . rest)
      (let loop ((key-sequence (dylan-call dylan:key-sequence coll-1))
		 (rest-coll rest))
	(if rest-coll
	    (loop (dylan-call dylan:intersection key-sequence
			      (dylan-call dylan:key-sequence (car rest-coll)))
			 (cdr rest-coll))
	    (let ((all-collections (cons coll-1 rest)))
	      (do ((state (dylan-call dylan:initial-state key-sequence)
			  (dylan-call dylan:next-state key-sequence state)))
		  ((not state) table)
		(let ((current-key (dylan-call
				    dylan:current-element key-sequence state)))
		  (dylan-call
		   dylan:setter/element/
		   table
		   current-key
		   (dylan-call dylan:apply proc
			       (map (lambda (coll)
				      (dylan-call
				       dylan:element coll current-key))
				    all-collections)))))))))))

;;;
;;; Mutable Collections
;;;

(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((TABLE ,<table>) (STATE ,<object>) (new-value ,<object>)) #F #F #F)
    (lambda (table state new-value)
      (dylan-call dylan:setter/element/
		  table
		  (dylan-call dylan:current-key table state)
		  new-value))))
