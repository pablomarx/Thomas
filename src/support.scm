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

; $Id: support.scm,v 1.20 1992/09/08 11:43:43 birkholz Exp $

;;;; General Supporting Procedures

;;; This file contains general support routines used at runtime by Dylan
;;; procedures.  See also the files generic.scm (generic operator
;;; dispatch), class.scm (class heterarchy), and runtime.scm
;;; (Dylan-callable operations written in Scheme).

(define the-unassigned-value '<<THE-UNASSIGNED-VALUE>>)
(define *the-uninitialized-slot-value* (list '<<UNINITIALIZED>>))
(define next-method:not-generic #T)	; Passed as default
					; next-method parameter when
					; not going through generic
					; dispatch mechanism

;; Create a new symbol from two strings (prefix and postfix) and an
;; existing symbol or string.

(define (new-name prefix name postfix)
  (string->symbol
   (canonicalize-string-for-symbol
    (list->string
     (append (string->list prefix)
	     (string->list ((if (string? name)
				canonicalize-string-for-symbol
				symbol->string)
			    name))
	     (string->list postfix))))))

;;;; Slot descriptors

(define slot-type
  (make-record-type
   'dylan-slot
   '(debug-name
     getter
     setter
     type
     init-value
     has-init-value?
     init-function
     init-keyword
     required-init-keyword
     allocation
     inherited?
     data-location)))			; Depends on allocation type:
					;  Instance = offset in instance data
					;  Class = (class-ptr . offset)
					;  Each-Subclass = offset in class-data
					;  Virtual = #F
					;  Constant = the value of the slot
(define slot? (record-predicate slot-type))
(define make-slot (record-constructor slot-type))
(define slot.debug-name
  (record-accessor slot-type 'debug-name))
(define slot.getter
  (record-accessor slot-type 'getter))
(define slot.setter
  (record-accessor slot-type 'setter))
(define slot.type
  (record-accessor slot-type 'type))
(define slot.init-value
  (record-accessor slot-type 'init-value))
(define slot.has-initial-value?
  (record-accessor slot-type 'has-init-value?))
(define slot.init-function
  (record-accessor slot-type 'init-function))
(define slot.init-keyword
  (record-accessor slot-type 'init-keyword))
(define slot.required-init-keyword
  (record-accessor slot-type 'required-init-keyword))
(define slot.allocation
  (record-accessor slot-type 'allocation))
(define slot.inherited?
  (record-accessor slot-type 'inherited?))
(define slot.data-location
  (record-accessor slot-type 'data-location))
(define set-slot.debug-name!
  (record-updater slot-type 'debug-name))
(define set-slot.getter!
  (record-updater slot-type 'getter))
(define set-slot.setter!
  (record-updater slot-type 'setter))
(define set-slot.type!
  (record-updater slot-type 'type))
(define set-slot.init-value!
  (record-updater slot-type 'init-value))
(define set-slot.has-initial-value?!
  (record-updater slot-type 'has-init-value?))
(define set-slot.init-function!
  (record-updater slot-type 'init-function))
(define set-slot.init-keyword!
  (record-updater slot-type 'init-keyword))
(define set-slot.required-init-keyword!
  (record-updater slot-type 'required-init-keyword))
(define set-slot.allocation!
  (record-updater slot-type 'allocation))
(define set-slot.inherited?!
  (record-updater slot-type 'inherited?))
(define set-slot.data-location!
  (record-updater slot-type 'data-location))

;;;; Keywords and names

(define (keyword? obj)
  (and (symbol? obj)
       (let ((string (symbol->string obj)))
	 (char=? #\: (string-ref string
				 (- (string-length string) 1))))))

(define (dylan::find-keyword keyword-list keyword default-fn)
  (let loop ((rest keyword-list))
    (cond ((null? rest) (default-fn))
	  ((eq? keyword (car rest)) (cadr rest))
	  (else (loop (cddr rest))))))

(define (validate-keywords arglist allowed error)
  (let loop ((args arglist))
    (cond ((null? args) #T)
	  ((or (not (pair? args))
	       (not (pair? (cdr args)))
	       (not (keyword? (car args))))
	   (error "incorrect keyword format" arglist allowed))
	  ((not (or (eq? allowed #T)
		    (memq (car args) allowed)))
	   (error "keyword not allowed"
		  arglist (if (eq? allowed #T) 'ANY allowed)))
	  (else (loop (cddr args))))))

;;;; General utilities

(define (last l)
  (if (null? l)
      '()
      (let loop ((l l))
	(if (null? (cdr l))
	    (car l)
	    (loop (cdr l))))))

(define (split-last l continue)
  (if (null? l)
      (continue '() '())
      (let loop ((previous '())
		 (left l))
	(if (null? (cdr left))
	    (continue (reverse previous) left)
	    (loop (cons (car left) previous) (cdr left))))))

(define (subset? smaller larger)
  (let loop
      ((smaller smaller))
    (if (pair? smaller)
	(if (memq (car smaller) larger)
	    (loop (cdr smaller))
	    #F)
	#T)))

(define (unique? objects predicate)
  (let loop ((objects objects))
    (or (null? objects)
	(and (not (predicate (car objects) (cdr objects)))
	     (loop (cdr objects))))))

(define (set-difference main subtract predicate)
  (let loop ((result '())
	     (to-do main))
    (cond ((null? to-do) (reverse result))
	  ((predicate (car to-do) subtract)
	   (loop result (cdr to-do)))
	  (else (loop (cons (car to-do) result) (cdr to-do))))))

(define (union set1 set2 predicate)
  (let loop ((result set1)
	     (remaining set2))
    (if (null? remaining)
	result
	(loop (if (predicate (car remaining) result)
		  result
		  (cons (car remaining) result))
	      (cdr remaining)))))

(define (adjoin elem set predicate)
  (if (predicate elem set)
      set
      (cons elem set)))

(define (any? fn l)
  (and (not (null? l))
       (or (fn (car l))
	   (any? fn (cdr l)))))

(define (all? fn l)
  (or (null? l)
      (and (fn (car l))
	   (all? fn (cdr l)))))

(define (but-first count list)
  (if (= count 0)
      list
      (but-first (- count 1) (cdr list))))

(define (population->list population)
  (map-over-population population (lambda (x) x)))
