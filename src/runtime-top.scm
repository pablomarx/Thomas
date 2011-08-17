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

; $Id: runtime-top.scm,v 1.2 1992/09/11 15:30:09 jmiller Exp $

;;;; Utility procedures for the runtime system only.

(define (get-type obj)
  (cond ((instance? obj) (instance.class obj))
	((number? obj)			; Might be wrong
	 (if (real? obj)
	     (if (exact? obj)
		 (if (integer? obj)
		     <integer>
		     <ratio>)
		 <float>)
	     <complex>))
	((class? obj) <class>)
	((singleton? obj) <singleton>)
	((null? obj) <empty-list>)
	((slot? obj) <slot-descriptor>)
	((pair? obj) <pair>)
	((vector? obj) <simple-object-vector>)
	((string? obj) <byte-string>)
	((char? obj) <character>)
	((procedure? obj)
	 (cond ((dylan::generic-function? obj) <generic-function>)
	       ((dylan::method? obj) <method>)
	       (else <function>)))
	((keyword? obj) <keyword>)
	((symbol? obj) <symbol>)
	(else <object>)))

(define (dylan-list-length l)
  ; Returns >= 0  for finite proper lists
  ;          = -1 for infinite lists
  ;          = (size + 1) for improper lists, where size is the "proper list"
  ;                       portion of the list
  (define (phase-1 l1 l2 n)
    (cond ((pair? l1) (phase-2 (cdr l1) l2 (+ n 1)))
	  ((null? l1) n)
	  (else (+ n 1))))
  (define (phase-2 l1 l2 n)
    (cond ((eq? l1 l2) -1)		; Circular list.
	  ((pair? l1) (phase-1 (cdr l1) (cdr l2) (+ n 1)))
	  ((null? l1) n)
	  (else (+ n 1))))
  (phase-1 l l 0))

(define (dylan::keyword-validate next-method arglist allowed)
  (if (procedure? next-method)
      ;; Assume that the generic function has checked the content of `args'.
      #T
      (validate-keywords arglist allowed
			 (lambda args (dylan-apply dylan:error args)))))

;;;
;;; MACROS for make-param-list argument 
;;;

(define only-rest-args (make-param-list `() #F 'REST-ARGS #F))
(define function-and-arguments 
  (make-param-list `((FUNCTION ,<function>)) #F 'REST-FNS #F))
(define procedure-and-at-least-one-collection
  (make-param-list `((PROCEDURE ,<function>) (COLLECTION ,<collection>))
		   #F 'REST #F))

;; one-<xxx>
(define one-number (make-param-list `((NUMBER ,<number>)) #F #F #F))
(define one-object (make-param-list `((OBJECT ,<object>)) #F #F #F))
(define one-list (make-param-list `((LIST ,<list>)) #F #F #F))
(define one-function (make-param-list `((FUNCTION ,<function>)) #F #F #F))
(define one-real (make-param-list `((REAL ,<real>)) #F #F #F))
(define one-integer (make-param-list `((INTEGER ,<integer>)) #F #F #F))
(define one-class (make-param-list `((CLASS ,<class>)) #F #F #F))
(define one-slot (make-param-list `((SLOT ,<slot-descriptor>)) #F #F #F))
(define one-char (make-param-list `((CHARACTER ,<character>)) #F #F #F))
(define one-string (make-param-list `((STRING ,<string>)) #F #F #F))
(define one-byte-string 
  (make-param-list `((BYTE-STRING ,<byte-string>)) #F #F #F))
(define one-collection (make-param-list `((COLLECTION ,<collection>)) #F #F #F))
(define one-vector (make-param-list `((VECTOR ,<vector>)) #F #F #F))
(define one-stretchy-vector 
  (make-param-list `((STRETCHY-VECTOR ,<stretchy-vector>)) #F #F #F))
(define one-simple-object-vector
  (make-param-list `((SIMPLE-OBJECT-VECTOR ,<simple-object-vector>)) #F #F #F))
(define one-sequence (make-param-list `((SEQUENCE ,<sequence>)) #F #F #F))
(define one-deque (make-param-list `((DEQUE ,<deque>)) #F #F #F))
(define one-range (make-param-list `((RANGE ,<range>)) #F #F #F))
(define one-table (make-param-list `((TABLE ,<table>)) #F #F #F))

;; at-least-one-<xxx>
(define at-least-one-number 
  (make-param-list `((NUMBER ,<number>)) #F 'REST-ARGS #F))
(define at-least-one-function 
  (make-param-list `((FUNCTION ,<function>)) #F 'REST-FNS #F))
(define at-least-two-objects
  (make-param-list `((OBJECT-1 ,<object>) (OBJECT-2 ,<object>)) #F 'REST #F))
(define at-least-one-real (make-param-list `((REAL ,<real>)) #F 'REST-REAL #F))
(define at-least-one-list (make-param-list `((FIRST-LIST ,<list>)) #F 'REST #F))
(define at-least-one-sequence
  (make-param-list `((SEQUENCE ,<sequence>)) #F 'REST #F))

;; two-<xxx>
(define two-objects
  (make-param-list `((OBJECT-1 ,<object>) (OBJECT-2 ,<object>)) #F #F #F))
(define two-collections
  (make-param-list
   `((COLLECTION-1 ,<collection>) (COLLECTION-2 ,<collection>)) #F #F #F))
(define two-sequences
  (make-param-list 
   `((SEQUENCE-1 ,<sequence>) (SEQUENCE-2 ,<sequence>)) #F #F #F))
(define two-numbers
  (make-param-list `((NUMBER-1 ,<number>) (NUMBER-2 ,<number>)) #F #F #F))
(define two-reals
  (make-param-list `((REAL-1 ,<real>) (REAL-2 ,<real>)) #F #F #F))
(define two-integers
  (make-param-list `((INTEGER-1 ,<integer>) (INTEGER-2 ,<integer>)) #F #F #F))
(define two-ranges
  (make-param-list `((RANGE-1 ,<range>) (RANGE-2 ,<range>)) #F #F #F))
(define two-lists
  (make-param-list `((LIST-1 ,<list>) (LIST-2 ,<list>)) #F #F #F))
(define two-strings 
  (make-param-list `((STRING-1 ,<string>) (STRING-2 ,<string>)) #F #F #F))
(define two-tables
  (make-param-list `((TABLE-1 ,<table>) (TABLE-2 ,<table>)) #F #F #F))

;; one-<xxx>-and-one-<zzz>
(define one-sequence-and-an-object
  (make-param-list `((SEQUENCE ,<sequence>) (OBJECT ,<object>)) #F #F #F))
(define one-mutable-sequence-and-an-object
  (make-param-list 
   `((MUTABLE-SEQUENCE ,<mutable-sequence>) (OBJECT ,<object>)) #F #F #F))
(define one-vector-and-an-object
  (make-param-list `((VECTOR ,<vector>) (OBJECT ,<object>)) #F #F #F))
(define one-simple-object-vector-and-an-object
  (make-param-list 
   `((SIMPLE-OBJECT-VECTOR ,<simple-object-vector>) (OBJECT ,<object>)) #F #F #F))
(define one-string-and-an-object
  (make-param-list `((STRING ,<string>) (OBJECT ,<object>)) #F #F #F))
(define one-byte-string-and-an-object
  (make-param-list `((BYTE-STRING ,<byte-string>) (OBJECT ,<object>)) #F #F #F))
(define one-list-and-an-object
  (make-param-list `((LIST ,<list>) (OBJECT ,<object>)) #F #F #F))
(define one-stretchy-vector-and-an-object
  (make-param-list `((STRETCHY-VECTOR ,<stretchy-vector>) (OBJECT ,<object>)) 
		   #F #F #F))
(define one-collection-and-a-state
  (make-param-list `((COLLECTION ,<collection>) (STATE ,<object>)) #F #F #F))
(define one-deque-and-a-value
  (make-param-list `((DEQUE ,<deque>) (OBJECT ,<object>)) #F #F #F))
(define one-range-and-an-object
  (make-param-list `((RANGE ,<range>) (OBJECT ,<object>)) #F #F #F))
(define one-deque-and-an-object
  (make-param-list `((DEQUE ,<deque>) (OBJECT ,<object>)) #F #F #F))
  
;;; The File compiler.scm contains the definition of
;;; dylan::scheme-names-of-predefined-names, which must be kept up to
;;; date with the actual methods/classes/functions defined in the
;;; runtime system.
