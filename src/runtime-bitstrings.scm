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

; $Id: runtime-bitstrings.scm,v 1.7 1992/09/03 16:20:45 jmiller Exp $

;;;; Support for handling integers as bit strings

;;;
;;; UTILITY FUNCTIONS
;;;

(define (flip-bit bit) (if (= 0 bit) 1 0))

(define (negate-bits l)
  (let loop ((answer '())
	     (remainder (reverse (map flip-bit l))))
    (cond ((null? remainder) l)		; (0 0 0 0 ...) was input
	  ((= (car remainder) 0)
	   (append (reverse (cdr remainder)) '(1) answer))
	  (else (loop (cons (flip-bit (car remainder)) answer)
		      (cdr remainder))))))

(define (integer->bits integer)
  (if (negative? integer)
      (let ((nbits (negate-bits (cdr (integer->bits (- integer))))))
	(if (zero? (car nbits))
	    (cons 1 nbits)
	    nbits))
      (let loop ((bits '())
		 (value integer))
	(cond ((zero? value) (cons 0 bits))
	      ((even? value) (loop (cons 0 bits) (quotient value 2)))
	      (else (loop (cons 1 bits)
			  (quotient (- value 1) 2)))))))

(define (pad-bitstring-to n bits)
  (let ((basic-bits (length bits)))
    (if (< n basic-bits)
	(dylan-call dylan:error "bitstring internal length error"))
    (append
     (vector->list
      (make-vector (- n basic-bits) (if (null? bits) 0 (car bits))))
     bits)))

(define (integers->same-length integer1 integer2 continue)
  (let ((bits1 (integer->bits integer1))
	(bits2 (integer->bits integer2)))
    (let ((length1 (length bits1))
	  (length2 (length bits2)))
      (cond ((= length1 length2) (continue bits1 bits2))
	    ((< length1 length2)
	     (continue (pad-bitstring-to length2 bits1) bits2))
	    (else
	     (continue bits1 (pad-bitstring-to length1 bits2)))))))

(define (bits->integer bits)
  (define (unsigned->integer bits)
    (let loop ((result 0)
	       (bits bits))
      (if (null? bits)
	  result
	  (loop (+ (car bits) (* 2 result)) (cdr bits)))))
  (cond ((zero? (car bits)) (unsigned->integer (cdr bits)))
	((null? (cdr bits)) -1)
	(else (- (unsigned->integer (negate-bits (cdr bits)))))))

(define (logical-bitstr vals)
  ;; Vals is a vector '#(0/0 0/1 1/0 1/1)
  (lambda (int1 int2)
    (bits->integer
     (integers->same-length int1 int2
       (lambda (bits1 bits2)
	 (map (lambda (a b) (vector-ref vals (+ (* 2 a) b)))
	      bits1 bits2))))))

(define (logical-op-only-rest-args no-arg-value vals)
  (lambda all-integers
    (if (not all-integers)
	no-arg-value
	  (let loop ((integers-left (cdr all-integers))
		     (op-result (car all-integers)))
	    (if (null? integers-left)
		op-result
		(loop (cdr integers-left)
		      ((logical-bitstr vals) op-result (car integers-left))))))))

;;;
;;; DYLAN FUNCTIONS
;;;

(define dylan:ash
  ;; Assume (ash int count) shifts int left by count bits if count>0, right if
  ;; count<0
  (dylan::generic-fn 'ash two-integers
    (lambda (integer shift)
      (dylan-call dylan:floor (* (expt 2 shift) integer)))))

;      (cond ((or (zero? shift) (zero? integer)) integer)
;	    ((positive? shift) (* (expt 2 shift) integer))
;	    (else
;	     (let ((bits (integer->bits integer)))
;	       (if (>= (- shift) (length bits))
;		   (if (negative? integer) -1 0)
;		   (quotient integer (expt 2 (- shift))))))))))

(define dylan:logand
  (dylan::generic-fn 'logand only-rest-args
    (logical-op-only-rest-args -1 '#(0 0 0 1))))

(define dylan:logandc1
  (dylan::generic-fn 'logandc1 two-integers
    (logical-bitstr '#(0 1 0 0))))

(define dylan:logandc2
  (dylan::generic-fn 'logandc2 two-integers
    (logical-bitstr '#(0 0 1 0))))

(define dylan:logbit?
  ;; Assuming this is a bit index primitive with 0 being the low bit
  ;; and assuming it should treat the integer as sign extended
  ;; Number representation is 2's complement (sign extended)
  (dylan::generic-fn 'logbit two-integers
    (lambda (index integer)
      (if (negative? index)
	  (dylan-call dylan:error "logbit? -- negative index" index integer))
      (let* ((integer-bits (integer->bits integer))
	     (bit-index (- (length integer-bits) index 1)))
	(if (negative? bit-index)
	    (negative? integer)
	    (= (list-ref integer-bits (- (length integer-bits) index 1)) 1))))))

(define dylan:logeqv
  (dylan::generic-fn 'logeqv two-integers
    (logical-op-only-rest-args -1 '#(1 0 0 1))))

(define dylan:logior
  (dylan::generic-fn 'logior only-rest-args
    (logical-op-only-rest-args 0 '#(0 1 1 1))))

(define dylan:lognand
  (dylan::generic-fn 'lognand two-integers
    (logical-bitstr '#(1 1 1 0))))

(define dylan:lognor
  (dylan::generic-fn 'lognor two-integers
    (logical-bitstr '#(1 0 0 0))))

(define dylan:lognot
  (dylan::generic-fn 'lognot one-integer
    (lambda (integer)
      (bits->integer (map flip-bit (integer->bits integer))))))

(define dylan:logorc1
  (dylan::generic-fn 'logorc1 two-integers
    (logical-bitstr '#(1 1 0 1))))

(define dylan:logorc2
  (dylan::generic-fn 'logorc2 two-integers
    (logical-bitstr '#(1 0 1 1))))

(define dylan:logxor
  (dylan::generic-fn 'logxor two-integers
    (logical-op-only-rest-args 0 '#(0 1 1 0))))