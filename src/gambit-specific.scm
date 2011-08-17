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


;;;; This file contains the definitions of all functions used in the
;;;; implementation of Dylan which aren't part of R4RS.

;;; Populations

(load "poplat.scm")

;;; Hash tables that use weak links for objects

(load "hash.scm")

;;; Stuff for use in exception code (exception.scm)

(load "exceptions.scm")

;;; For conversion from strings to symbols, we need a function that
;;; canonicalizes the case of the string.

(define canonicalize-string-for-symbol
  (let ((converter (if (char=? #\a (string-ref (symbol->string 'a) 0))
		       char-downcase
		       char-upcase)))
    (lambda (string)
      (list->string (map converter (string->list string))))))

;; Record package

(load "record.scm")

;; sort

(load "msort.scm")

;; write-line

(define (write-line x)
  (write x)
  (newline))

;; pp -- already provided

;; dynamic-wind

(load "dynwnd.scm")

;; Imaginary numbers aren't supported by all implementations, and some
;; numeric functions are optional

(define (get-+i) +i)

;; Called at compile-time only

(define (dylan::error string . args)
  (apply error (string-append "Error: " string) args))

;; Load-up

(define dylan::load load)		; Defaults to .scm

(define (implementation-specific:generate-file in-exprs out-expr)
  (define (print x) (newline) (display x))
  (define (pp-to-string exprs)
    (let ((port (open-output-string)))
      (for-each (lambda (x) (newline port) (pp x port))
		exprs)
      (let ((str (get-output-string port)))
	(close-output-port port)
	str)))
  (define (split-char-list chars continue)
    (let loop ((output '())
	       (chars chars))
      (cond ((null? chars)
	     (continue (list->string (reverse output)) '()))
	    ((char=? (car chars) #\newline)
	     (continue (list->string (reverse output)) (cdr chars)))
	    (else (loop (cons (car chars) output) (cdr chars))))))
  (define (string->strings string)
    (let loop ((output '())
	       (input (string->list string)))
      (if (null? input)
	  (reverse output)
	  (split-char-list input
	    (lambda (string rest-chars)
	      (loop (cons string output) rest-chars))))))
  (print ";;;; Input expressions:")
  (for-each (lambda (line)
	      (if (not (zero? (string-length line))) (display "; "))
	      (display line)
	      (newline))
	    (string->strings (pp-to-string in-exprs)))
  (print ";;;; Compiled output:")
  (newline)
  (print "(##declare (standard-bindings) (not safe))")
  (newline)
  (pp out-expr)
  (newline))

;; Eval

(define (implementation-specific:eval expression)
  (eval expression))

;;;; Additional Dylan bindings

(define (dylan:scheme-variable-ref mv nm variable-name)
  (eval variable-name))

(define (dylan:scheme-procedure-ref mv nm variable-name)
  (make-dylan-callable (eval variable-name)))

(define implementation-specific:additional-dylan-bindings
  `((pp (make-dylan-callable pp 1))
    (scheme-variable dylan:scheme-variable-ref)
    (scheme-procedure dylan:scheme-procedure-ref)))
