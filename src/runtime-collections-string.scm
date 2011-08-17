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

; $Id: runtime-collections-string.scm,v 1.17 1992/08/31 05:26:50 birkholz Exp $

;;;; Specializations for string and byte-string types.

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <byte-string>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class
     (if (dylan-call dylan:instance? collection <byte-string>)
	 collection
	 (let* ((size (dylan-call dylan:size collection))
		(new-string (make-string size)))
	   (do ((state (dylan-call dylan:initial-state collection)
		       (dylan-call dylan:next-state collection state))
		(index 0 (+ index 1)))
	       ((not state) new-string)
	     (let ((cur-element
		    (dylan-call dylan:current-element collection state)))
	       (string-set! new-string index
			    (dylan-call
			     dylan:as <character> cur-element)))))))))

(add-method dylan:as
  (dylan::function->method
   (make-param-list `((CLASS ,(dylan::make-singleton <string>))
		      (COLLECTION ,<collection>)) #F #F #F)
   (lambda (class collection)
     class
     (if (dylan-call dylan:instance? collection <string>)
	 collection
	 (dylan-call dylan:as <byte-string> collection)))))

;;;
;;; BYTE-STRING MAKE yields a Scheme string
;;;
;;; size keyword overrides inherited dimensions keyword (which is not used)
;;;
(add-method
 dylan:make
 (dylan::dylan-callable->method
  (make-param-list `((STRING ,(dylan::make-singleton <byte-string>)))
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
		      "(make (singleton <string>)) -- size: invalid" size))
      (if (and have-fill? (not (char? fill)))
	  (dylan-call dylan:error
		      "(make (singleton <string>)) -- fill: not a character"
		      fill))
      (if have-fill?
	  (make-string size fill)
	  (make-string size))))))

;;;
;;; STRING SPECIALIZED MAKE
;;; And we'll make <string> turn into <byte-string>
;;;
(add-method dylan:make
  (dylan::function->method
   (make-param-list `((STRING ,(dylan::make-singleton <string>)))
		    #F #F #T)
   (lambda (class . rest)
     class				; Ignored
     (dylan-apply dylan:make <byte-string> rest))))


;;;
;;; UNICODE-STRING SPECIALIZED MAKE
;;; <unicode-string> not supported
;;;
(add-method dylan:make
  (dylan::function->method
   (make-param-list `((UNICODE
		       ,(dylan::make-singleton <unicode-string>)))
		    #F #F #T)
   (lambda (class . rest)
     class				; Ignored
     rest				; Ignored
     (dylan-call dylan:error
		 "(make (singleton <unicode-string>)) -- not supported"))))

;;;
;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;
(add-method dylan:size
  (one-arg 'STRING <byte-string>
    (lambda (string) (string-length string))))

(add-method dylan:class-for-copy
  (dylan::function->method one-string (lambda (x) x <byte-string>)))

;;;;
;;;; Functions for Sequences (page 104)
;;;;
(add-method dylan:add
  (dylan::function->method one-byte-string-and-an-object
    (lambda (the-string new-element)
      (if (char? new-element)
	  (string-append (string new-element) the-string)
	  (dylan-call dylan:error "(add <byte-string> <object>) -- cannot add a non-character to string" the-string new-element)))))

(add-method dylan:concatenate
  (dylan::function->method
    (make-param-list `((BYTE-STRING ,<byte-string>)) #F 'REST #F)
    (lambda (string-1 . rest)
      (let loop ((result string-1)
		 (rest-strings
		  (map (lambda (seq)
			 (dylan-call dylan:as <byte-string> seq))
		       rest)))
	(if (null? rest-strings)
	    result
	    (loop (string-append result (car rest-strings))
		  (cdr rest-strings)))))))

(add-method dylan:concatenate
  (dylan::function->method
    (make-param-list `((STRING ,<string>)) #F 'REST #F)
    (lambda (string-1 . rest)
      (dylan-call dylan:apply dylan:concatenate string-1 rest))))

(add-method dylan:reverse
  (dylan::function->method one-byte-string
    (lambda (string-1)
      (let ((result (make-string (string-length string-1))))
	(do ((from (- (string-length string-1) 1) (- from 1))
	     (to 0 (+ to 1)))
	    ((< from 0) result)
	  (string-set! result to (string-ref string-1 from))
	  result)))))

(add-method dylan:reverse!
  (dylan::function->method one-byte-string
    (lambda (string-1)
      (do ((from (- (string-length string-1) 1) (- from 1))
	   (to 0 (+ to 1)))
	  ((<= from to) string-1)
	(let ((to-char (string-ref string-1 to)))
	  (string-set! string-1 to (string-ref string-1 from))
	  (string-set! string-1 from to-char))))))

(add-method dylan:first
  (dylan::function->method one-string
    (lambda (string)
      (if (= (string-length string) 0)
	  (dylan-call dylan:error "(first <string>) -- string is empty" string)
	  (string-ref string 0)))))

(add-method dylan:first
  (dylan::function->method one-byte-string
    (lambda (string)
      (if (= (string-length string) 0)
	  (dylan-call dylan:error
		      "(first <byte-string>) -- byte-string is empty" string)
	  (string-ref string 0)))))

(add-method dylan:second
  (dylan::function->method one-string
    (lambda (string)
      (if (< (string-length string) 2)
	  (dylan-call dylan:error
		      "(second <string>) -- string doesn't have 2 elements"
		      string)
	  (string-ref string 1)))))

(add-method dylan:second
  (dylan::function->method one-byte-string
    (lambda (string)
      (if (< (string-length string) 2)
	  (dylan-call dylan:error
		      "(second <string>) -- string doesn't have 2 elements"
		      string)
	  (string-ref string 1)))))

(add-method dylan:third
  (dylan::function->method one-string
    (lambda (string)
      (if (< (string-length string) 3 )
	  (dylan-call dylan:error
		      "(third <string>) -- string doesn't have 3 elements"
		      string)
	  (string-ref string 2)))))

(add-method dylan:third
  (dylan::function->method one-byte-string
    (lambda (string)
      (if (< (string-length string) 3 )
	  (dylan-call dylan:error
		      "(third <byte-string>) -- string doesn't have 3 elements"
		      string)
	  (string-ref string 2)))))

(add-method dylan:last
  (dylan::function->method one-string
    (lambda (string)
      (let ((sl (string-length string)))
	(if (zero? sl)
	    (dylan-call dylan:error "(last <string>) -- string is empty" string)
	    (string-ref string (- sl 1)))))))

(add-method dylan:last
  (dylan::function->method one-byte-string
    (lambda (string)
      (let ((sl (string-length string)))
	(if (zero? sl)
	    (dylan-call dylan:error
			"(last <byte-string>) -- byte-string is empty" string)
	    (string-ref string (- sl 1)))))))

;;;; Operations on Strings (page 119)

(add-method dylan:binary< (dylan::function->method two-strings string<?))

;; Generic function dylan:as-lowercase defined in runtime-functions.scm.

(add-method
 dylan:as-lowercase
 (dylan::function->method
  (make-param-list `((BYTE-STRING ,<byte-string>)) #F #F #F)
  (lambda (string)
    (list->string (map (lambda (char) (char-downcase char))
		       (string->list string))))))

(define dylan:as-lowercase!
  (dylan::generic-fn 'as-lowercase! one-string #F))

(add-method
 dylan:as-lowercase!
 (dylan::function->method
  (make-param-list `((BYTE-STRING ,<byte-string>)) #F #F #F)
  (lambda (string)
    (do ((index 0 (+ index 1)))
	((>= index (string-length string)) string)
      (string-set! string index (char-downcase (string-ref string index)))))))

;; Generic function dylan:as-uppercase defined in runtime-functions.scm.

(add-method
 dylan:as-uppercase
 (dylan::function->method
  (make-param-list `((BYTE-STRING ,<byte-string>)) #F #F #F)
  (lambda (string)
    (list->string (map (lambda (char) (char-upcase char))
		       (string->list string))))))

(define dylan:as-uppercase!
  (dylan::generic-fn 'as-uppercase! one-string #F))

(add-method
 dylan:as-uppercase!
 (dylan::function->method
  (make-param-list `((BYTE-STRING ,<byte-string>)) #F #F #F)
  (lambda (string)
    (do ((index 0 (+ index 1)))
	((>= index (string-length string)) string)
      (string-set! string index (char-upcase (string-ref string index)))))))


(add-method dylan:previous-state	; Not specified in the manual
  (dylan::function->method
   (make-param-list `((STRING ,<string>) (STATE ,<number>)) #F #F #F)
   (lambda (string offset)
     string				; unused
     (if (= offset 0) #F (- offset 1)))))


;;;
;;; Collection Keys
;;;
(add-method
 dylan:element
 (dylan::dylan-callable->method
  (make-param-list `((BYTE-STRING ,<byte-string>) (INDEX ,<integer>))
		   #F #F '(default:))
  (lambda (multiple-values next-method string-value index . rest)
    multiple-values
    (dylan::keyword-validate next-method rest '(default:))
    (let ((size (string-length string-value)))
      (if (and (>= index 0) (< index size))
	  (string-ref string-value index)
	  (dylan::find-keyword
	   rest '(default:)
	   (lambda ()
	     (dylan-call dylan:error "(element <byte-string> <integer>) -- invalid index with no default value" string-value index))))))))

;;;
;;; Mutable Collections
;;;
(add-method dylan:setter/current-element/
  (dylan::function->method
    (make-param-list
     `((BYTE-STRING ,<byte-string>) (STATE ,<object>) (new-value ,<object>))
       #F #F #F)
    (lambda (the-string state new-value)
      (string-set! the-string (vector-ref state 0) new-value)
      new-value)))

(add-method dylan:setter/element/
  (dylan::function->method
    (make-param-list
     `((BYTE-STRING ,<byte-string>) (INDEX ,<object>) (NEW-VALUE ,<object>))
     #F #F #F)
    (lambda (string index new-value)
      (string-set! string index new-value)
      new-value)))
