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

; $Id: generic.scm,v 1.29 1992/09/10 17:35:44 birkholz Exp $

;;;;  Generic Operation Dispatch Mechanism

;;; Uses MIT Scheme 1d-tables to implement method -> param-list
;;; table lookup.  Portability requires renaming from "1d" to "oned"

;;;; Methods

(define param-list-type
  (make-record-type 'dylan-parameter-list
		    '(nrequired specializers next? rest? keys)))
(define param-list? (record-predicate param-list-type))
(define param-list.nrequired (record-accessor param-list-type 'nrequired))
(define param-list.specializers (record-accessor param-list-type 'specializers))
(define param-list.next? (record-accessor param-list-type 'next?))
(define param-list.rest? (record-accessor param-list-type 'rest?))
(define param-list.keys (record-accessor param-list-type 'keys))

(define make-param-list
  (let ((makeit (record-constructor param-list-type)))
    (lambda (required next? rest? keys)
      (let ((required (guarantee-symbol-and-specializer-list required)))
	(makeit (length required)
		(map cadr required)
		(guarantee-boolean next?)
		(guarantee-boolean rest?)
		(guarantee-keys keys))))))

(define (guarantee-symbol-and-specializer-list original-list)
  (if (all? (lambda (elt)
	      (and (pair? elt) (pair? (cdr elt)) (null? (cddr elt))
		   (symbol? (car elt))
		   (or (class? (cadr elt))
		       (singleton? (cadr elt)))))
	    original-list)
      original-list
      (guarantee-symbol-and-specializer-list
       (dylan-call dylan:error
		   "invalid param-list"
		   original-list))))

(define (guarantee-keys keys)
  (cond ((not keys) #F)
	((not (pair? keys)) '#())
	((all? keyword? keys) keys)
	(else (guarantee-keys
	       (dylan-call dylan:error
			   "invalid keywords" keys)))))

(define (guarantee-boolean bool)
  (if bool #T #F))

(define (guarantee-integer object)
  (if (integer? object)
      object
      (guarantee-integer (dylan-call dylan:error "not an integer" object))))

(define *method-data* (make-OneD-table))

(define (method-data method)
  (let ((data (OneD-table/get *method-data* method #F)))
    (if (not data)
	(dylan-call dylan:error "not a method" method)
	data)))

(define (method.param-list method)
  (method-data method))

(define (method.specializers method)
  (param-list.specializers (method-data method)))

(define (method.nrequired method)
  (param-list.nrequired (method-data method)))

(define (method.rest? method)
  (param-list.rest? (method-data method)))

(define (method.keys method)
  (param-list.keys (method-data method)))

(define (dylan::method? method)
  (if (OneD-table/get *method-data* method #F)
      #T
      #F))

(define (dylan::make-method param-list method)
  (OneD-table/put! *method-data* method (guarantee-param-list param-list))
  method)

(define (guarantee-param-list param-list)
  (if (param-list? param-list)
      param-list
      (guarantee-param-list
       (dylan-call dylan:error "not a param-list" param-list))))

;;;; Generic Functions

(define generic-function-data-type
  (make-record-type 'dylan-generic-function-data
		    '(name nrequired keys rest? methods read-only?)))

(define generic-function-data.nrequired
  (record-accessor generic-function-data-type 'nrequired))
(define generic-function-data.keys
  (record-accessor generic-function-data-type 'keys))
(define generic-function-data.rest?
  (record-accessor generic-function-data-type 'rest?))
(define generic-function-data.methods
  (record-accessor generic-function-data-type 'methods))
(define set-generic-function-data.methods!
  (record-updater generic-function-data-type 'methods))
(define generic-function-data.read-only?
  (record-accessor generic-function-data-type 'read-only?))
(define set-generic-function-data.read-only?!
  (record-updater generic-function-data-type 'read-only?))
(define make-generic-function-data
  (record-constructor generic-function-data-type))

(define *generic-function-data* (make-OneD-table))

(define (generic-function-data fn)
  (let ((data (OneD-table/get *generic-function-data* fn #F)))
    (if (not data)
	(dylan-call dylan:error "not a generic function" fn)
	data)))

(define (generic-function.nrequired generic-function)
  (generic-function-data.nrequired (generic-function-data generic-function)))

(define (generic-function.keys generic-function)
  (generic-function-data.keys (generic-function-data generic-function)))

(define (generic-function.rest? generic-function)
  (generic-function-data.rest? (generic-function-data generic-function)))

(define (generic-function.methods generic-function)
  (generic-function-data.methods (generic-function-data generic-function)))

(define (generic-function.read-only? generic-function)
  (generic-function-data.read-only? (generic-function-data generic-function)))

(define (set-generic-function.read-only?! generic-function read-only?)
  (set-generic-function-data.read-only?!
   (generic-function-data generic-function) (if read-only? #T #F)))

(define (find-method generic-function specializers)
  (let loop ((methods (generic-function.methods generic-function)))
    (if (pair? methods)
	(if (specializers=? specializers
			    (method.specializers (car methods)))
	    (car methods)
	    (loop (cdr methods)))
	#F)))

(define delete-method!
  (letrec ((delete-pair!
	    (lambda (pair list)
	      (if (eq? pair list)
		  (cdr list)
		  (let loop ((pairs list))
		    (if (pair? pairs)
			(if (eq? pair (cdr pairs))
			    (begin 
			      (set-cdr! pairs (cddr pairs))
			      list)
			    (loop (cdr pairs)))
			list))))))
    (lambda (generic-function method)
      (let* ((data (generic-function-data generic-function))
	     (pair (memq method (generic-function-data.methods data))))
	(if (not pair)
	    (dylan-call dylan:error
			"method not in generic function"
			method generic-function)
	    (set-generic-function-data.methods!
	     data (delete-pair! pair (generic-function-data.methods data)))))
      method)))

(define (Add-Method generic-function method . multi-value-receiver)
  (let ((data (generic-function-data generic-function))
	(param-list (method.param-list method)))
    (define (congruency-error)
      (cond
       ((not (= (param-list.nrequired param-list)
		(generic-function-data.nrequired data)))
	"required argument count mismatch")
       ((and (not (or (generic-function-data.rest? data)
		      (generic-function-data.keys data)))
	     (or (param-list.rest? param-list)
		 (param-list.keys param-list)))
	"generic function doesn't allow rest/keys")
       ((and (or (generic-function-data.rest? data)
		 (generic-function-data.keys data))
	     (not (or (param-list.rest? param-list)
		      (param-list.keys param-list))))
	"generic function requires rest/keys")
       (else #F)))
    (cond ((generic-function-data.read-only? data)
	   (dylan-call dylan:error
		       "add-method -- generic function is read-only"
		       generic-function))
	  ((and (pair? (generic-function-data.keys data))
		(not (let ((method-keys (param-list.keys param-list)))
		       (or (param-list.rest? param-list)
			   (and method-keys (not (pair? method-keys)))
			   (subset? (generic-function-data.keys data)
				    method-keys)))))
	   (dylan-call dylan:error
		       "add-method -- generic function requires certain keys"
		       (generic-function-data.keys data)))
	  ((congruency-error)
	   =>
	   (lambda (specific-error)
	     (dylan-call dylan:error
			 (string-append
			  "add-method -- parameter lists not congruent, "
			  specific-error)
			 generic-function method))))
    (let ((old-method (find-method generic-function
				   (method.specializers method))))
      (if old-method
	  (delete-method! generic-function old-method))
      (set-generic-function-data.methods!
       data (cons method (generic-function-data.methods data)))
      (if (not (null? multi-value-receiver))
	  ((car multi-value-receiver) method old-method)
	  old-method))))

(define (dylan::generic-function? obj)
  (if (OneD-table/get *generic-function-data* obj #F) #T #F))

(define (dylan::create-generic-function name nrequired keys rest?)
  (letrec ((the-data (make-generic-function-data
		      name
		      (guarantee-integer nrequired)
		      (guarantee-keys keys)
		      (guarantee-boolean rest?)
		      '() #F))
	   (the-function (lambda args
			   (generic-dispatch (car args) (cddr args)
					     the-function the-data))))
    (OneD-table/put! *generic-function-data* the-function the-data)
    the-function))

;;;; Generic Dispatch

(define (generic-dispatch multiple-values original-args generic-function data)
  (let ((nreq (generic-function-data.nrequired data))
	(ngiven (length original-args)))
    (if (> nreq ngiven)
	(dylan-call dylan:error
		    "generic-dispatch -- too few arguments supplied"
		    nreq original-args))
    (let ((applicable-methods
	   (sorted-applicable-methods
	    (generic-function-data.methods data)
	    original-args))
	  (non-req-args (but-first nreq original-args)))
      (if (not (pair? applicable-methods))
	  (dylan-call dylan:error
		      "generic-dispatch -- no applicable methods"
		      generic-function original-args))
      (if (> ngiven nreq)		; More supplied than required
	  (if (or (generic-function-data.keys data)
		  (generic-function-data.rest? data))
	      (check-handled-keywords non-req-args applicable-methods)
	      (dylan-call dylan:error
			  "generic-dispatch -- too many arguments supplied"
			  generic-function nreq original-args)))
      (let next-method-loop ((remaining-methods applicable-methods)
			     (multiple-values multiple-values)
			     (current-args original-args))
	(apply (car remaining-methods)
	       multiple-values
	       (if (null? (cdr remaining-methods))
		   #F
		   (lambda (multiple-values next-method . these-args)
		     next-method	; Ignored
		     (next-method-loop (cdr remaining-methods)
				       multiple-values
				       (if (null? these-args)
					   current-args
					   these-args))))
	       current-args)))))

(define (check-handled-keywords non-req-args methods)
  ;;   gather the keywords for all of the applicable methods
  ;;   if ALL methods specify !rest without !key then the call
  ;;      is allowable
  ;;   if ANY method specifies !rest (or !key with no specific
  ;;      keys), then the call is allowable provided the extra
  ;;      arguments are in keyword/value format
  ;;   otherwise all of the keywords passed must be accepted by
  (define all-!rest? #T)
  (define any-!key? #F)
  (let loop ((keywords '())
	     (methods methods))
    (if (pair? methods)
	(let* ((param-list (method.param-list (car methods)))
	       (keys (param-list.keys param-list))
	       (rest? (param-list.rest? param-list)))
	  (if (or (not rest?) keys) (set! all-!rest? #F))
	  (cond ((or (param-list.rest? param-list)
		     (and keys (not (pair? keys))))
		 (set! any-!key? #T))
		((pair? keys)
		 (loop (append keys keywords)
		       (cdr methods)))))
	(cond (all-!rest? 'OK)
	      (any-!key? (dylan::keyword-validate #T non-req-args #T))
	      (else (dylan::keyword-validate #T non-req-args keywords))))))

;;;; Finding, ranking, sorting applicable methods.

(define (sorted-applicable-methods methods arguments)
  (map cdr				; Strip rankings.
       (sort (find-applicable-method-rankings methods arguments)
	     (lambda (ranking/method-1 ranking/method-2)
	       ;; Rankings are handled left-to-right through the list.
	       (let loop ((ranking-1 (car ranking/method-1))
			  (ranking-2 (car ranking/method-2)))
		 (cond ((null? ranking-1) #T)
		       ;; MIT-Scheme bogosity forces method-applicable?
		       ;; to return #T rather than '()!!!
		       ((eq? #T ranking-1) #T)
		       ((< (car ranking-1) (car ranking-2)) #T)
		       ((> (car ranking-1) (car ranking-2)) #F)
		       (else (loop (cdr ranking-1) (cdr ranking-2)))))))))

(define (find-applicable-method-rankings methods arguments)
  ;; Returns a list of (ranking . method) for each applicable method in
  ;; `methods'.  `ranking' is a list containing the generality ranking of
  ;; each specializer of `method'.  (Singletons are given rank 0 [least
  ;; general].) These rankings are typically used to sort the applicable
  ;; methods.
  (let loop ((ranking/method-pairs '())
	     (methods methods))
    (if (not (pair? methods))
	ranking/method-pairs
	(let ((method (car methods)))
	  (let ((ranking (method-applicable? method arguments)))
	    (loop (if ranking
		      (cons (cons ranking method) ranking/method-pairs)
		      ranking/method-pairs)
		  (cdr methods)))))))

(define (method-applicable? method arguments)
  ;; Returns #F if `method' shouldn't be applied to `arguments'.  Else,
  ;; returns a generality ranking for the match.  This ranking is a list of
  ;; generality rankings computed by match-specializer? applied to each
  ;; required argument and its corresponding specializer.
  (let loop ((remaining-arguments arguments)
	     (remaining-specializers (method.specializers method))
	     (ranking '()))
    (if (pair? remaining-specializers)
	(if (not (pair? remaining-arguments))
	    (dylan-call dylan:error
			"method-applicable? -- too few arguments"
			arguments method)
	    (let ((rank (match-specializer? (car remaining-arguments)
					    (car remaining-specializers))))
	      (if rank
		  (loop (cdr remaining-arguments) (cdr remaining-specializers)
			(cons rank ranking))
		  #F)))
	;; MIT-Scheme bogosity.  (eq? #F '()) => #T!!!
	(if (null? ranking)
	    #T
	    (reverse ranking)))))

(define (match-specializer? object specializer)
  ;; Returns #F if `object' doesn't match `specializer'.  Else, returns a
  ;; generality ranking for the match.  (A high ranking indicates a very
  ;; general, unspecific match.  If the specializer is a singleton and the
  ;; object matches, the ranking is 0 [very specific].)
  (cond ((singleton? specializer)
	 (if (Id? object (singleton.object specializer))
	     0
	     #F))
	((class? specializer)
	 (if (subclass? (get-type object) specializer)
	     (class.generality specializer)
	     #F))
	(else (dylan-call dylan:error
			  "match-specializer? -- weird specializer"
			  specializer))))

(define (specializers=? specializers1 specializers2)
  ;; Returns #T when two lists of specializers contain the same specializer
  ;; in each position.  "same" is `eq?' except for singletons, which are
  ;; not guaranteed to be unique for arbitrary objects (e.g. `3').
  (let loop ((specs1 specializers1)
	     (specs2 specializers2))
    (if (and (pair? specs1)
	     (pair? specs2))
	(let ((spec1 (car specs1))
	      (spec2 (car specs2)))
	  (if (or (and (singleton? spec1)
		       (singleton? spec2)
		       (Id? (singleton.object spec1)
			    (singleton.object spec2)))
		  (and (class? spec1)
		       (class? spec2)
		       (eq? spec1 spec2)))
	      (loop (cdr specs1) (cdr specs2))
	      #F))
	(if (and (null? specs1)
		 (null? specs2))
	    #T
	    (dylan-call dylan:error
			"specializer list length mismatch"
			specializers1 specializers2)))))

;;; Dylan Calling Conventions

(define (make-dylan-callable scheme-operation . n-args)
  ;; Remove the incoming NEXT-METHOD and multiple-value arguments, so
  ;; that a standard Scheme procedure can be called from Dylan
  (if (or (null? n-args) (> (car n-args) 3) (negative? (car n-args)))
      (lambda args (apply scheme-operation (cddr args)))
      (case (car n-args)
	((0) (lambda (multi next) multi next (scheme-operation)))
	((1) (lambda (multi next a) multi next (scheme-operation a)))
	((2) (lambda (multi next a b) multi next (scheme-operation a b)))
	((3) (lambda (multi next a b c)
	       multi next
	       (scheme-operation a b c))))))

(define (dylan-call dylan-fn . args)
  ;; Fills in the missing multiple-values and next-method parameters.
  ;; This is useful if you do NOT want multiple values back!
  (case (length args)
    ((0) (dylan-fn #F NEXT-METHOD:NOT-GENERIC))
    ((1) (dylan-fn #F NEXT-METHOD:NOT-GENERIC (car args)))
    ((2) (dylan-fn #F NEXT-METHOD:NOT-GENERIC (car args) (cadr args)))
    ((3) (dylan-fn #F NEXT-METHOD:NOT-GENERIC (car args) (cadr args)
		   (caddr args)))
    ((4) (dylan-fn #F NEXT-METHOD:NOT-GENERIC (car args) (cadr args)
		   (caddr args) (cadddr args)))
    (else (apply dylan-fn #F NEXT-METHOD:NOT-GENERIC args))))

(define (dylan-full-call dylan-fn multi-value next-method . args)
  ;; Use this ONLY if you must specify multi-value or next-method
  ;; when calling from Scheme to Dylan.
  (case (length args)
    ((0) (dylan-fn multi-value next-method))
    ((1) (dylan-fn multi-value next-method (car args)))
    ((2) (dylan-fn multi-value next-method (car args) (cadr args)))
    ((3) (dylan-fn multi-value next-method
		   (car args) (cadr args) (caddr args)))
    ((4) (dylan-fn multi-value next-method
		   (car args) (cadr args) (caddr args) (cadddr args)))
    (else (apply dylan-fn multi-value next-method args))))

(define (dylan-mv-call dylan-fn multi-value . args)
  ;; Use this ONLY if you must specify multi-value
  ;; when calling from Scheme to Dylan.
  (case (length args)
    ((0) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC))
    ((1) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC (car args)))
    ((2) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC (car args) (cadr args)))
    ((3) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC
		   (car args) (cadr args) (caddr args)))
    ((4) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC
		   (car args) (cadr args) (caddr args) (cadddr args)))
    (else (apply dylan-fn multi-value NEXT-METHOD:NOT-GENERIC args))))

(define (reformat-apply-args args)
  (split-last args
	      (lambda (early end)
		(append early (if (null? end) '() (car end))))))

(define (dylan-apply dylan-fn . args)
  ;; Fills in the missing multiple-values and next-method parameters.
  ;; This is useful if you do NOT want multiple values back!
  (let ((args (reformat-apply-args args)))
    (case (length args)
      ((0) (dylan-fn #f NEXT-METHOD:NOT-GENERIC))
      ((1) (dylan-fn #f NEXT-METHOD:NOT-GENERIC (car args)))
      ((2) (dylan-fn #f NEXT-METHOD:NOT-GENERIC (car args) (cadr args)))
      ((3) (dylan-fn #f NEXT-METHOD:NOT-GENERIC
		     (car args) (cadr args) (caddr args)))
      ((4) (dylan-fn #f NEXT-METHOD:NOT-GENERIC
		     (car args) (cadr args) (caddr args) (cadddr args)))
      (else (apply dylan-fn #f NEXT-METHOD:NOT-GENERIC args)))))

(define (dylan-full-apply dylan-fn multi-value next-method . args)
  ;; You must specify the first two arguments (multiple-values and
  ;; next-method) explictly.
  (let ((args (reformat-apply-args args)))
    (case (length args)
      ((0) (dylan-fn multi-value next-method))
      ((1) (dylan-fn multi-value next-method (car args)))
      ((2) (dylan-fn multi-value next-method (car args) (cadr args)))
      ((3) (dylan-fn multi-value next-method
		     (car args) (cadr args) (caddr args)))
      ((4) (dylan-fn multi-value next-method
		     (car args) (cadr args) (caddr args) (cadddr args)))
      (else (apply dylan-fn multi-value next-method args)))))

(define (dylan-mv-apply dylan-fn multi-value . args)
  ;; You must specify the first argument (multiple-values) explictly.
  (let ((args (reformat-apply-args args)))
    (case (length args)
      ((0) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC))
      ((1) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC (car args)))
      ((2) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC (car args)
		     (cadr args)))
      ((3) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC
		     (car args) (cadr args) (caddr args)))
      ((4) (dylan-fn multi-value NEXT-METHOD:NOT-GENERIC
		     (car args) (cadr args) (caddr args) (cadddr args)))
      (else (apply dylan-fn multi-value NEXT-METHOD:NOT-GENERIC args)))))

(define (dylan::function->method param-list scheme-function)
  (dylan::make-method
   param-list
   (let ((nreq (param-list.nrequired param-list))
	 (rest? (param-list.rest? param-list))
	 (keys (param-list.keys param-list)))
     (make-dylan-callable
      scheme-function
      (if (or rest? keys)
	  -1				; Unknown number of arguments
	  nreq)))))

(define (dylan::dylan-callable->method param-list dylan-callable)
  (dylan::make-method param-list dylan-callable))
