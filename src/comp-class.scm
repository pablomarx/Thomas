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

; $Id: comp-class.scm,v 1.9 1992/09/05 16:04:19 jmiller Exp $

;;;; More of the compiler: class and binding related operations
;;;;
;;;; BIND, DEFINE-GENERIC-FUNCTION, DEFINE-CLASS, DEFINE-SLOT

;;; Parsing and compiling BIND special forms

;; Access functions for BINDings

(define (binding->names binding)
  (let loop ((names '())
	     (rest binding))
    (if (not (list? binding))
	(dylan::error "bind -- bad binding" binding))
    (if (or (null? rest) (null? (cdr rest)) (eq? '!rest (car rest)))
	(reverse names)
	(let ((this-binding (car rest)))
	  (cond ((variable-name? this-binding)
		 (loop (cons this-binding names)
		       (cdr rest)))
		((and (list-of-length? this-binding 2)
		      (variable-name? (car this-binding)))
		 (loop (cons (car this-binding) names)
		       (cdr rest)))
		(else (dylan::error "bind -- bad binding list" binding)))))))

(define (binding->restrictions binding)
  (let loop ((restrictions '())
	     (rest binding))
    (if (or (null? rest) (null? (cdr rest)) (eq? '!rest (car rest)))
	(reverse restrictions)
	(let ((this-binding (car rest)))
	  (loop (cons
		 (if (variable-name? this-binding) #F (cadr this-binding))
		 restrictions)
		(cdr rest))))))

(define (binding->rest binding)
  (let ((found (memq '!rest binding)))
    (if found
	(begin
	  (must-be-list-of-length found 3
	    "bind -- error in bindings")
	  (cadr found))
	#F)))

(define (binding->init binding)
  (last binding))

(define (build-BIND-form bound-names rest-name compiled-restrictions
			 compiled-init compiled-body)
  (let ((all
	   ;; Build a list with entries (offset name restriction)
	   ;; where 'offset' is offset in values vector,
	   (let process ((offset 0)
			 (names bound-names)
			 (restrictions compiled-restrictions))
	     (if (null? names)
		 '()
		 `((,offset ,(car names) ,(car restrictions))
		   ,@(process (+ offset 1) (cdr names) (cdr restrictions))))))
	  (->offset car)
	  (->name cadr)
	  (->restriction caddr))
      (define restricted
	(let loop ((rest all))
	  (if (null? rest)
	      '()
	      `(,@(if (->restriction (car rest)) (list (car rest)) `())
		,@(loop (cdr rest))))))
      `(LET ((!BIND-BODY
	      (LAMBDA (,@bound-names ,@(if rest-name (list rest-name) `()))
		,compiled-body))
	     (!BIND-INIT-FORM
	      (LAMBDA (!MULTI-VALUE) ,compiled-init))
	     ,@(map (lambda (restriction)
		      `(,(->name restriction) ,(->restriction restriction)))
		    restricted))
	 (LET* ((!MULTI-VALUE
		 (DYLAN::VECTOR
		  ,@(map (lambda (name) name #F) bound-names)
		  `())) 
		(!FIRST-VALUE (!BIND-INIT-FORM !MULTI-VALUE)))
	   (IF (DYLAN::EQ? !FIRST-VALUE !MULTI-VALUE)
	       ;; We went through values ...
	       ,(let ((call
		      `(!BIND-BODY
			,@(map (lambda (offset)
				 `(DYLAN::VECTOR-REF !MULTI-VALUE ,offset))
			       (map ->offset all))
			,@(if rest-name
			      `((DYLAN::VECTOR-REF !MULTI-VALUE
						   ,(length bound-names))) 
			      `()))))
		 (if (null? restricted)
		     call
		     `(BEGIN
			,@(map (lambda (offset name)
				 `(DYLAN::TYPE-CHECK
				   (DYLAN::VECTOR-REF !MULTI-VALUE ,offset)
				   ,name))
			       (map ->offset restricted)
			       (map ->name restricted))
			,call)))
	       ;; Didn't go through values ...
	       ,(if (null? bound-names)
		    `(!BIND-BODY (DYLAN::LIST !FIRST-VALUE))
		    (let* ((first (car all))
			   (restriction (->restriction first)))
		      (define call
			`(!BIND-BODY !FIRST-VALUE
				  ,@(map (lambda (name) name #F)
					 (cdr bound-names))
				  ,@(if rest-name `('()) `())))
		      (if restriction
			  `(BEGIN
			     (DYLAN::TYPE-CHECK !FIRST-VALUE ,(->name first))
			     ,call)
			  call))))))))

;; Compiling the BIND form

(define (compile-BIND-form
	 e module-vars bound-vars really-compile
	 multiple-values? continue)
  (must-be-list-of-at-least-length e 1 "BIND form invalid")
  (let ((bindings (car e))
	(forms (cdr e)))
    (if (null? bindings)
	(if (null? forms)
	    (continue compiled-sharp-f bound-vars)
	    (really-compile
	     `(BEGIN ,@forms)
	     module-vars bound-vars multiple-values? continue))
	(begin
	  (if (not (list? bindings))
	      (dylan::error "bind -- bad bindings" bindings))
	  (let* ((binding (car bindings))
		 (bound-names (binding->names binding))
		 (rest-name (binding->rest binding))
		 (init-form (binding->init binding)))
	    (if (and (null? bound-names) (not rest-name))
		(dylan::error "bind -- no variables bound" e))
	    (validate-names (if rest-name
				(cons rest-name bound-names)
				bound-names))
	    (compile-forms
	     (binding->restrictions binding)
	     module-vars bound-vars really-compile #F
	     (lambda (compiled-restrictions mod-vars)
	       (really-compile
		init-form mod-vars bound-vars
		(if (or rest-name
			(and (not (null? bound-names))
			     (not (null? (cdr bound-names)))))
		    '!MULTI-VALUE
		    #F)
		(lambda (compiled-init new-mods)
		  (let ((bound-names (map variable->name bound-names))
			(rest-name (and rest-name
					(variable->name rest-name))))
		    (really-compile
		     `(BIND ,(cdr bindings) ,@forms)
		     new-mods (append (if rest-name (list rest-name) '())
				      bound-names
				      bound-vars)
		     multiple-values?
		     (lambda (compiled-body new-mods)
		       (continue
			(build-BIND-form bound-names rest-name
					 compiled-restrictions compiled-init
					 compiled-body)
			new-mods)))))))))))))

;;; Parsing and compiling the DEFINE-GENERIC-FUNCTION special form

(define (gen-fn-param-error params where)
  (dylan::error "define-generic-function -- parameter syntax error"
		params where))

(define (parse-gen-fn-params orig-params allowed-fn continue)
  (let loop ((names '())
	     (params orig-params))
    (cond ((null? params) (continue (reverse names) params))
	  ((not (pair? params))
	   (gen-fn-param-error orig-params (list 'PARSING params)))
	  ((allowed-fn (car params)) =>
	   (lambda (value)
	     (loop (cons value names) (cdr params))))
	  (else (continue (reverse names) params)))))

(define (parse-DEFINE-GENERIC-FUNCTION
	 name params keywords compiler)
  keywords				; Not used
  (if (not (variable-name? name))
      (dylan::error "define-generic-function -- illegal name" name))
  (parse-gen-fn-params
   params (lambda (x) (and (variable-name? x) x)) 
   (lambda (reqs rest)
     (define (symbol-or-keyword x)
       (cond ((memq x '(!KEY !REST)) x)
	     ((dylan-special-name? x)
	      (gen-fn-param-error params (list 'RESERVED-NAME rest)))
	     ((keyword? x) x)
	     ((symbol? x) (name->keyword x))
	     (else #F)))
     (define (compile-keys rest)
       (lambda (keys after-keys)
	 (if (not (null? after-keys))
	     (gen-fn-param-error params (list 'COMPILE-KEYS after-keys)))
	 (compiler name reqs rest keys)))
     (cond ((null? rest) (compiler name reqs #F #F))
	   ((not (pair? rest))
	    (gen-fn-param-error params (list 'STRANGE-REST rest)))
	   (else
	    (case (car rest)
	      ((!REST)
	       (let ((after-rest (cdr rest)))
		 (if (or (not (pair? after-rest))
			 (not (variable-name? (car after-rest))))
		     (gen-fn-param-error
		      params
		      (list 'POST-!REST after-rest)))
		 (let ((rest (car after-rest)))
		   (cond ((null? (cdr after-rest))
			  (compiler name reqs rest #F))
			 ((not (pair? (cdr after-rest)))
			  (gen-fn-param-error
			   params
			   (list 'AFTER-!REST (cdr after-rest))))
			 ((eq? `!KEY (cadr after-rest))
			  (parse-gen-fn-params
			   (cddr after-rest) symbol-or-keyword
			   (compile-keys rest)))
			 (else
			  (gen-fn-param-error
			   params
			   (list 'BEFORE-!KEY (cddr after-rest))))))))
	      ((!KEY)
	       (if (null? (cdr rest))
		   (compiler name reqs #F #T)
		   (parse-gen-fn-params
		    (cdr rest) symbol-or-keyword
		    (compile-keys #F))))
	      (else (gen-fn-param-error
		     params
		     (list 'UNKNOWN-STOPPER rest)))))))))

(define (compile-DEFINE-GENERIC-FUNCTION-form
	 e mod-vars bound-vars compiler multiple-values? continue)
  compiler				; No sub-compilations
  multiple-values?			; No reductions
  (must-be-list-of-length e 2
    "DEFINE-GENERIC-FUNCTION: invalid syntax")
  (parse-DEFINE-GENERIC-FUNCTION (car e) (cadr e) (cddr e)
   (lambda (name reqs rest keys)
     (module-refs
      name bound-vars mod-vars
      continue
      (lambda (ref set)
	`(IF (DYLAN::NOT (OR (DYLAN::EQ? ,ref ',the-unassigned-value)
			     (DYLAN::GENERIC-FUNCTION? ,ref)))
	     (DYLAN-CALL DYLAN:ERROR
			 "define-generic-function -- already has a value"
			 ',name ,ref ',reqs ',rest ',keys)
	     (BEGIN
	       ,(set `(DYLAN::CREATE-GENERIC-FUNCTION
		       ',name ,(length reqs) ',keys ,(if rest #T #F)))
	       ',name)))))))

;;; Parsing and compiling the DEFINE-CLASS form

(define (expand-slot slot)
  (define (build-slot pairs default-getter)
    (validate-keywords
     pairs
     '(getter: setter: type: init-value: init-function: init-keyword:
	       required-init-keyword: allocation:)
     dylan::error)
    (let ((getter
	   (dylan::find-keyword pairs 'getter:
				(lambda ()
				  (or default-getter
				      (dylan::error
				       "slot expander -- no getter name"
				       pairs)))))
	  (setter
	   (dylan::find-keyword
	    pairs 'setter: (lambda ()
			     (if default-getter
				 `(SETTER ,default-getter)
				 #F))))
	  (type
	   (dylan::find-keyword pairs 'type: (lambda () '<object>)))
	  (has-initial-value? #T))
      (let ((init-value
	     (dylan::find-keyword pairs 'init-value:
				  (lambda ()
				    (set! has-initial-value? #F)
				    #F)))
	    (init-function
	     (dylan::find-keyword pairs 'init-function: (lambda () #F)))
	    (init-keyword
	     (dylan::find-keyword pairs 'init-keyword: (lambda () #F)))
	    (required-init-keyword
	     (dylan::find-keyword pairs 'required-init-keyword:
				  (lambda () #F)))
	    (allocation
	     (dylan::find-keyword pairs 'allocation: (lambda () 'instance))))
	(if (and (variable-name? getter)
		 (or (not setter) (variable-name? setter))
		 (or (not has-initial-value?) (not init-function))
		 (or (not required-init-keyword)
		     (and (not init-keyword) (not has-initial-value?)
			  (not init-function)))
		 (memq allocation '(instance class each-subclass
					     constant virtual)))
	    (make-slot getter		; debug-name
		       getter setter type init-value has-initial-value?
		       init-function init-keyword required-init-keyword
		       allocation #F #F)
	    (dylan::error "slot expander -- bad slot"
			  getter setter has-initial-value? init-value
			  init-function init-keyword
			  required-init-keyword allocation)))))
  (cond ((symbol? slot)
	 (make-slot slot slot `(SETTER ,slot) '<object> #F #F #F #F #F
		    'instance #F #F))
	((and (pair? slot) (keyword? (car slot)))
	 (build-slot slot #F))
	((and (pair? slot) (symbol? (car slot))
	      (variable-name? (car slot)))
	 (build-slot (cdr slot) (car slot)))
	(else (dylan::error "slot expander -- bad slot specification"
			    slot))))

(define (expand-slots slots)
  (map expand-slot slots))

(define (make-add-slot-call
	 slot owner bound-vars mod-vars compiler continue)
  (define (get-mod-var name mod-vars setter? continue)
    (if name
	(module-refs name bound-vars mod-vars continue 
	  (lambda (ref set)
	    `(BEGIN
	       (COND ((DYLAN::EQ? ,ref ',the-unassigned-value)
		      ,(set `(DYLAN::CREATE-GENERIC-FUNCTION
			      ',(variable->name name)
			      ,(if setter? 2 1) ; # of arguments
			      #F	; No required keywords
			      #F)))	; No rest argument
		     ((DYLAN::NOT (DYLAN::GENERIC-FUNCTION? ,ref))
		      (DYLAN-CALL DYLAN:ERROR
				  "Slot function -- already has a value"
				  ',(slot.debug-name slot) ,owner)))
	       ,ref)))
	(continue #F mod-vars)))
  (get-mod-var (slot.getter slot) mod-vars #F
    (lambda (getter mv)
      (get-mod-var (slot.setter slot) mv #T
	(lambda (setter mv)
	  (compile-forms
	   (list (slot.type slot)
		 (slot.init-value slot)
		 (slot.init-function slot))
	   mv bound-vars compiler #F
	   (lambda (compiled-forms mv)
	     (continue
	      `(DYLAN::ADD-SLOT ,owner
				,(car compiled-forms) ; Type
				',(slot.allocation slot)
				,setter
				,getter
				',(slot.debug-name slot)
				,(cadr compiled-forms) ; Init-Value
				,(slot.has-initial-value? slot)
				,(caddr compiled-forms) ; Init-Function
				',(slot.init-keyword slot)
				',(slot.required-init-keyword slot))
	      mv))))))))

(define (generate-DEFINE-CLASS
	 name superclasses getter-code slots bound-vars
	 mod-vars compiler continue)
  (let loop ((slots slots)
	     (mod-vars mod-vars)
	     (add-slot-calls '()))
    (if (null? slots)
	(module-refs name bound-vars mod-vars continue
	  (lambda (ref set)
	    set				; Ignored
	    `(BEGIN
	       (IF (DYLAN::NOT
		    (OR (DYLAN::EQ? ,ref ',the-unassigned-value)
			(DYLAN::CLASS? ,ref)))
		   (DYLAN-CALL DYLAN:ERROR
			       "define-class -- already has a value"
			       ',name ,ref ',(map slot.getter slots))
		   (LET ((!CLASS
			  (DYLAN::MAKE-A-CLASS
			   ',name
			   (DYLAN::LIST ,@superclasses)
			   (DYLAN::LIST ,@getter-code))))
		     ,@add-slot-calls
		     ,(set '!CLASS)
		     ',name)))))
	(make-add-slot-call
	 (car slots) '!CLASS bound-vars mod-vars compiler
	 (lambda (code mod-vars)
	   (loop (cdr slots)
		 mod-vars
		 (cons code add-slot-calls)))))))

(define (compile-DEFINE-CLASS-form
	 e mod-vars bound-vars compiler multiple-values? continue)
  multiple-values?			; No reductions
  (must-be-list-of-at-least-length e 2 "DEFINE-CLASS: invalid syntax")
  (let ((superclasses (cadr e))
	(slots (expand-slots (cddr e))))
    (compile-forms superclasses mod-vars bound-vars compiler #F
      (lambda (supers mod-vars)
	(let loop ((getter-code '())
		   (slots-left slots)
		   (mod-vars mod-vars))
	  (if (null? slots-left)
	      (generate-DEFINE-CLASS
	       (car e) supers getter-code slots
	       bound-vars mod-vars compiler continue)
	      (module-refs (slot.getter (car slots))
			   bound-vars mod-vars
			   (lambda (code mod-vars)
			     (loop (cons code getter-code)
				   (cdr slots-left)
				   mod-vars))
			   (lambda (ref set)
			     set	; Ignored
			     ref))))))))

;;; DEFINE-SLOT

(define (compile-DEFINE-SLOT-form
	 e mod-vars bound-vars compiler multiple-values? continue)
  multiple-values?			; Ignored
  (must-be-list-of-at-least-length e 2 "DEFINE-SLOT: invalid syntax")
  (let ((owner (car e))
	(slot (cdr e)))
    (make-add-slot-call
     (expand-slot
      (if (keyword? (car slot)) slot `(GETTER: ,@slot)))
     owner bound-vars mod-vars compiler continue)))
