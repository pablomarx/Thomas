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

; $Id: compiler.scm,v 1.26 1992/09/11 21:24:20 jmiller Exp $

;;;; This file contains the Thomas -> Scheme compiler and
;;;; routines needed ONLY at compilation time.  Support routines that
;;;; are also needed when Dylan programs run are located in generic.scm
;;;; (generic operator dispatch), class.scm (class heterarchy), and
;;;; support.scm (general support routines)

;;; Normal external entry points for compilation

(define (compile-expression e multi-value mod-vars continue)
  ;; e is a single Thomas expression
  ;; multi-value is an expression to be passed as the multi-value
  ;;  vector (or #F) at runtime
  ;; mod-vars is a list of pre-existing module variables
  ;; continue is a function that receives:
  ;;   a: the output code
  ;;   b: the preamble (def'ns of free variables, refs, sets)
  ;;   c: the list of newly created module variables
  (define (define-module-variable name)
    `(DEFINE ,name ',the-unassigned-value))
  (define (define-module-getter name)
    `(DEFINE (,(name->module-getter name)) ,name))
  (define (define-module-setter name)
    `(DEFINE (,(name->module-setter name) NEW-VALUE)
       (SET! ,name NEW-VALUE)))
  (really-compile e mod-vars '() multi-value
   (lambda (compiled-output free-vars)
     (let* ((need-getter/setters
	     (if (null? free-vars)
		 '()
		 (set-difference free-vars mod-vars member)))
	    (need-definition
	     (set-difference need-getter/setters
			     dylan::predefined-variables member)))
       (continue need-definition
		 `(,@(map define-module-variable need-definition)
		   ,@(map define-module-getter need-getter/setters)
		   ,@(map define-module-setter need-getter/setters))
		 compiled-output)))))

(define dylan::scheme-names-of-predefined-names
  `((* dylan:*)					    ; Method
    (+ dylan:+)					    ; Method
    (- dylan:-)					    ; Method
    (/ dylan:/)					    ; Method
    (/= dylan:/=)				    ; Method
    (< dylan:<)					    ; Method
    (<= dylan:<=)				    ; Method
    (<abort> <abort>)				    ; Class
    (<array> <array>)				    ; Class
    (<byte-string> <byte-string>)		    ; Class
    (<character> <character>)			    ; Class
    (<class> <class>)				    ; Class
    (<collection> <collection>)			    ; Class
    (<complex> <complex>)			    ; Class
    (<condition> <condition>)			    ; Class
    (<deque> <deque>)				    ; Class
    (<double-float> <double-float>)		    ; Class
    (<empty-list> <empty-list>)			    ; Class
    (<error> <error>)				    ; Class
    (<explicit-key-collection>			    ; Class
     <explicit-key-collection>)
    (<extended-float> <extended-float>)		    ; Class
    (<float> <float>)				    ; Class
    (<function> <function>)			    ; Class
    (<generic-function> <generic-function>)	    ; Class
    (<integer> <integer>)			    ; Class
    (<keyword> <keyword>)			    ; Class
    (<list> <list>)				    ; Class
    (<method> <method>)				    ; Class
    (<mutable-collection> <mutable-collection>)	    ; Class
    (<mutable-explicit-key-collection>		    ; Class
     <mutable-explicit-key-collection>)
    (<mutable-sequence> <mutable-sequence>)         ; Class
    (<number> <number>)				    ; Class
    (<object> <object>)				    ; Class
    (<pair> <pair>)				    ; Class
    (<range> <range>)				    ; Class
    (<ratio> <ratio>)				    ; Class
    (<rational> <rational>)			    ; Class
    (<real> <real>)				    ; Class
    (<rectangular-complex> <rectangular-complex>)   ; Class
    (<restart> <restart>)			    ; Class
    (<sequence> <sequence>)			    ; Class
    (<serious-condition> <serious-condition>)	    ; Class
    (<simple-error> <simple-error>)		    ; Class
    (<simple-object-vector> <simple-object-vector>) ; Class
    (<simple-restart> <simple-restart>)		    ; Class
    (<simple-warning> <simple-warning>)		    ; Class
    (<single-float> <single-float>)		    ; Class
    (<singleton> <singleton>)			    ; Class
    (<slot-descriptor> <slot-descriptor>)	    ; Class
    (<stretchy-vector> <stretchy-vector>)	    ; Class
    (<string> <string>)				    ; Class
    (<symbol> <symbol>)				    ; Class
    (<table> <table>)				    ; Class
    (<type-error> <type-error>)			    ; Class
    (<unicode-string> <unicode-string>)		    ; Class
    (<vector> <vector>)				    ; Class
    (<warning> <warning>)			    ; Class
    (= dylan:=)					    ; Method
    (=hash dylan:=hash)				    ; Generic-Function
    (> dylan:>)					    ; Method
    (>= dylan:>=)				    ; Method
    (Id? dylan:id?)				    ; Method
    (abort dylan:abort)				    ; Sealed-Function
    (abs dylan:abs)				    ; Generic-Function
    (acos dylan:acos)				    ; Generic-Function
    (acosh dylan:acosh)				    ; Generic-Function
    (add dylan:add)				    ; Generic-Function
    (add! dylan:add!)				    ; G.F. Method
    (add-method dylan:add-method)		    ; Generic-Function
    (add-new dylan:add-new)			    ; Generic-Function
    (add-new! dylan:add-new!)			    ; Generic-Function
    (add-slot dylan:add-slot)			    ; Generic-Function
    (all-superclasses dylan:all-superclasses)       ; Generic-Function
    (always dylan:always)			    ; Method
    (angle dylan:angle)				    ; Generic-Method
    (any? dylan:any?)				    ; Generic-Function
    (append dylan:append)			    ; Generic-Function
    (applicable-method? dylan:applicable-method?)   ; Generic-Function
    (apply dylan:apply)				    ; Function
    (aref dylan:aref)				    ; Generic-Function
    (as dylan:as)				    ; Generic-Function
    (as-lowercase dylan:as-lowercase)	            ; G.F. Method
    (as-lowercase! dylan:as-lowercase!)		    ; G.F. Method
    (as-uppercase dylan:as-uppercase)		    ; G.F. Method
    (as-uppercase! dylan:as-uppercase!)		    ; G.F. Method
    (ash dylan:ash)				    ; Generic-Method
    (asin dylan:asin)				    ; Generic-Function
    (asinh dylan:asinh)				    ; Generic-Function
    (atan dylan:atan)				    ; Generic-Function
    (atan2 dylan:atan2)				    ; Generic-Function
    (atanh dylan:atanh)				    ; Generic-Function
    (binary* dylan:binary*)			    ; Generic-Function
    (binary+ dylan:binary+)			    ; Generic-Function
    (binary- dylan:binary-)			    ; Generic-Function
    (binary-gcd dylan:binary-gcd)		    ; Generic-Method
    (binary-lcm dylan:binary-lcm)		    ; Generic-Method
    (binary/ dylan:binary/)			    ; Generic-Function
    (binary< dylan:binary<)			    ; Generic-Function
    (binary= dylan:binary=)			    ; Generic-Function
    (break dylan:break)				    ; Sealed-Function
    (caaar dylan:caaar)				    ; Method
    (caadr dylan:caadr)				    ; Method
    (caar dylan:caar)				    ; Method
    (cadar dylan:cadar)				    ; Method
    (caddr dylan:caddr)				    ; Method
    (cadr dylan:cadr)				    ; Method
    (car dylan:car)				    ; Method
    (cdaar dylan:cdaar)				    ; Method
    (cdadr dylan:cdadr)				    ; Method
    (cdar dylan:cdar)				    ; Method
    (cddar dylan:cddar)				    ; Method
    (cdddr dylan:cdddr)				    ; Method
    (cddr dylan:cddr)				    ; Method
    (cdr dylan:cdr)				    ; Method
    (ceiling dylan:ceiling)			    ; Generic-Function
    (ceiling/ dylan:ceiling/)			    ; Generic-Function
    (cerror dylan:cerror)			    ; Sealed-Function
    (check-type dylan:check-type)		    ; Sealed-Function
    (choose dylan:choose)			    ; Generic-Function
    (choose-by dylan:choose-by)			    ; Generic-Function
    (class-for-copy dylan:class-for-copy)           ; Generic-Function
    (complement dylan:complement)		    ; Method
    (compose dylan:compose)			    ; Method
    (concatenate dylan:concatenate)		    ; Generic-Function
    (concatenate-as dylan:concatenate-as)	    ; Generic-Function
    (conjoin dylan:conjoin)			    ; Method
    (cons dylan:cons)				    ; Method
    (copy-sequence dylan:copy-sequence)		    ; Generic-Function
    (copy-state dylan:copy-state)		    ; Generic-Function
    (cos dylan:cos)				    ; Generic-Function
    (cosh dylan:cosh)				    ; Generic-Function
    (current-element dylan:current-element)	    ; Generic-Function
    (current-key dylan:current-key)		    ; Generic-Function
    (curry dylan:curry)				    ; Method
    (default-handler dylan:default-handler)	    ; Generic-Function
    (denominator dylan:denominator)		    ; Generic-Method
    (dimensions dylan:dimensions)		    ; Generic-Function
    (direct-subclasses dylan:direct-subclasses)	    ; Generic-Function
    (direct-superclasses dylan:direct-superclasses) ; Generic-Function
    (disjoin dylan:disjoin)			    ; Method
    (do dylan:do)			            ; Generic-Function
    (do-handlers dylan:do-handlers)		    ; Sealed-Function
    (element dylan:element)			    ; Generic-Function
    (empty? dylan:empty?)			    ; Generic-Function
    (error dylan:error)			            ; Sealed-Function
    (even? dylan:even?)				    ; Generic-Function
    (every? dylan:every?)			    ; Generic-Function
    (exp dylan:exp)				    ; Generic-Function
    (expt dylan:expt)				    ; Generic-Function
    (fill! dylan:fill!)				    ; Generic-Function
    (final-state dylan:final-state)		    ; G.F. Method
    (find-key dylan:find-key)			    ; Generic-Function
    (find-method dylan:find-method)		    ; Generic-Function
    (find-pair dylan:find-pair)			    ; Generic-Function
    (first dylan:first)				    ; Generic-Function
    (floor dylan:floor)				    ; Generic-Function
    (floor/ dylan:floor/)			    ; Generic-Function
    (freeze-methods dylan:freeze-methods)	    ; Generic-Function
    (function-arguments dylan:function-arguments)   ; Generic-Function
    (gcd dylan:gcd)				    ; Method
    (generic-function-methods			    ; Generic-Function
     dylan:generic-function-methods)
    (identity dylan:identity)			    ; Method
    (imag-part dylan:imag-part)			    ; Generic-Method
    (init-function dylan:init-function)		    ; Generic-Function
    (init-keyword dylan:init-keyword)		    ; Generic-Function
    (init-value dylan:init-value)		    ; Generic-Function
    (initial-state dylan:initial-state)		    ; Generic-Function
    (initialize dylan:initialize)		    ; Generic-Function
    (instance? dylan:instance?)			    ; Generic-Function
    (integral? dylan:integral?)			    ; Generic-Function
    (intersection dylan:intersection)		    ; Generic-Function
    (key-sequence dylan:key-sequence)		    ; Generic-Function
    (last dylan:last)				    ; Generic-Function
    (lcm dylan:lcm)				    ; Method
    (list dylan:list)				    ; Method
    (list* dylan:list*)				    ; Method
    (log dylan:log)				    ; Generic-Function
    (logand dylan:logand)			    ; Generic-Method
    (logandc1 dylan:logandc1)			    ; Generic-Method
    (logandc2 dylan:logandc2)			    ; Generic-Method
    (logbit? dylan:logbit?)			    ; Generic-Method
    (logeqv dylan:logeqv)			    ; Generic-Method
    (logior dylan:logior)			    ; Generic-Method
    (lognand dylan:lognand)			    ; Generic-Method
    (lognor dylan:lognor)			    ; Generic-Method
    (lognot dylan:lognot)			    ; Generic-Method
    (logorc1 dylan:logorc1)			    ; Generic-Method
    (logorc2 dylan:logorc2)			    ; Generic-Method
    (logxor dylan:logxor)			    ; Generic-Method
    (make dylan:make)				    ; Generic-Function
    (make-polar dylan:make-polar)		    ; Generic-Function
    (make-read-only dylan:make-read-only)	    ; Generic-Function
    (make-rectangular dylan:make-rectangular)	    ; Generic-Function
    (map dylan:map)				    ; Generic-Function
    (map-as dylan:map-as)			    ; Generic-Function
    (map-into dylan:map-into)			    ; Generic-Function
    (max dylan:max)				    ; Method
    (member? dylan:member?)			    ; Generic-Function
    (method-specializers dylan:method-specializers) ; Generic-Function
    (min dylan:min)				    ; Method
    (modulo dylan:modulo)			    ; Generic-Function
    (negative? dylan:negative?)			    ; Generic-Function
    (next-state dylan:next-state)		    ; Generic-Function
    (not dylan:not)				    ; Function
    (numerator dylan:numerator)			    ; Generic-Method
    (object-class dylan:object-class)		    ; Generic-Function
    (odd? dylan:odd?)				    ; Generic-Function
    (pop dylan:pop)				    ; Generic-Function
    (pop-last dylan:pop-last)			    ; Generic-Function
    (positive? dylan:positive?)			    ; Generic-Function
    (previous-state dylan:previous-state)	    ; G.F. Method
    (push dylan:push)				    ; Generic-Function
    (push-last dylan:push-last)			    ; Generic-Function
    (range dylan:range)				    ; Generic-Function
    (rcurry dylan:rcurry)			    ; Method
    (real-part dylan:real-part)			    ; Generic-Method
    (rationalize dylan:rationalize)		    ; Generic-Method
    (reduce dylan:reduce)			    ; Generic-Function
    (reduce1 dylan:reduce1)			    ; Generic-Function
    (remainder dylan:remainder)			    ; Generic-Function
    (remove dylan:remove)			    ; Generic-Function
    (remove! dylan:remove!)			    ; Generic-Function
    (remove-duplicates dylan:remove-duplicates)	    ; Generic-Function
    (remove-duplicates! dylan:remove-duplicates!)   ; Generic-Function
    (remove-key! dylan:remove-key!)		    ; Generic-Function
    (remove-method dylan:remove-method)		    ; Generic-Function
    (remove-slot dylan:remove-slot)		    ; Generic-Function
    (replace-elements! dylan:replace-elements!)	    ; Generic-Function
    (replace-subsequence! dylan:replace-subsequence!) ; Generic-Function
    (restart-query dylan:restart-query)		    ; Generic-Function
    (return-allowed? dylan:return-allowed?)	    ; Generic-Function
    (return-description dylan:return-description)   ; Generic-Function
    (return-query dylan:return-query)		    ; Generic-Function
    (reverse dylan:reverse)			    ; Generic-Function
    (reverse! dylan:reverse!)			    ; Generic-Function
    (round dylan:round)				    ; Generic-Function
    (round/ dylan:round/)			    ; Generic-Function
    (seal dylan:seal)				    ; Generic-Function
    (second dylan:second)			    ; Generic-Function
    (shallow-copy dylan:shallow-copy)		    ; Generic-Function
    (signal dylan:signal)			    ; Sealed-Function
    (sin dylan:sin)				    ; Generic-Function
    (singleton dylan:singleton)			    ; Function
    (sinh dylan:sinh)				    ; Generic-Function
    (size dylan:size)				    ; G.F. Method
    (slot-allocation dylan:slot-allocation)	    ; Generic-Function
    (slot-descriptor dylan:slot-descriptor)	    ; Generic-Function
    (slot-descriptors dylan:slot-descriptors)       ; Generic-Function
    (slot-getter dylan:slot-getter)		    ; Generic-Function
    (slot-initialized? dylan:slot-initialized?)	    ; Generic-Function
    (slot-setter dylan:slot-setter)		    ; Generic-Function
    (slot-type dylan:slot-type)			    ; Generic-Function
    (slot-value dylan:slot-value)		    ; Generic-Function
    (sort dylan:sort)				    ; Generic-Function
    (sort! dylan:sort!)				    ; Generic-Function
    (sorted-applicable-methods			    ; Generic-Function
     dylan:sorted-applicable-methods)
    (sqrt dylan:sqrt)				    ; Generic-Method
    (subclass? dylan:subclass?)			    ; Generic-Function
    (subsequence-position dylan:subsequence-position) ; Generic-Function
    (tan dylan:tan)				    ; Generic-Function
    (tanh dylan:tanh)				    ; Generic-Function
    (third dylan:third)				    ; Generic-Function
    (truncate dylan:truncate)			    ; Generic-Function
    (truncate/ dylan:truncate/)			    ; Generic-Function
    (unary- dylan:unary-)			    ; Generic-Function
    (unary/ dylan:unary/)			    ; Generic-Function
    (union dylan:union)				    ; Generic-Function
    (values dylan:values)			    ; Function
    (vector dylan:vector)			    ; Method
    (zero? dylan:zero?)				    ; Generic-Function
    ;;;;;;;;;;;;;;; SETTER VARIABLES
    (,(name->setter 'slot-value) dylan:setter/slot-value/)
    (,(name->setter 'element) dylan:setter/element/)
    (,(name->setter 'current-element) dylan:setter/current-element/)
    (,(name->setter 'first) dylan:setter/first/)
    (,(name->setter 'second) dylan:setter/second/)
    (,(name->setter 'third) dylan:setter/third/)
    (,(name->setter 'aref) dylan:setter/aref/)
    (,(name->setter 'car) dylan:setter/car/)
    (,(name->setter 'cdr) dylan:setter/cdr/)
    (,(name->setter 'caar) dylan:setter/caar/)
    (,(name->setter 'cadr) dylan:setter/cadr/)
    (,(name->setter 'cdar) dylan:setter/cdar/)
    (,(name->setter 'cddr) dylan:setter/cddr/)
    (,(name->setter 'caaar) dylan:setter/caaar/)
    (,(name->setter 'caadr) dylan:setter/caadr/)
    (,(name->setter 'cadar) dylan:setter/cadar/)
    (,(name->setter 'caddr) dylan:setter/caddr/)
    (,(name->setter 'cdaar) dylan:setter/cdaar/)
    (,(name->setter 'cdadr) dylan:setter/cdadr/)
    (,(name->setter 'cddar) dylan:setter/cddar/)
    (,(name->setter 'cdddr) dylan:setter/cdddr/)
    ;;;;;;;;;;;;;;; CRL ADDITIONS
    (display dylan:display)
    (newline dylan:newline)
    (write-line dylan:write-line)
    (print dylan:print)
    ,@implementation-specific:additional-dylan-bindings
    ))

(define dylan::predefined-names
  (map car dylan::scheme-names-of-predefined-names))

(define dylan::predefined-variables
  (map cadr dylan::scheme-names-of-predefined-names))

(define (thomas file-name . expressions)
  (compile-expression `(BEGIN ,@expressions) #F '()
    (lambda (new-vars preamble-code compiled)
      new-vars				; Not used
      (with-output-to-file file-name
	(lambda ()
	  (display "; Output generated by the CRL Thomas->Scheme compiler.")
	  (newline)
	  (implementation-specific:generate-file
	   expressions
	   `(dylan::catch-all-conditions
	     (lambda () ,@preamble-code ,compiled))))))))

(define (thomas->scheme input output)
  (let ((in-port (open-input-file input)))
    (let loop ((exprs '()))
      (let ((next (read in-port)))
	(if (eof-object? next)
	    (thomas output `(BEGIN ,@(reverse exprs)))
	    (loop (cons next exprs)))))))

;;; Compile a list of forms, returning a list of Scheme expressions
;;; ASSUMPUTION: multiple-values?, if not #F, is to be used only for
;;; compiling the last of the forms.

(define (compile-forms
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
  (let loop ((result '())
	     (forms forms)
	     (mod-vars module-vars))
    (if (null? forms)
	(continue (reverse result) mod-vars)
	(really-compile (car forms) mod-vars bound-vars
			(if (null? (cdr forms)) multiple-values? #F)
	  (lambda (compiled mod-vars)
	    (loop (cons compiled result)
		  (cdr forms)
		  mod-vars))))))

;;; The real compiler.
;;;
;;; Input: e is a form to be compiled
;;;        module-vars are the module variables already known to exist
;;;        bound-vars are the names of lexically enclosing variables
;;;        multiple-values? is either #F, indicating that the current
;;;          expressions is being compiled in non-tail position or has
;;;          the name of an internal variable to be used at runtime to
;;;          transmit the multiple-value returning function along the
;;;          tail call chain.
;;;        continue is called with the result of the compilation, and
;;;          is passed the single SCHEME form resulting from compiling
;;;          e and the new list of module variables.
;;; Output: always either error exits or tail calls into continue

(define (really-compile e module-vars bound-vars
			multiple-values? continue)
  (cond
   ((or (null? e) (boolean? e) (string? e)
	(char? e) (number? e))		; syntax might be an issue...
    (continue e module-vars))
   ((or (vector? e) (keyword? e))	; Keywords self-evaluate in
					; Dylan, but not in Scheme
    (continue `(QUOTE ,e) module-vars))
   ((variable-name? e)
    ;; As in Scheme, but the compiler needs to distinguish  bound from
    ;; free
    (let* ((name (variable->name e))
	   (new-mod-vars (add-variable name bound-vars module-vars)))
      (continue
       (if (memq name new-mod-vars)
	   `(DYLAN::FREE-VARIABLE-REF ,name ',name)
	   name)
       new-mod-vars)))
   ((symbol? e)
    (dylan::error "illegal Thomas variable" e))
   ((and (pair? e) (assq (car e) compilation-functions)) =>
    (lambda (binding)
      (((cdr binding)) (cdr e) module-vars bound-vars
		       really-compile multiple-values? continue)))
   ((and (list? e) (not (null? e)))
    (compile-forms e module-vars bound-vars really-compile #F
		   (lambda (forms module-vars)
		     (continue
		      `(DYLAN::APPLY ,multiple-values?
				     ,@(map (lambda (x) `(LAMBDA () ,x))
					    forms))
			       module-vars))))
   (else
    (dylan::error "ill-formed expression" e))))

(define compiled-sharp-f
  (really-compile #F '() '() #F (lambda (compiled free)
				  free	; Ignored
				  compiled)))

(define compilation-functions
  `((AND            . ,(lambda () compile-AND-form))
    (BEGIN          . ,(lambda () compile-BEGIN-form))
    (BIND           . ,(lambda () compile-BIND-form))
    (BIND-EXIT      . ,(lambda () compile-BIND-EXIT-form))
    (BIND-METHODS   . ,(lambda () compile-BIND-METHODS-form))
    (CASE           . ,(lambda () compile-CASE-form))
    (COND           . ,(lambda () compile-COND-form))
    (DEFINE         . ,(lambda () compile-DEFINE-form))
    (DEFINE-CLASS   . ,(lambda () compile-DEFINE-CLASS-form))
    (DEFINE-GENERIC-FUNCTION .
      ,(lambda () compile-DEFINE-GENERIC-FUNCTION-form))
    (DEFINE-METHOD  . ,(lambda () compile-DEFINE-METHOD-form))
    (DEFINE-SLOT    . ,(lambda () compile-DEFINE-SLOT-form))
    (DOTIMES        . ,(lambda () compile-DOTIMES-form))
    (FOR            . ,(lambda () compile-FOR-form))
    (FOR-EACH       . ,(lambda () compile-FOR-EACH-form))
    (HANDLER-BIND   . ,(lambda () compile-HANDLER-BIND-form))
    (HANDLER-CASE   . ,(lambda () compile-HANDLER-CASE-form))
    (IF             . ,(lambda () compile-IF-form))
    (METHOD         . ,(lambda () compile-METHOD-form))
    (OR             . ,(lambda () compile-OR-form))
    (QUOTE          . ,(lambda () compile-QUOTE-form))
    (SELECT         . ,(lambda () compile-SELECT-form))
    (SET!           . ,(lambda () compile-SET!-form))
    (SETTER         . ,(lambda () compile-SETTER-form))
    (UNLESS         . ,(lambda () compile-UNLESS-form))
    (UNTIL          . ,(lambda () compile-UNTIL-form))
    (UNWIND-PROTECT . ,(lambda () compile-UNWIND-PROTECT-form))
    (WHEN           . ,(lambda () compile-WHEN-form))
    (WHILE          . ,(lambda () compile-WHILE-form))))

(define (compile-AND-form forms module-vars bound-vars really-compile
			  multiple-values? continue)
   (if (null? forms) (dylan::error "AND must have forms"))
   (compile-forms
    forms module-vars bound-vars really-compile multiple-values?
    (lambda (code mod-vars) (continue `(AND ,@code) mod-vars))))

(define (compile-BEGIN-form forms module-vars bound-vars really-compile
		multiple-values? continue)
   (if (null? forms)
       (continue compiled-sharp-f module-vars)
       (compile-forms
	forms module-vars bound-vars
	really-compile multiple-values?
	(lambda (compiled module-vars)
	  (continue `(BEGIN ,@compiled) module-vars)))))

; compile-BIND-form in file comp-class

(define (compile-BIND-EXIT-form
	 forms module-vars bound-vars really-compile multiple-values?
	 continue)
  (must-be-list-of-at-least-length forms 1 "BIND-EXIT form invalid")
  (let ((var (car forms))
	(forms (cdr forms)))
    (must-be-list-of-length var 1 "BIND-EXIT bad variable")
    (if (not (variable-name? (car var)))
	(dylan::error "BIND-EXIT -- bad variable name" var forms))
    (let ((name (variable->name (car var))))
      (really-compile
       `(BEGIN ,@forms)
       module-vars (cons name bound-vars) multiple-values?
       (lambda (body module-vars)
	 (continue
	  `(DYLAN::CALL/CC
	    (LAMBDA (!BIND-EXIT)
	      (LET ((,name
		     (LAMBDA (!MULTIPLE-VALUES !NEXT-METHOD . VALUES)
		       !MULTIPLE-VALUES !NEXT-METHOD
		       (!BIND-EXIT
			(DYLAN::SCHEME-APPLY
			 DYLAN:VALUES ,multiple-values?
			 NEXT-METHOD:NOT-GENERIC
			 VALUES)))))
		,body)))
	  module-vars))))))

; compile-BIND-METHODS-form in file comp-method.scm
; compile-CASE-form in file comp-sf
; compile-COND-form in file comp-sf

(define (compile-DEFINE-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
  multiple-values?			; No reductions
  (must-be-list-of-length forms 2 "Bad DEFINE syntax")
  (let ((name (car forms))
	(value (cadr forms)))
    (if (not (variable-name? name))
	(dylan::error "bad DEFINE variable" forms))
    (really-compile value
      (add-module-variable (variable->name name) #F module-vars)
      bound-vars #F
      (lambda (compiled-value new-module-vars)
	(continue
	 `(BEGIN
	    (,(name->module-setter name) ,compiled-value)
	    ',name)
	 new-module-vars)))))

; compile-DEFINE-CLASS-form in file comp-class
; compile-DEFINE-GENERIC-FUNCTION-form in file comp-class
; compile-DEFINE-METHOD-form in file comp-method
; compile-DEFINE-SLOT-form in file comp-class

(define (compile-DOTIMES-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
    (must-be-list-of-at-least-length forms 1 "DOTIMES: bad syntax")
    (let ((v/c/r (car forms))
	  (forms (cdr forms)))
      (must-be-list-of-at-least-length v/c/r 2
       "DOTIMES: Bad var/count/result list")
      (let ((var (car v/c/r))
	    (count-form (cadr v/c/r))
	    (result (if (pair? (cddr v/c/r)) (caddr v/c/r) #F)))
	(if (not (variable-name? var))
	    (dylan::error "DOTIMES -- invalid variable" var forms))
	(if (not (or (null? (cddr v/c/r))
		     (null? (cdddr v/c/r))))
	    (dylan::error "DOTIMES -- bad syntax"))
	(let ((name (variable->name var)))
	  (compile-forms
	   forms module-vars (cons name bound-vars) really-compile #F
	   (lambda (body-forms module-vars)
	     (compile-forms
	      (list count-form result)
	      module-vars bound-vars really-compile multiple-values?
	      (lambda (c/r-code module-vars)
		(continue
		 `(DYLAN::DOTIMES ,(car c/r-code)
				  (LAMBDA () ,(cadr c/r-code))
				  (LAMBDA (,name) ,@body-forms))
		 module-vars)))))))))

; compile-FOR-form in file comp-sf

(define (compile-FOR-EACH-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
    (must-be-list-of-at-least-length forms 2 "FOR-EACH: bad syntax")
    (for-each
     (lambda (binding)
       (must-be-list-of-length binding 2 "FOR-EACH: bad binding"))
     (car forms))
    (let ((names (map car (car forms)))
	  (collections (map cadr (car forms)))
	  (end-test-and-return-vals (cadr forms))
	  (forms (cddr forms)))
      (compile-forms
       collections module-vars bound-vars really-compile #F
       (lambda (compiled-collections module-vars)
	 (compile-forms
	  (if (null? end-test-and-return-vals)
	      '(#F)
	      end-test-and-return-vals)
	  module-vars
	  (append names bound-vars) really-compile multiple-values?
	  (lambda (compiled-et module-vars)
	    (compile-forms
	     forms module-vars (append names bound-vars)
	     really-compile #F
	     (lambda (compiled-forms module-vars)
	       (continue
		`(DYLAN::FOR-EACH
		  (LAMBDA (!MULTIPLE-VALUES !DYLAN:NEXT-METHOD ,@names)
		    !MULTIPLE-VALUES	; Ignored
		    !DYLAN:NEXT-METHOD	; Ignored
		    ,(if (null? end-test-and-return-vals)
			 `(BEGIN ,@compiled-forms #F)
			 `(IF ,(car compiled-et)
			      (DYLAN::LIST ,@(if (null? (cdr compiled-et))
						 (list compiled-sharp-f)
						 (cdr compiled-et)))
			      (BEGIN ,@compiled-forms #F))))
		  ,@compiled-collections)
		module-vars)))))))))

; compile-HANDLER-BIND-form in file comp-exc
; compile-HANDLER-CASE-form in file comp-exc

(define (compile-IF-form forms module-vars bound-vars really-compile
			 multiple-values? continue)
    (must-be-list-of-length forms 3 "IF: invalid syntax")
    (let ((pred (car forms))
	  (conseq (cadr forms))
	  (alter (caddr forms)))
      (really-compile pred module-vars bound-vars #F
	(lambda (c-pred module-vars)
	  (really-compile conseq module-vars
			  bound-vars multiple-values?
	    (lambda (c-conseq module-vars)
	      (really-compile alter module-vars
			      bound-vars multiple-values?
	        (lambda (c-alter module-vars)
		  (continue `(IF ,c-pred ,c-conseq ,c-alter)
			    module-vars)))))))))

; compile-METHOD-form in file comp-method

(define (compile-OR-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
  (compile-forms
   forms module-vars bound-vars really-compile multiple-values?
   (lambda (code mod-vars)
     (continue `(OR ,@code) mod-vars))))

(define (compile-QUOTE-form forms module-vars bound-vars really-compile
		 multiple-values? continue)
  bound-vars really-compile multiple-values?
  (must-be-list-of-length forms 1 "QUOTE: invalid syntax")
  (continue `(QUOTE ,@forms) module-vars))

; compile-SELECT-form in file comp-sf
; compile-SET!-form in file

(define (compile-SETTER-form forms module-vars bound-vars really-compile
			     multiple-values? continue)
  forms module-vars bound-vars really-compile
  multiple-values? continue
  (dylan::error "bad SETTER syntax" forms))

(define (compile-UNLESS-form
	 forms module-vars bound-vars
	 really-compile multiple-values? continue)
  bound-vars				; Ignored
  (must-be-list-of-at-least-length forms 1 "UNLESS: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile
		 (if (null? (cdr forms)) #F multiple-values?)
    (lambda (forms module-vars)
      (continue
       `(IF (DYLAN::NOT ,(car forms))
	    (BEGIN ,@(if (null? (cdr forms)) (list #F) (cdr forms)))
	    #F)
       module-vars))))

(define (compile-UNTIL-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
  multiple-values?
  (must-be-list-of-at-least-length forms 2 "UNTIL: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile #F
    (lambda (forms module-vars)
      (continue
       `(DYLAN::WHILE (LAMBDA () (DYLAN::NOT ,(car forms)))
		      (LAMBDA () ,@(if (null? (cdr forms))
				       (list #F)
				       (cdr forms))))
       module-vars))))

(define (compile-UNWIND-PROTECT-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "UNWIND-PROTECT: bad syntax")
  (really-compile (car forms) module-vars bound-vars multiple-values?
    (lambda (c-protect module-vars)
      (really-compile `(BEGIN ,@(cdr forms))
		      module-vars bound-vars #F
	(lambda (c-cleanup module-vars)
	  (continue
	   `(DYLAN::DYNAMIC-WIND (LAMBDA () 'DONE)
				 (LAMBDA () ,c-protect)
				 (LAMBDA () ,c-cleanup))
	   module-vars))))))

(define (compile-WHEN-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "WHEN: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile
		 (if (null? (cdr forms)) #F multiple-values?)
    (lambda (forms module-vars)
      (continue
       `(IF ,(car forms)
	    (BEGIN ,@(if (null? (cdr forms)) (list #F) (cdr forms)))
	    #F)
       module-vars))))

(define (compile-WHILE-form
	 forms module-vars bound-vars really-compile
	 multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "UNTIL: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile
		 (if (null? (cdr forms)) #F multiple-values?)
    (lambda (forms module-vars)
      (continue
       `(DYLAN::WHILE (LAMBDA () ,(car forms))
		      (LAMBDA () ,@(if (null? (cdr forms))
				       (list #F)
				       (cdr forms))))
       module-vars))))
