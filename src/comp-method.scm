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

; $Id: comp-method.scm,v 1.12 1992/09/11 02:10:54 jmiller Exp $

;;;; More of the compiler: METHOD and DEFINE-METHOD

;;; Parsing and compiling of METHOD form

(define (validate-names names)
  (define (validate name)
    (if (not (variable-name? name))
	(dylan::error "invalid parameter name" name names)))
  (for-each validate names)
  (must-be-unique names memq "parameter names not unique"))

(define (parse-requireds orig-l continuation)
  (define (oops l) (cant-parse "requireds" orig-l l))
  (let loop ((l orig-l)
	     (requireds '()))
    (cond ((null? l) (continuation (reverse requireds) l))
	  ((pair? l)
	   (let ((this-param (car l)))
	     (cond ((or (eq? this-param '!next)
			(eq? this-param '!rest)
			(eq? this-param '!key))
		    (continuation (reverse requireds) l))
		   ((variable-name? this-param)
		    (loop (cdr l)
			  `((,this-param <object>)
			    ,@requireds)))
		   ((and (pair? this-param)
			 (variable-name? (car this-param))
			 (pair? (cdr this-param))
			 (null? (cdr (cdr this-param))))
		    (loop (cdr l)
			  `((,(car this-param)
			     ,(cadr this-param))
			    ,@requireds)))
		   (else (oops l)))))
	  (else (oops l)))))

(define (make-parse-next/rest flag name)
  (lambda (orig-l continuation)
    (cond ((null? orig-l) (continuation #F orig-l))
	  ((pair? orig-l)
	   (let ((this-param (car orig-l))
		 (next-params (cdr orig-l)))
	     (if (and (eq? this-param flag)
		      (pair? next-params)
		      (variable-name? (car next-params)))
		 (continuation (car next-params) (cdr next-params))
		 (continuation #F orig-l))))
	  (else (cant-parse name orig-l orig-l)))))

(define parse-next (make-parse-next/rest '!next "next"))
(define parse-rest (make-parse-next/rest '!rest "rest"))

(define (parse-keys orig-l continuation)
  (define (oops l) (cant-parse "keys" orig-l l))
  (define (loop l keys)
    (cond
     ((null? l) (continuation (if (null? keys) #T (reverse keys))))
     ((pair? l)
      (let ((this-param (car l)))
	(cond
	 ((symbol? this-param)
	  (loop (cdr l)
		`((,(name->keyword this-param) ,this-param #F)
		  ,@keys)))
	 ((pair? this-param)
	  (cond
	   ((keyword? (car this-param))
	    (let ((the-key (car this-param))
		  (the-rest (cdr this-param)))
	      (if (and (pair? the-rest)
		       (symbol? (car the-rest)))
		  (let ((variable (car the-rest))
			(the-rest (cdr the-rest)))
		    (cond
		     ((null? the-rest)
		      (loop (cdr l)
			    `((,the-key ,variable #F)
			      ,@keys)))
		     ((and (pair? the-rest)
			   (null? (cdr the-rest)))
		      (loop (cdr l)
			    `((,the-key ,variable ,(car the-rest))
			      ,@keys)))
		     (else (oops l))))
		  (oops l))))
	   ((symbol? (car this-param))
	    (let ((variable (car this-param))
		  (the-rest (cdr this-param)))
	      (cond
	       ((null? the-rest)
		(loop (cdr l)
		      `((,(name->keyword variable) ,variable #F)
			,@keys)))
	       ((pair? the-rest)
		(let ((expr (car the-rest))
		      (the-rest (cdr the-rest)))
		  (if (null? the-rest)
		      (loop
		       (cdr l)
		       `((,(name->keyword variable) ,variable ,expr)
			 ,@keys))
		      (oops l))))
	       (else (oops l)))))
	   (else (oops l))))
	 (else (oops l)))))
     (else (oops l))))
  (cond ((null? orig-l) (continuation #F))
	((and (pair? orig-l)
	      (eq? (car orig-l) '!key))
	 (loop (cdr orig-l) '()))
	(else (oops orig-l))))

(define (parse-METHOD-parameter-list orig-list default-next-method continue)
  (parse-requireds orig-list
    (lambda (required-params l)
      (parse-next l
        (lambda (next l)
	  (parse-rest l
	    (lambda (rest-param l)
	      (parse-keys l
	        (lambda (keys)
		  (validate-names
		   (append (map car required-params)
			   (if next (list next) '())
			   (if rest-param (list rest-param) '())
			   (if (pair? keys) (map cadr keys) '())))
		  (if (pair? keys)
		      (must-be-unique (map car keys) memq
				      "keywords not unique"))
		  (continue required-params
			    (or next default-next-method)
			    rest-param
			    keys))))))))))

(define (compile-METHOD-form
	 e mod-vars bound-vars compiler multiple-values? continue)
  multiple-values?			; No reductions
  (must-be-list-of-at-least-length e 2 "METHOD -- invalid syntax")
  (let ((params (car e))
	(forms (cdr e)))
    (parse-METHOD-parameter-list params '!NEXT-METHOD
      (lambda (reqs next rest keys)
	(define (lambda-list req-names)
	  `(!MULTI-VALUES ,next ,@req-names .
	      ,(cond (rest rest) (keys '!keys) (else '()))))
	(let ((req-names (map car reqs))
	      (req-restrictions (map cadr reqs))
	      (key-name (if rest rest '!keys)))
	  (compile-forms req-restrictions mod-vars bound-vars compiler #F
	    (lambda (restrictions mod-vars)
	      (define (generate mod-vars validation-code)
		(continue
		 `(DYLAN::MAKE-METHOD
		   (DYLAN::MAKE-PARAM-LIST
		    (DYLAN::LIST
		     ,@(map (lambda (name code)
			      `(DYLAN::LIST ',name ,code))
			    req-names restrictions))
		    ',next ',rest ',(if (pair? keys) (map car keys) keys))
		   (LAMBDA ,(lambda-list req-names) ,@validation-code))
		 mod-vars))
	      (compile-forms
	       forms mod-vars
	       (append (list next) req-names (if rest (list rest) '())
		       (if (pair? keys) (map cadr keys) '()) bound-vars)
	       compiler '!MULTI-VALUES
	       (lambda (body-forms mod-vars)
		 (cond
		  ((not keys) (generate mod-vars body-forms))
		  ((eq? keys #T)
		   (generate mod-vars
			     `((DYLAN::KEYWORD-VALIDATE ,next ,key-name #T)
			       ,@body-forms)))
		  (else
		   (compile-let*-forms
		    (map cadr keys) (map caddr keys) mod-vars
		    (append (list next) req-names
			    (if rest (list rest) '()) bound-vars)
		    compiler #F
		    (lambda (defaults mod-vars)
		      (generate
		       mod-vars
		       `((DYLAN::KEYWORD-VALIDATE
			  ,next ,key-name
			  ,(if rest #T `',(map car keys)))
			 (LET* (,@(map
				   (lambda (key var default)
				     `(,key
				       (DYLAN::FIND-KEYWORD
					,key-name ',var
					(LAMBDA () ,default))))
				       (map cadr keys)
				       (map car keys)
				       defaults))
			   ,@body-forms))))))))))))))))

(define (compile-let*-forms
	 names forms module-vars bound-vars
	 compiler multiple-values? continue)
  (let loop ((result '())
	     (forms forms)
	     (names names)
	     (bound-vars bound-vars)
	     (mod-vars module-vars))
    (if (null? forms)
	(continue (reverse result) mod-vars)
	(compiler (car forms) mod-vars bound-vars multiple-values?
	  (lambda (compiled mod-vars)
	    (loop (cons compiled result) (cdr forms) (cdr names)
		  (cons (car names) bound-vars) mod-vars))))))

(define (compile-DEFINE-METHOD-form
	 e mod-vars bound-vars compiler multiple-values? continue)
  (define (rebuild-param-list params)
    (let loop ((result '())
	       (so-far params))
      (if (null? so-far)
	  (reverse `(NEXT-METHOD !NEXT ,@result))
	  (case (car so-far)
	    ((!next) params)
	    ((!rest !key) `(,@(reverse result) !NEXT NEXT-METHOD ,@so-far))
	    (else (loop (cons (car so-far) result)
			(cdr so-far)))))))
	  
  multiple-values?			; Doesn't reduce
  (must-be-list-of-at-least-length e 3
   "DEFINE-METHOD -- invalid syntax")
  (let ((name (car e))
	(params (cadr e))
	(forms (cddr e)))
    (if (not (variable-name? name))
	(dylan::error "DEFINE-METHOD -- illegal name" name params forms))
    (compiler `(METHOD ,(rebuild-param-list params) ,@forms)
	      mod-vars bound-vars #F
      (lambda (method-code mod-vars)
	(module-refs
	 name bound-vars mod-vars
	 continue
	 (lambda (ref set)
	   (parse-METHOD-parameter-list params #F
	     (lambda (reqs next rest keys)
	       `(BEGIN
		  (COND ((DYLAN::EQ? ,ref ',the-unassigned-value)
			 ,(set `(DYLAN::CREATE-GENERIC-FUNCTION
				 ',(variable->name name)
				 ,(length reqs)
				 #F	; No required keywords
				 ,(if (or keys rest) #T #F))))
			((DYLAN::NOT (DYLAN::GENERIC-FUNCTION? ,ref))
			 (DYLAN-CALL DYLAN:ERROR
				     "DEFINE-METHOD -- already has a value"
				     ',name ,ref ',reqs ',next ',rest ',keys)))
		  (DYLAN::ADD-METHOD ,ref ,method-code)
		  ',name)))))))))

(define (compile-BIND-METHODS-form
	 e mod-vars bound-vars compiler multiple-values? continue)
  (must-be-list-of-at-least-length e 2 "BIND-METHODS -- bad syntax")
  (let ((bindings (car e))
	(forms (cdr e)))
    (for-each (lambda (binding)
		(must-be-list-of-at-least-length binding 3
		  "BIND-METHODS -- illegal binding syntax"))
	      bindings)
    (let ((names (map car bindings))
	  (methods (map (lambda (binding)
			  `(METHOD ,(cadr binding)
				   ,@(cddr binding)))
			bindings)))
      (let ((new-bvs (append names bound-vars)))
	(compile-forms methods mod-vars new-bvs compiler #F
	  (lambda (method-bodies mod-vars)
	    (compile-forms forms mod-vars new-bvs compiler multiple-values?
	      (lambda (body-codes mod-vars)
		(continue
		 `(LETREC (,@(map list names method-bodies))
		    ,@body-codes)
		 mod-vars)))))))))
