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

; $Id: comp-util.scm,v 1.12 1992/09/05 16:05:18 jmiller Exp $

;;;; Utility procedures for use at compile time only
;;;;
;;;; Name operations: new-name, name->module-getter,
;;;;                  name->module-setter, name->setter,
;;;;                  variable->name, variable-name?,
;;;;                  name->keyword, dylan-special-name?
;;;; General: cant-parse, list-of-length?, list-of-at-least-length?,
;;;;          must-be-list-of-length, must-be-list-of-at-least-length,
;;;;          set-difference
;;;; Compiler specific: module-refs, add-variable, add-module-variable

;;; Name operations.  These convert from Dylan names (including
;;; (SETTER x) and Keyword:) to Scheme forms.

; new-name is now in support.scm, since it is used at runtime as well as compile time

(define (name->module-getter name)
  (new-name "dylan:module-get/" (variable->name name) "/"))
(define (name->module-setter name)
  (new-name "dylan:module-set/" (variable->name name) "/!"))

(define (name->setter name)
  (new-name "dylan:setter/" name "/"))

(define (name->scheme-safe-name name)
  (new-name "dylan:scheme-safe/" name "/"))

(define scheme-reserved-names
  '(=> and begin case cond define delay do else if lambda let let*
       letrec or quasiquote quote set! unquote unquote-splicing))

(define (variable->name dylan-variable)
  (if (symbol? dylan-variable)
      (cond ((assq dylan-variable dylan::scheme-names-of-predefined-names)
	     => cadr)
	    ((memq dylan-variable scheme-reserved-names)
	     (name->scheme-safe-name dylan-variable))
	    (else dylan-variable))
      (name->setter (cadr dylan-variable))))

(define (variable-name? x)
  (define (simple-variable-name? x)
    (and (symbol? x)
	 (not (keyword? x))
	 (not (dylan-special-name? x))))
  (or (simple-variable-name? x)
      (and (list-of-length? x 2)
	   (eq? (car x) 'SETTER)
	   (simple-variable-name? (cadr x)))))

(define (name->keyword symbol)
  (new-name "" symbol ":"))

(define (dylan-special-name? x)
  ;; The Scheme reader doesn't allow Dylan's #rest, etc. so we
  ;; simulate them with !rest and preclude the use of variable names
  ;; that begin with "dylan:" or "!"
  (and (symbol? x)
       (let* ((string (symbol->string x))
	      (length (string-length string))
	      (chars (string->list string)))
	 (or (and (> length 0) (char=? #\! (car chars)))
	     (and (> length 5) (string-ci=? "dylan:"
					    (substring string 0 6)))))))

; (keyword? obj) is used at runtime: see support.scm

;;; General support operations needed only at compile time

(define (cant-parse reason orig-l l)
  (dylan::error (string-append "illegal parameter list in " reason)
		orig-l l))

(define (list-of-length? l n)
  (cond ((and (= n 0) (null? l)) #T)
	((or (= n 0) (not (pair? l))) #F)
	(else (list-of-length? (cdr l) (- n 1)))))

(define (list-of-at-least-length? l n)
  (and (list? l)
       (let loop ((l l) (n n))
	 (cond ((= n 0) #T)
	       ((not (pair? l)) #F)
	       (else (loop (cdr l) (- n 1)))))))

(define (must-be-list-of-length l n error-message)
  (or (list-of-length? l n)
      (dylan::error error-message l)))

(define (must-be-list-of-at-least-length l n error-message)
  (or (list-of-at-least-length? l n)
      (dylan::error error-message l)))

(define (module-refs
	 variable bound-vars module-vars continue core)
  ;; Called to generate code to deal with direct references to module
  ;; variables. name is the variable name, core is a procedure to
  ;; generate the main body.
  (let* ((name (variable->name variable))
	 (hidden? (memq name bound-vars)))
    (continue
     (if hidden?
	 `(LET ((!OLD-VALUE (,(name->module-getter name))))
	    ,(core '!OLD-VALUE
		   (lambda (val) `(,(name->module-setter name) ,val))))
	 (core name (lambda (val) `(SET! ,name ,val))))
     (add-module-variable name hidden? module-vars))))

;;; Compile a list of forms, producing a list of corresponding Scheme
;;; forms and the (updated) module variables.

(define (add-variable name bound-vars module-vars)
  (if (or (memq name bound-vars)
	  (memq name dylan::predefined-variables))
      module-vars
      (adjoin name module-vars memq)))

(define (add-module-variable
	 name require-accessor-fns module-vars)
  (if (or require-accessor-fns (not (memq name dylan::predefined-names)))
      (adjoin name module-vars memq)
      module-vars))

(define (must-be-unique objects predicate error-string)
  (if (not (unique? objects predicate))
      (dylan::error error-string (car objects) objects)))
