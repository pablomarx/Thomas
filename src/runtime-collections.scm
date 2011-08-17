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

; $Id: runtime-collections.scm,v 1.19 1992/09/09 22:12:55 jmiller Exp $

;;;;; RUNTIME-COLLECTIONS.SCM

;;;;; The specializations of method based on collection type are in the file:
;;;;; runtime-collections-xxx.scm (where xxx is collection type)

;;; Class definitions on pages 111 and 112 imply specialization for
;;; the make operation.
;;;
;;; General theory of operation: some of the collection types are
;;; handled very, very specially to make them more efficient.  In
;;; particular:
;;;  <byte-string> : implemented as a Scheme string,
;;;  <simple-object-vector> : implemented as a Scheme vector
;;;  <list>, <pair>, <empty-list> : implemented in Scheme
;;; 
;;; All other collection types are instances (in the sense of
;;; "created by calling make-instance in class.scm").
;;;
;;; Consequences of this decision: any generic operation on a class
;;; which is a superclass of one of the special classes MUST BE
;;; SPECIALIZED for the special case.  This can either be done by
;;; conditionalizing the code that handles the superclass or by
;;; providing a specific method for the special subclass.


;;; KNOWN PROBLEMS

;;;
;;; UTILITY FUNCTIONS
;;;
(define (one-arg name type fn)
  (dylan::function->method
   (make-param-list `((,name ,type)) #F #F #F) fn))

(define (but-last list)
  (reverse (cdr (reverse list))))

(define (create-private-slot owner type-restriction name continue)
  ;; Adds a new slot to an existing class, and returns the getter and
  ;; setter generic functions to be used for the slot.
  (define the-getter
    (dylan::generic-fn (new-name "dylan:" name "-getter")
      (make-param-list `((ARRAY ,<object>)) #F #F #F)
      #F))

  (define the-setter
    (dylan::generic-fn (new-name "dylan:" name "-setter!")
      (make-param-list `((ARRAY ,<object>) (VALUE ,<object>)) #F #F #F)
      #F))

  (dylan::add-slot owner
		   type-restriction 'INSTANCE the-setter the-getter
		   (new-name "" name "") #F #F #F #F #F)

  (continue the-setter the-getter))
