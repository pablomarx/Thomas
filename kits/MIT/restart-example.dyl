; -*- Scheme -*-
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

; $Id: MIT_restart-example.dyl,v 1.1 1992/09/22 20:55:21 birkholz Exp $

;;;; Example from pages 144-6.  (Punted initial example code.)
;;;; Many changed were necessary.  So many that they aren't noted here.

;;; Classes such as <file-not-found> used in these examples are
;;; invented for the example and are not part of the specification
;;; This example shows minimal handling of a file-not-found error

;;; This is the same example improved so the restart handler that
;;; reads another file can only be reached by a handler for the
;;; associated condition, useful if there are nested errors.

(define (operating-system-open filename)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler
	 (list condition-type:file-operation-error)
	 (lambda (condition)
	   (if (and (string=? "open" (access-condition condition 'VERB))
		    (string=? "file" (access-condition condition 'NOUN))
		    (string-ci=? "no such file or directory"
				 (access-condition condition 'REASON)))
	       (continuation 'file-not-found)))
       (lambda ()
	 (open-input-file filename))))))
;;;Value: operating-system-open

(define-class <file-not-found> (<error>)
  (file-name init-keyword: file-name:))
;;;Value: <file-not-found>

(define-class <try-a-different-file> (<restart>)
  (condition init-keyword: condition:
	     getter: restart-condition)
  (file-name init-keyword: file-name:))
;;;Value: <try-a-different-file>

(define-method open (the-file)
  (bind ((result ((scheme-procedure 'operating-system-open) the-file)))
    (cond ((id? result 'file-not-found)
	   (bind ((condition (make <file-not-found> file-name: the-file)))
	     (handler-case (error condition)
	       ((<try-a-different-file>
		 test: (compose (curry id? condition) restart-condition)
		 condition: restart
		 description:
		 (method (stream)
		   (format stream
			   "Read a different file instead of ~A" the-file)))
		(open (file-name restart))))))
	  (else: result))))
;;;Value: open

(handler-bind (<file-not-found>
	       (method (condition next-handler)
		 (signal (make <try-a-different-file>
			       condition: condition
			       file-name: "/dev/null"))))
  (open "file-that-doesnt-exist")
  )
;;;Value: #[input-port 778 for file: #[pathname 779 "/dev/null"]]
