; -*- Scheme -*-

;;;; **** Example from pages 144-6.

;;; Classes such as <file-not-found> used in these examples are
;;; invented for the example and are not part of the specification
;;; This example shows minimal handling of a file-not-found error

;;; **** Punted initial example code.  Just testing improved example with
;;; reordered definitions below.

;;; This is the same example improved so the restart handler that
;;; reads another file can only be reached by a handler for the
;;; associated condition, useful if there are nested errors.

;; **** Added SCHEME definition of operating-system-open.
(define (operating-system-open filename)
  (or (##open-input-file filename) 'file-not-found))
;;;Value: operating-system-open

(define-class <file-not-found> (<error>)
  (file-name init-keyword: file-name:))	; **** lose extra parens around slots
;;;Value: <file-not-found>

;;; **** lose (define-method print... )

(define-class <try-a-different-file> (<restart>)
  (condition init-keyword: condition:	; **** lose extra parens around slots
	     getter: restart-condition)	; **** getter:, not reader:
  (file-name init-keyword: file-name:))
;;;Value: <try-a-different-file>

;;; **** Many things:
;;; 1. Combine open and guts-of-open.
;;; 2. Use scheme-procedure to reference operating-system-open.
;;; 3. Punt (instance? result <stream>) clause -- there's no <stream>.
;;; 4. Use 'file-not-found rather than +file-not-found-error-code+.
;;; 5. Description: is never used, so format is OK.  (It ain't implemented.)
;;; 6. (else: result) rather than dots.
;;; 7. Get parens right!
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

;;; **** Include handler method in handler clause with <file-not-found>.
;;; **** /dev/null probably exists.  my-emergency-backup-file probably doesn't.
(handler-bind (<file-not-found>
	       (method (condition next-handler)
		 (signal (make <try-a-different-file>
			       condition: condition
			       file-name: "/dev/null"))))
  (open "file-that-doesnt-exist")
  )					; **** lose dots
;;;Value: #[input-port "/dev/null"]
