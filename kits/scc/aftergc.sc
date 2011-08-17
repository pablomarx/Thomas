;;; Scheme->C
;;;
;;; An orderly extension to AFTER-COLLECT to allows a number of modules to
;;; add and delete a cleanup procedure.

(module aftergc (top-level after-gc))

(define TAG-CLEANUPS '())	;;; A-list of tags and clean-up procedures.

(set! after-collect
    (lambda ignore
	    (for-each (lambda (tag-cleanup) ((cdr tag-cleanup)))
		tag-cleanups)))

(define (AFTER-GC tag cleanup)
    (let ((x (assoc tag tag-cleanups)))
	 (if x (set! tag-cleanups (remq! x tag-cleanups)))
	 (if tag (set! tag-cleanups (cons (cons tag cleanup) tag-cleanups)))
	 tag))
