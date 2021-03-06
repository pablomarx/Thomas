; "dynwind.scm", wind-unwind-protect for Scheme
; Copyright (c) 1992, Aubrey Jaffer

;This facility is a generalization of Common Lisp `unwind-protect',
;designed to take into account the fact that continuations produced by
;CALL-WITH-CURRENT-CONTINUATION may be reentered.

;  (dynamic-wind <thunk1> <thunk2> <thunk3>)		procedure

;The arguments <thunk1>, <thunk2>, and <thunk3> must all be procedures
;of no arguments (thunks).

;DYNAMIC-WIND calls <thunk1>, <thunk2>, and then <thunk3>.  The value
;returned of <thunk2> is returned as the result of DYNAMIC-WIND.
;<thunk3> is also called just before <thunk2> calls any continuations
;created by CALL-WITH-CURRENT-CONTINUATION.  If <thunk2> captures its
;continuation as an escape procedure, <thunk1> is invoked just before
;continuing that continuation.

(define *winds* '())

(define (dynamic-wind <thunk1> <thunk2> <thunk3>)
  (<thunk1>)
  (set! *winds* (cons (cons <thunk1> <thunk3>) *winds*))
  (let ((ans (<thunk2>)))
    (set! *winds* (cdr *winds*))
    (<thunk3>)
    ans))

(define call-with-current-continuation
  (let ((oldcc call-with-current-continuation))
    (lambda (proc)
      (let ((winds *winds*))
	(oldcc
	 (lambda (cont)
	   (proc (lambda (c2)
		   (dynamic:do-winds *winds* winds)
		   (cont c2)))))))))

(define (dynamic:do-winds from to)
  (set! *winds* from)
  (cond ((eq? from to))
	((null? from)
	 (dynamic:do-winds from (cdr to))
	 ((caar to)))
	((null? to)
	 ((cdar from))
	 (dynamic:do-winds (cdr from) to))
	(else
	 ((cdar from))
	 (dynamic:do-winds (cdr from) (cdr to))
	 ((caar to))))
  (set! *winds* to))
