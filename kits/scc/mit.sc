;;; Additions from MIT C-Scheme.
; $Id: scc_mit.sc,v 1.3 1992/09/23 15:06:35 birkholz Exp $

(module mit)

(define-external AFTER-GC top-level)

(define-external (WEAK-CONS x y) sc)

(include "poplat.sc")
(include "hash.sc")
(include "record.sc")
(include "msort.sc")
