;;; Additions from MIT C-Scheme.

(module mit)

(define-external AFTER-GC top-level)

(define-external (WEAK-CONS x y) sc)

(include "poplat.sc")
(include "hash.sc")
(include "record.sc")
(include "msort.sc")

