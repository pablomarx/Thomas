;;; Main program for extensions to 01nob91jfb Scheme->C
; $Id: scc_main.sc,v 1.2 1992/09/23 15:15:50 birkholz Exp $

(module main (main main) (with aftergc mit sccspecific))

(define (MAIN clargs)
    (set-write-circle! #t stdout-port)
    (set-write-length! 20 stdout-port)
    (set-write-level! 5 stdout-port)
    (set-write-circle! #t stderr-port)
    (set-write-length! 20 stderr-port)
    (set-write-level! 5 stderr-port)
    (read-eval-print))
