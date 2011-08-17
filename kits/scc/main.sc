;;; Main program for extensions to 01nob91jfb Scheme->C

(module main (main main) (with aftergc mit sccspecific))

(define (MAIN clargs)
    (set-write-circle! #t stdout-port)
    (set-write-length! 20 stdout-port)
    (set-write-level! 5 stdout-port)
    (set-write-circle! #t stderr-port)
    (set-write-length! 20 stderr-port)
    (set-write-level! 5 stderr-port)
    (read-eval-print))
