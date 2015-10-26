; in scheme the switch is completelly unneeded
; (define (switch f) (lambda (x y) (f y x)))
; (define recons (switch cons))
(define (myrev l) (fold cons '() l))

