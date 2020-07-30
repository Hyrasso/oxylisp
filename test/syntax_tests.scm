(load 'src/libs/derived_forms.scm)

; (letrec* ((p
;     (lambda (x)
;         (+ 1 (q (- x 1)))))
;     (q
;     (lambda (y)
;         (if (zero? y)
;             0
;             (+ 1 (p (- y 1))))))
;     (x (p 5))
;     (y x))
;     y)

; (define p
;     (lambda (x)
;         (+ 1 (q (- x 1)))))
(define q (lambda (y)
        (if (zero? y)
            0
            (+ 1 (+ 1 (q (- (- y 1) 1)))))))
(define x (+ 1 (q (- 5 1))) 5)
(define y x)
(y)