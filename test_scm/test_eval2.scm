(define (list . x) x)
(define (null? l) (== '() l))
(null? '())
(null? '(1))
(null? 5)
(define (sequence x) (if (null? (cdr x)) (car x) (sequence (cdr x))))
(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))

(define (factorial-debug x)
    (if (= x 1)
        1
        (sequence (list
            (write (list "finding factorial of " x))
            (* x (factorial-debug (- x 1)))
        ))
    )
)
(factorial-debug 10)
(define x 5)
x
(define x 10)
x
(* x x x x)
(string-ref x x)
(define s "tomorrow!!!")
(define (length xs) (if (null? xs)
                        0
                        (+ 1 (length (cdr xs)))
                    ))
(define (string-length s) (length (string->list s)))
(length (list 1 2 3 4 5 6))
(string-ref s (- (string-length s) 1))
x

