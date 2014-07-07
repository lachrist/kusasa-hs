
(define pow (lambda (base exp)
  (if (equal? exp 1.0)
    x
    (* x (pow base (- exp 1.0))))))

(define x (read))
(define y (read))
(define z (read))

(write (+ (pow x y) z))
