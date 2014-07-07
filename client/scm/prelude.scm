(define string? (lambda (x)
  (if (data? x) (?string? x) #f)))

(define boolean? (lambda (x)
  (if (data? x) (?boolean? x) #f)))

(define null? (lambda (x)
  (if (data? x) (?null? x) #f)))

(define number? (lambda (x)
  (if (data? x) (?number? x) #f)))





(define and (lambda (x y)
  (begin
    (if (data? x) #null (set! x #t))
    (if (data? y) #null (set! y #t))
    (?and x y))))

(define or (lambda (x y)
  (begin
    (if (data? x) #null (set! x #t))
    (if (data? y) #null (set! y #t))
    (?or x y))))

(define not (lambda (x)
  (if (data? x) #f (?not x))))





(define equal? (lambda (x y)
  (if (?and (data? x) (data? y))
    (?equal? x y)
    (*equal? x y))))



(define < (lambda (x y)
  (if (?and (number? x) (number? y))
    (?< x y)
    (failure "type error on <"))))

(define <= (lambda (x y)
  (if (?and (number? x) (number? y))
    (?<= x y)
    (failure "type error on <="))))
 
(define + (lambda (x y)
  (if (?and (number? x) (number? y))
    (?+ x y)
    (failure "type error on +"))))

(define - (lambda (x y)
  (if (?and (number? x) (number? y))
    (?- x y)
    (failure "type error on -"))))

(define * (lambda (x y)
  (if (?and (number? x) (number? y))
    (?* x y)
    (failure "type error *"))))

(define / (lambda (x y)
  (if (?and (number? x) (number? y))
    (if (?equal? y 0.0)
      (failure "division by zero")
      (?/ x y))
    (failure "type error on /"))))





(define data->string (lambda (x)
  (if (data? x)
    (?data->string x)
    (failure "type error on data->string"))))

(define string->data (lambda (x)
  (if (string? x)
    (if (null? (?string->data x))
      (failure "parser-error")
      (?string->data x))
    (failure "type error on string->data"))))






(define write (lambda (x)
  (!write (data->string x))))

(define read (lambda ()
  (string->data (!read))))



