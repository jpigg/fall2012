;Excercise 1
(define square (lambda (x) (* x x)))
(define squareFirst (lambda (x) (square (car x))))
;Excercise 2
(define showCons (lambda (x y) (cons x y)))
(define showAppend (lambda (x y) (append (list x) y)))
(define showList (lambda (x y) (list x y)))
;Excercise 3
(define half (lambda (x) (/ x 2)))
;Excercise 4
(define theParts (lambda (x)
                   (display "car: ")(display (car x))(write-char #\newline)
                   (display "cdr: ")(display (cdr x))(write-char #\newline)
                   (display "caar: ")(display (caar x))(write-char #\newline)
                   (display "cadar: ")(display (cadar x))(write-char #\newline)
                             ))
;Excercise 5
