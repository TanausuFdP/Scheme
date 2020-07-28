(require errortrace) ;; Ofrece más información al producirse un error
(require racket/trace) ;; Permite seguir la ejecución de una función usando antes (trace nombre_de_función)

(define (fibo n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (+ (fibo (- n 1)) (fibo (- n 2)))
        )
    )
)

(define (expo b n)
    (if (= n 0)
        1
        (* b (expo b (- n 1)))
    )
)

(define (minimo l)
    (if (null? (cdr l))
        (car l)
        (if (< (car l) (car (cdr l)))
            (minimo (cons (car l) (cdr (cdr l))))
            (minimo (cdr l))
        )
    )
)

(define (inserta n l)
    (if (null? l)
        (list n)
        (if (< n (car l))
            (cons n l)
            (cons (car l) (inserta n (cdr l)))
        )
    )
)

(define (concatena l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (concatena (cdr l1) l2))
    )
)

(define (invierte l)
    (if (or (null? l) (null? (cdr l)))
        l
        (concatena (invierte (cdr l)) (list (car l)))
    )
)

(define (elimina e l)
    (if (null? l)
        l
        (if (equal? e (car l))
            (elimina e (cdr l))
            (cons (car l) (elimina e (cdr l)))
        )
    )
)
(define (auxiliar l)
    (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (auxiliar (elimina (car l) (cdr l))))
    )
)
(define (repetidos l)
    (if (or (null? l) (null? (cdr l)))
        l
        (invierte (auxiliar (invierte l)))
    )
)