(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (pair) (cons first pair)) rests)
)

(define (zip pairs)
  (list 
    (map (lambda (pair) (car pair)) pairs) 
    (map (lambda (pair) (cadr pair)) pairs)))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper lst index)
      (if (null? lst) nil
      (cons 
        (list index (car lst))
        (helper (cdr lst) (+ index 1))
        ))
  )
  (helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((null? denoms) nil)
    ((< total 0) nil)
    ((= total 0) (cons (cons (car denoms) nil) nil))
    ((= (car denoms) total) (append (list-change (- total (car denoms)) denoms) (list-change total (cdr denoms))))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))

  )
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr)) ; lambda
               (params (cadr expr)) ; (let a b)
               (body   (cddr expr))) ;(+ let a b)
           ; BEGIN PROBLEM 19
          (append (list form params) (map let-to-lambda body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
            (append `((lambda ,(car (zip values)) ,(let-to-lambda (car body)))) 
              (let-to-lambda (cadr (zip values))))
              )
            )
           ; END PROBLEM 19
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
