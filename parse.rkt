#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list 'add1 e)  (Prim1 'add1 (parse e))]
    [(list 'sub1 e)  (Prim1 'sub1 (parse e))]
    ;; TODO: Handle abs, - and not
    [(list 'abs e)  (Prim1 'abs (parse e))]
    [(list '- e)  (Prim1 '- (parse e))]
    [(list 'not e)  (Prim1 'not (parse e))]
    [(list 'zero? e) (Prim1 'zero? (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    ;; TODO: Handle cond  
    [(list 'cond cs ...)
     (if (equal? (car (last cs)) 'else) (Cond (parse-clause cs) (parse (car(cdr (last cs))))) (error "parse error"))]
    [(list 'case e1 cs ...)
     (if (equal? (car (last cs)) 'else) (Case (parse e1) (parse-clause-case cs) (parse(car(cdr (last cs))))) (error "parse error"))]
    ;; TODO: Handle case
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (Int 0)]
    [_ (error "parse error")]))


(define (parse-clause cs)
  (match cs
    [(cons a '()) '()]
    [(cons a c) (match a
                  [(cons e1 e2) (cons (Clause (parse e1) (parse (car e2))) (parse-clause c))]
                  )
                ]
   )
  )


(define (parse-clause-case cs)
  (match cs
    [(cons a '()) '()]
    [(cons a c) (match a
                  [(cons e1 e2) (cons (Clause (parse-list e1) (parse (car e2))) (parse-clause c))]
                  )
                ]
   )
  )

(define (parse-list e1)
  (match e1
    ['() '()]
    [(cons a c) (cons (parse a) (parse-list c))]
    )
  )
