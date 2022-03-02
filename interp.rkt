#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    ;; TODO: Handle cond
    [(Cond cs e) (if (clause-bool cs) (clause-result cs) (interp e))]
    ;; TODO: Handle case
    [(Case e cs el) (if (case-bool cs e) (case-result cs e) (interp el))]
    ))

(define (clause-bool cs)
  (match cs
    ['() #f]
    [(cons a b) (match a
                  [(Clause p b1) (if (interp p) #t (clause-bool b))]
                  )
                ]
   )
  )

(define (clause-result cs)
  (match cs
    [(cons a '()) (match a
                  [(Clause p b1) (interp b1)]
                  )]
    [(cons a b) (match a
                  [(Clause p b1) (if (interp p) (interp b1) (clause-result b))]
                  )
                ]
   )
  )

(define (case-bool cs e)
  (match cs
    ['() #f]
    [(cons a b) (match a
                  [(Clause p b1) (if (check-in p e) #t (case-bool b e))]
                  )
                ]
   )
  )

(define (case-result cs e)
  (match cs
    [(cons a '()) (match a
                  [(Clause p b1) (interp b1)]
                  )]
    [(cons a b) (match a
                  [(Clause p b1) (if (check-in ) (interp b1) (case-result b e))]
                  )
                ]
   )
  )

(define (check-in p e)
  (match p
    ['() #f]
    [(cons a b) (if (equal? (interp a) (interp e)) #t (check-in b e))]
   )
  )