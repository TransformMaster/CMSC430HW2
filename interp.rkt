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