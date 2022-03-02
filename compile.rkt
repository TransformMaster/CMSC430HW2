#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Expr -> Asm
(define (compile e)
  (prog (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Prim1 p e)       (compile-prim p e)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]
    ;; TODO: Handle cond
    [(Cond cs e)    (compile-cond cs e)]
    ;; TODO: Handle case
    ))


;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (value->bits i))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (value->bits b))))

;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ;; TODO: Handle abs, -, and not
         ['abs (let ((l1 (gensym 'positive)))
                (seq (Cmp 'rax 0)
                    (Jg l1)
                    (Mov 'rbx 0)
                    (Sub 'rbx 'rax)
                    (Mov 'rax 'rbx)
                    (Label l1)                
                ))]
         ['- (seq   (Mov 'rbx 0)
                    (Sub 'rbx 'rax)
                    (Mov 'rax 'rbx)                
                )]
         ['not (let ((l1 (gensym 'ntrue)))
                (seq (Cmp 'rax val-true)
                     (Mov 'rax val-false)
                     (Je l1)
                     (Mov 'rax val-true)
                     (Label l1)                
                ))]
         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
         ))) 

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

(define (compile-cond cs e)
  (let ((l1 (gensym 'cond))
        (l2 (gensym 'cond)))
    (seq (compile-clause-bool cs)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-clause-result cs)
         (Jmp l2)
         (Label l1)
         (compile-e e)
         (Label l2))))

(define (compile-clause-bool cs)
  (match cs
    ['() (seq (Mov 'rax val-false))]
    [(cons a b) (match a
                  [(Clause p b1) (let ((l1 (gensym 'clausebool))
                                       (l2 (gensym 'clausebool)))
                                   (seq (compile-e p)
                                      (Cmp 'rax val-true)
                                      (Je l1)
                                      (compile-clause-bool b)
                                      (Jmp l2)
                                      (Label l1)
                                      (Mov 'rax val-true)
                                      (Label l2)
                                      ))]
                  )
                ]
    )
  )


(define (compile-clause-result cs)
  (match cs
    ['() (seq (Mov 'rax 10))]
    [(cons a '()) (match a
                  [(Clause p b1) (seq (compile-e b1))]
                  )]
    [(cons a b) (match a
                  [(Clause p b1) (let ((l1 (gensym 'clauseresult))
                                       (l2 (gensym 'clauseresult)))
                                   (seq (compile-e p)
                                      (Cmp 'rax val-true)
                                      (Je l1)
                                      (compile-clause-result b)
                                      (Jmp l2)
                                      (Label l1)
                                      (compile-e b1)
                                      (Label l2)
                                      ))]
                  )
                ]
    )
  )


