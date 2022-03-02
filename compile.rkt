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
    [(Case e cs e1)    (compile-case e cs e1)]
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
                    (Mov 'r9 'rax)
                    (Mov 'rax 0)
                    (Sub 'rax 'r9)
                    (Label l1)                
                ))]
         ['- (seq   (Mov 'r9 'rax)
                    (Mov 'rax 0)
                    (Sub 'rax 'r9)                
                )]
         ['not (let ((l1 (gensym 'ntrue)))
                (seq (Cmp 'rax val-false)
                     (Mov 'rax val-false)
                     (Jne l1)
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
                                      (Cmp 'rax val-false)
                                      (Jne l1)
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
                                      (Cmp 'rax val-false)
                                      (Jne l1)
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

(define (compile-case e cs e1)
  (let ((l1 (gensym 'case))
        (l2 (gensym 'case)))
    (seq (compile-case-bool cs e)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-case-result cs e)
         (Jmp l2)
         (Label l1)
         (compile-e e1)
         (Label l2)))
  )

(define (compile-case-bool cs e)
  (match cs
    ['() (seq (Mov 'rax val-false))]
    [(cons a b) (match a
                  [(Clause p b1) (let ((l1 (gensym 'casebool))
                                       (l2 (gensym 'casebool)))
                                   (seq (compile-check-in p e)
                                        (Cmp 'rax val-false)
                                        (Jne l1)
                                        (compile-case-bool b e)
                                        (Jmp l2)
                                        (Label l1)
                                        (Mov 'rax val-true)
                                        (Label l2)
                                         ))]
                  )
                ]
    )
  )

(define (compile-case-result cs e)
  (match cs
    ['() (seq (Mov 'rax 10))]
    [(cons a '()) (match a
                  [(Clause p b1) (seq (compile-e b1))]
                  )]
    [(cons a b) (match a
                  [(Clause p b1) (let ((l1 (gensym 'caseresult))
                                       (l2 (gensym 'caseresult)))
                                   (seq (compile-check-in p e)
                                        (Cmp 'rax val-false)
                                        (Jne l1)
                                        (compile-case-result b e)
                                        (Jmp l2)
                                        (Label l1)
                                        (compile-e b1)
                                        (Label l2)
                                         ))]
                  )
                ]
    )
  )

(define (compile-check-in p e)
  (match p
    ['() (seq (Mov 'rax val-false))]
    [(cons a b) (let ((l1 (gensym 'checkin))
                      (l2 (gensym 'checkin)))
                       (seq (compile-e a)
                            (Mov 'r9 'rax)
                            (Push 'r9)
                            (compile-e e)
                            (Pop 'r9)
                            (Cmp 'rax 'r9)
                            (Je l1)
                            (compile-check-in b e)
                            (Jmp l2)
                            (Label l1)
                            (Mov 'rax val-true)
                            (Label l2)
       ))]
    )
  )