#lang racket
;#lang typed/racket/no-check

;debug
(require racket/trace)
(require syntax/macro-testing)
(require macro-debugger/stepper)
(require racket/generic)
(require racket/vector)

; Quantum Lambda Calculus
#|
λexpr :==  y                        ; Variable 
             | (λ (x)  λexpr)       ; Function 
             | (λexpr λexpr)        ; Application 
             | c                    ; Constant 
             | !t                   ; Nonlinear term 
             | (λ! (x)  λexpr)      ; Nonlinear function 
c :== Constants: 
            0 | 1 | H | S | R3 | cnot | X | Y | Z | . . .
|#


;; Constants
; Primitives 
(struct 0> (amp) #:transparent) ; [amp : Int]
(struct 1> (amp) #:transparent) ; [amp : Int]

(struct primitive (prim) #:transparent) ;[prim : (U 0> 1>)]
(struct ψ (psi) #:transparent) ; [psi : (Listof (Vectorof primitive))]

; Gates
(define-generics eval-qc
  (evalq eval-qc θ))

; helper
(define θ0
  (λ (θ)
    (vector->list (car (ψ-psi θ)))))

(define ψcon
  (λ (ls)
    (ψ (list (list->vector ls)))))

(define ψsp
  (λ (ls)
    (ψ (map vector ls))))

(struct H ()
  #:transparent
  #:methods gen:eval-qc
  [(define evalq
     (λ (eval-qc θ)
       (match (θ0 θ)
         [`(,x) #:when (0>? x)
              (ψsp (list (0> (* (0>-amp x) (sqrt 0.5))) (1> (* (0>-amp x) (sqrt 0.5)))))]
         [`(,x) #:when (1>? x)
              (ψsp (list (0> (* (1>-amp x) (sqrt 0.5))) (1> (* -1 (1>-amp x) (sqrt 0.5)))))])))])

(struct cnot ()
  #:transparent
  #:methods gen:eval-qc
  [(define evalq
     (λ (eval-qc θ)
       (match (θ0 θ)
         [`(,x  ,y) #:when (0>? x) θ]
         [`(,x  ,y) #:when (1>? x)
                     (cond
                       [(0>? y) (ψcon (list x (1> (0>-amp y))))]
                       [(1>? y) (ψcon (list x (0> (1>-amp y))))])]
         [else (error "no match")])))])

(struct S ()  #:transparent)
(struct R3 ()  #:transparent)

;; Create superposition
(define +
  (λ (θ1 θ2)
    (normalize (ψ (append (ψ-psi θ1) (ψ-psi θ2))))))

(define normalize
  (λ (θ)
    θ))


(struct universal-gates (gate)) ; [gate : (U H cnot S R3)]

(define constant?
  (λ (c)
    (or (H? c)
        (cnot? c)
        (S? c)
        (R3? c)
        (0>? c)
        (1>? c))))

(define linear?
  (λ (l)
    (or (ψ? l))))

;; Validate QC
; lambda calculus parser
(define syntax-parse
  (λ (e)
    (match e
      [`,c #:when (constant? c) c]
      [`,x #:when (linear? x) x]
      [`,!t #:when (symbol? !t) !t]
      [`(λ (,x) ,body) #:when (symbol? x) `(λ (,x) ,(syntax-parse body))]
      [`(λ! (,x) ,body) #:when (symbol? x) `(λ! (,x) ,(syntax-parse body))]
      [`(,rator ,rand) `(,(syntax-parse rator) ,(syntax-parse rand))]
      [else (error "Given expression not a valid lambda calculus expression")])))


;(trace syntax-parse)
; test program
(syntax-parse (0> 1))
(syntax-parse (1> 1))
(syntax-parse (H))
(syntax-parse 'a)
(syntax-parse '(λ! (x) x))
(syntax-parse '((λ! (x) x) y))
(syntax-parse '(λ (x) x))
(syntax-parse '((λ (x) x) y))
(syntax-parse '(((λ (x) (λ (y) x)) z) k))
(syntax-parse `(λ! (x) ,(0> 1))) ;invalid |0>
(syntax-parse `((λ! (x) x) ,(0> 1))) ; invalid
(syntax-parse `(λ (x) ,(0> 1)))
(syntax-parse `((λ (x) x) ,(0> 1)))

; symantic checker for QC
(define symantic-check
  (λ (e)
    (match e
      [`,c #:when (constant? c) c]
      [`,x #:when (linear? x) x]
      [`,!t #:when (symbol? !t) !t]
      [`(λ (,x) ,body) #:when (symbol? x) `(λ (,x) ,(symantic-check body))]
      [`(λ! (,x) ,body) #:when (symbol? x) `(λ! (,x) ,(symantic-check body))]
      [`(,rator ,rand) `(,(symantic-check rator) ,(symantic-check rand))]
      [else (error "Invalid symantics")])))

; β reduction of QC
(define reduce
  (λ (e)
    (match e
      [`,c #:when (constant? c) c]
      [`,x #:when (linear? x) x]
      [`,!t #:when (symbol? !t) !t]
      [`(λ (,x) ,body) #:when (symbol? x) `(λ (,x) ,(reduce body))]
      [`(λ! (,x) ,body) #:when (symbol? x) `(λ! (,x) ,(reduce body))]
      [`(,rator ,rand) `(,(reduce rator) ,(reduce rand))]
      [else (error "")])))

(define run-qc
  (λ (qc)
    (begin
      (syntax-parse qc)
      (symantic-check qc)
      (reduce qc))))

;; Test cases

; How ψ is represented
; each vector represents a nbit qbit
; the list represents the superposition of states

;; examples:
; 1 q-bit
; |0> 
(define 0>c
  (ψ (list (vector (0> 1)))))
; |1>
(define 1>c
  (ψ (list (vector (1> 1)))))

; superposition 1/√2(|0> + |1>)
(define +>
  (ψ (list (vector (0> (sqrt 0.5))) (vector (1> (sqrt 0.5))))))
; superposition 1/√2(|0> - |1>)
(define ->
  (ψ (list (vector (0> (sqrt 0.5))) (vector (1> (* -1 (sqrt 0.5)))))))


; 2 q-bits
; |00>
(define 00>c
  (ψ (list (vector (0> 1) (0> 1)))))
; |10>
(define 10>c
  (ψ (list (vector (1> 1) (0> 1)))))
; |01>
(define 01>c
  (ψ (list (vector (0> 1) (1> 1)))))
; |11>
(define 11>c
  (ψ (list (vector (1> 1) (1> 1)))))


; superposition of 2 qbits bell states
; Φ+  1/√2 (|00> + |11>)
(define Φ+
  (ψ (list (vector (0> (sqrt 0.5)) (0> (sqrt 0.5)))
           (vector (1> (sqrt 0.5)) (1> (sqrt 0.5))))))
; Φ-  1/√2 (|00> + |11>)
(define Φ-
  (ψ (list (vector (0> (sqrt 0.5)) (0> (sqrt 0.5)))
           (vector (1> (* -1 (sqrt 0.5))) (1> (* -1 (sqrt 0.5)))))))
; Φ+  1/√2 (|00> + |11>)
(define Ψ+
  (ψ (list (vector (0> (sqrt 0.5)) (1> (sqrt 0.5)))
           (vector (1> (sqrt 0.5)) (0> (sqrt 0.5))))))
; Φ+  1/√2 (|00> + |11>)
(define Ψ-
  (ψ (list (vector (0> (sqrt 0.5)) (0> (sqrt 0.5)))
           (vector (1> (* -1 (sqrt 0.5))) (0> (* -1 (sqrt 0.5)))))))


(print "abcd---")
; Hadaramart gate
(evalq (H) 0>c)
(evalq (H) 1>c)

; control-not gate
10>c
(evalq (cnot) 10>c)
11>c
(evalq (cnot) 11>c)

; gate operation on superposition
(define evalsp
  (λ (G θ)
    (cond
     [(null? (ψ-psi θ)) (ψ (list))]
     [else (ψ (append (ψ-psi (evalq G (ψ (list (car (ψ-psi θ))))))
                      (ψ-psi (evalsp G (ψ (cdr (ψ-psi θ)))))))])))
+>
(evalsp (H) +>)
->
(evalsp (H) ->)

Φ+
(evalsp (cnot) Φ+)
Φ-
(evalsp (cnot) Φ-)
Ψ+
(evalsp (cnot) Ψ+)
Ψ-
(evalsp (cnot) Ψ-)

; operate on single q-bit in n-qbit state
(define *@
  (λ (n θ1 θ2)
    (cond
      [(null? (ψ-psi θ2)) (ψ (list))]
      [else (let ([middle (car (ψ-psi θ2))])
              (let-values ([(left right) (vector-split-at (car (ψ-psi θ1)) n)])
                (ψ (append (list (vector-append left
                                                (vector-append middle right)))
                           (ψ-psi (*@ n θ1 (ψ (cdr (ψ-psi θ2)))))))))])))

(*@ 1 00>c +>)

(define ψ@
  (λ (n θ)
    (cond
      [(null? (ψ-psi θ)) (values (ψ (list)) (ψ (list)))]
      [else (let-values ([(θ1 θ2) (ψ@ n (ψ (cdr (ψ-psi θ))))])
              (let-values ([(left remaining) (vector-split-at (car (ψ-psi θ)) n)])
                (let-values ([(middle right) (vector-split-at remaining 1)])
                  (values (ψ (append (list (vector-append left right)) (ψ-psi θ1)))
                          (ψ (append (list middle) (ψ-psi θ2)))))))])))

(ψ@ 1 (*@ 1 00>c +>))

(define reduceψ
  (λ (ls)
    (cond
      [(null? ls) (ψ (list))]
      [else (ψ (append (ψ-psi (car ls))
                       (ψ-psi (reduceψ (cdr ls)))))])))

(define G@
  (λ (G n θ)
    (let-values ([(θ1 θ2) (ψ@ n θ)])
      (let ([θ2^ (map (λ (θ)
                       (evalq G θ))
                     (map (λ (l)
                            (ψ (list l))) (ψ-psi θ2)))])
        (reduceψ (map (λ (θ1 θ2)
                        (*@ n θ1 θ2))
                      (map (λ (l)
                             (ψ (list l))) (ψ-psi θ1))
                      θ2^))))))

(G@ (H) 1 (*@ 1 00>c +>))



; Construction of bell pair
; |00> ---H1---> ---cnot---> 1/√2(|00> + |11>)
(printf "Bell pair creation\n")
00>c
(G@ (H) 0 00>c)
(evalsp (cnot) (G@ (H) 0 00>c))


; Deutsch Algorithm
(printf "Deutsch algorithm\n")
01>c
(G@ (H) 0 01>c)
(G@ (H) 1 (G@ (H) 0 01>c))

(define DUf ;Assume f is idenity function 
  (λ (θ)
    #f))


