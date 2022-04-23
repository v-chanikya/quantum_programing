#lang racket
;#lang typed/racket/no-check

;debug
(require racket/trace)
(require syntax/macro-testing)
(require macro-debugger/stepper)
(require racket/generic)

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
(struct 0> ([amp : Int]) #:transparent)
(struct 1> ([amp : Int]) #:transparent)

(struct primitive ([prim : (U 0> 1>)]) #:transparent)
(struct ψ ([psi : (Listof primitive)]) #:transparent)

; Gates
(define-generics eval
  (abc eval a))
(struct H ()
  #:transparent
  #:methods gen:eval
  [(define abc
     (λ (a)
       #t))])
(struct cnot ()  #:transparent)
(struct S ()  #:transparent)
(struct R3 ()  #:transparent)
(struct universal-gates ([gate : (U H cnot S R3)]))

(define constant?
  (λ (c)
    ;(printf "constant check for ~a\n" c)
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
(syntax-parse `(λ! (x) ,(0> 1))) ;invalid
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

#|
; Macros to build λc from math

; syntax for quantum
(define-syntax H
  (syntax-rules (0> 1> + -)
    [(H ()) #f]
    [(H (ψ0 + ψ1 ψ ...)) (+ (H ψ0) (H ψ1) (H (ψ ...)))]
    [(H (ψ0 - ψ1 ψ ...)) (- (H ψ0) (H ψ1) (H (ψ ...)))]   
    [(H (+ ψ0 ψ ...)) (+ (H ψ0) (H (ψ ...)))]
    [(H (- ψ0 ψ ...)) (- (H ψ0) (H (ψ ...)))]
    [(H (ψ0 ψ ...)) ((H ψ0) (H (ψ ...)))]
    [(H 0>) (Hc (0>c 1))]
    [(H 1>) (Hc (1>c 1))]))


(define +
  (λ (ψ1 ψ2 . args)
    (match `(,ψ1 ,ψ2)
      [`(,0>c ,0>c) (0>c 2)]
      [`(1>c 1>c) 1>c]
      [`(0>c 1>c) (λ () (+ 0>c 1>c))]
      [`(1>c 0>c) (λ () (+ 0>c 1>c))])))

;(H 0>)
;(H 1>)
;(H (0>))
;(H (0> + 1>))
;(H (0> - 1>))

|#


