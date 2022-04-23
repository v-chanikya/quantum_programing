#lang typed/racket/no-check
(require racket/trace)


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


; Constants
(struct (ψ) Hc ([psi : ψ]) #:transparent)
(struct cnotc () #:transparent)
(struct 0>c ([amp : Int]) #:transparent)
(struct 1>c ([amp : Int]) #:transparent)

(define-type (constant c)
  (U (Hc c)
     cnotc
     0>c
     1>c))

(define constant?
  (λ (c)
    (cond
      [(Hc? c)]
      [(cnotc? c)]
      [(0>c? c)]
      [(1>c? c)]
      [else #f])))

; Nonlinear
(struct !t () #:transparent)

(define-type (nonlinear)
  (U !t))

(define nonlinear?
  (λ (t)
    (cond
      [(!t? t)]
      [else #f])))

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

(require syntax/macro-testing)
(require macro-debugger/stepper)
;(H 0>)
;(H 1>)
;(H (0>))
;(H (0> + 1>))
;(H (0> - 1>))

; lambda calculus parser
(define parse
  (λ (e)
    (match e
      [`,n #:when (number? n) n]
      [`,c #:when (constant? c) c]
      [`,t #:when (nonlinear? t) t]
      [`,y #:when (symbol? y) y]
      [`(λ (,x) ,body) #:when (symbol? x) `(λ (,x) ,(parse body))]
      [`(λ! (,x) ,body) #:when (symbol? x) `(λ! (,x) ,(parse body))]
      [`(,rator ,rand) `(,(parse rator) ,(parse rand))]
      [else (error "Given expression not a valid lambda calculus expression")])))

(trace parse)
;(define prog (H (0> + 1>)))
;(parse '(H (0> + 1>)))
(parse `( 0>c))









