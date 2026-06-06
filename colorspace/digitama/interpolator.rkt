#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "harmony/base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
; T: threshold for determine the dark and light theme
; k: the steepness of the smoothing function, descrease it for more natural change
(define sigmoid-interpolator
  (lambda [#:name [name : (U False Symbol String) #false]
           #:threshold [T : Brightness-Threshold 0.15] #:k [k : Nonnegative-Flonum 20.0]
           #:smooth-vector [sv : (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum) #(0.5 0.4 0.6)]
           #:dark-range [drk-rng : (Pairof Nonnegative-Flonum Nonnegative-Flonum) (cons 0.2 0.9)]
           #:light-range [lgt-rng : (Pairof Nonnegative-Flonum Nonnegative-Flonum) (cons 0.1 0.8)]
           [:: : (-> Flonum Flonum)]] : (-> Real Flonum)
    (define-values (Tdrk Tlgt) (brightness-threshold T))

    (define (weights [L : Real]) : (Values Flonum Flonum Flonum)
      (define Wdrk (:: (* k (- L Tdrk))))
      (define Wlgt (:: (* k (- L Tlgt))))
      (define sum (+ Wdrk Wlgt))

      (if (> sum 1.0)
          (let ([d (/ Wdrk sum)]
                [l (/ Wlgt sum)])
            (values d l (- 1.0 d l)))
          (values Wdrk Wlgt (- 1.0 Wdrk Wlgt))))

    (define (sigmoid-smooth-interpolate [L : Real]) : Flonum
      (define-values (Wdrk Wlgt Wneu) (weights L))

      ; for (0.5, 0.4, 0.6), the resulting brightness always exceeds 0.6,
      ; making it distinguishable against all backgrounds
      (define Ldrk (~clamp (+ L (vector-ref sv 0)) (car drk-rng) (cdr drk-rng)))
      (define Llgt (~clamp (- L (vector-ref sv 1)) (car lgt-rng) (cdr lgt-rng)))
      (define Lneu (vector-ref sv 2))
      
      (+ (* Wdrk Ldrk) (* Wlgt Llgt) (* Wneu Lneu)))
    
    (procedure-rename sigmoid-smooth-interpolate
                      (cond [(symbol? name) name]
                            [(string? name) (string->symbol name)]
                            [else (string->symbol
                                   (format "~a-interpolate"
                                     (object-name ::)))]))))
