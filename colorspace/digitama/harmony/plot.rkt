#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke-lightness : (-> Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum Flonum)
  (lambda [Tdark Tlight Lf bgL ΔL]
    (cond [(<= bgL Tdark)  (+ Lf ΔL (* 0.25 (log (max 1e-6 (- 2.0 bgL)))))]
          [(>= bgL Tlight) (- Lf ΔL (* 0.05 bgL))]
          [(<= Lf 0.5) (+ Lf ΔL (* 0.1 (- 1.0 bgL)))]
          [else (- Lf ΔL (* 0.1 bgL))])))

(define contrast-lightness : (-> Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum (Option Flonum))
  (lambda [Tdark Tlight Lorigin bgL ΔL]
    (define L (expt (max 0.0 Lorigin) 0.9))
    
    (cond [(and (< bgL Tdark)  (< L Tdark)) (foreground-lightness (- 1.0 L) bgL)]
          [(and (> bgL Tlight) (> L Tlight)) (foreground-lightness (- 1.0 L) bgL)]
          [(< (abs (- L bgL)) ΔL) (stroke-lightness Tdark Tlight L bgL ΔL)]
          [else #false])))

(define fill-chroma : (-> Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum Nonnegative-Flonum Flonum)
  (lambda [Tdark Tlight C0 H bgL bgC α]
    (* C0
       (cond [(< bgL Tdark)  (+ 0.8 (* 0.4 bgL))]
             [(> bgL Tlight) (- 1.5 (* 0.5 bgL))]
             [else (- 1.2 (* 0.5 bgL))])
       (exp (* -1.0 bgC))
       (+ (* α (cos (~rad (- H 240.0) 'deg)))
          1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define foreground-lightness : (-> Flonum Flonum Flonum)
  (lambda [L bgL]
    (~clamp (+ bgL (* 1/phi (- L bgL))) 0.0 1.0)))
