#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require digimon/constant)
(require digimon/metrics)
(require geofun/digitama/base)

(require "ok.rkt")
(require "misc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Palette-Index->Pen+Brush-Colors (-> Positive-Index (Option FlRGBA) (Pairof FlRGBA FlRGBA)))
(define-type Palette-Brightness-Threshold (U Nonnegative-Flonum (Pairof Nonnegative-Flonum Nonnegative-Flonum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
; T: threshold for determine the dark and light theme
; α: the amplification of chroma, increase it for higher contrast
; k: the steepness of the smoothing function, descrease it for more natural change
;
; high α and high k are good for information visualization, whereas
; low α and low k are good for animation and UI.
(define oklch-palette-create : (-> [#:sigmoid (-> Flonum Flonum)] 
                                   [#:chroma0 Real] [#:hue0 Real] [#:hue-count Real]
                                   [#:dark-range (Pairof Nonnegative-Flonum Nonnegative-Flonum)]
                                   [#:light-range (Pairof Nonnegative-Flonum Nonnegative-Flonum)]
                                   [#:α Nonnegative-Flonum] [#:k Nonnegative-Flonum] [#:T Palette-Brightness-Threshold]
                                   [#:delta-brightness Nonnegative-Flonum]
                                   Palette-Index->Pen+Brush-Colors)
  (lambda [#:sigmoid [sigmoid palette-sigmoid/logistic]
           #:α [α 0.25] #:k [k 20.0] #:T [T 0.20] #:delta-brightness [ΔL 0.15]
           #:dark-range [drk-rng (cons 0.2 0.9)] #:light-range [lgt-rng (cons 0.1 0.8)]
           #:chroma0 [chroma0 +nan.0] #:hue0 [hue0 +nan.0] #:hue-count [N +nan.0]]
    (define color-db : (HashTable Any (Pairof FlRGBA FlRGBA)) (make-weak-hash))
    (define fallback-hue0 : Flonum (* 360.0 1/phi 1/phi))
    (define hue-delta : Flonum (if (and (rational? N) (not (zero? N))) (/ 360.0 (real->double-flonum N)) fallback-hue0))
    (define C0 : Nonnegative-Flonum (real->ok-chroma (if (rational? chroma0) chroma0 2/5)))

    (define-values (Tdark Tlight)
      (cond [(pair? T) (values (car T) (cdr T))]
            [else (values (max 0.0 (- 0.5 T)) (min 1.0 (+ 0.5 T)))]))
    (define interpolate (oklch-palette-sigmoid-interpolator sigmoid #:threshold T #:k k #:dark-range drk-rng #:light-range lgt-rng))

    (define (gen-color [idx : Positive-Index] [bgL : Flonum] [bgC : Flonum]) : (Pairof FlRGBA FlRGBA)
      (define H (real->hue (+ (if (rational? hue0) hue0 fallback-hue0) (* (sub1 idx) hue-delta))))
      (define Lf (interpolate bgL))
      (define Cf (* C0
                    (cond [(< bgL Tdark)  (+ 0.8 (* 0.4 bgL))]
                          [(> bgL Tlight) (- 1.5 (* 0.5 bgL))]
                          [else 1.2])
                    (/ 1.0 (+ 1.0 (* 2.0 bgC)))
                    (+ (* α (cos (degrees->radians (- H 240.0))))
                       1.0)))

      (define Ls
        (cond [(<= bgL Tdark)  (+ Lf ΔL (* 0.25 (log (max 0.0 (- 2.0 bgL)))))]
              [(>= bgL Tlight) (- Lf ΔL (* 0.05 bgL))]
              [(<= Lf 0.5) (+ Lf ΔL (* 0.05 bgL))]
              [else (- Lf ΔL (* 0.05 bgL))]))
      
      (define Cs (* Cf (- 1.0 (* 0.1 (/ Ls Lf)))))
      
      (let-values ([(Rs Gs Bs) (oklch->rgb (~clamp Ls 0.1 0.9) (~clamp Cs 0.05 0.3) H)]
                   [(Rf Gf Bf) (oklch->rgb (~clamp Lf 0.2 0.8) (~clamp Cf 0.05 0.3) H)])
        (cons (rgba Rs Gs Bs 1.0)
              (rgba Rf Gf Bf 1.0))))
    
    (λ [idx bg]
      (define-values (bgL bgC bgH)
        (if (rgba? bg)
            (rgb->oklch (rgba-red bg) (rgba-green bg) (rgba-blue bg))
            (values 1.0 0.0 +nan.0)))
        
      (hash-ref! color-db (cons idx bgL) (λ [] (gen-color idx bgL bgC))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define oklch-palette-sigmoid-interpolator : (->* ((-> Flonum Flonum))
                                                  (#:name (Option Symbol)
                                                   #:threshold Palette-Brightness-Threshold #:k Nonnegative-Flonum
                                                   #:dark-range (Pairof Nonnegative-Flonum Nonnegative-Flonum)
                                                   #:light-range (Pairof Nonnegative-Flonum Nonnegative-Flonum))
                                                  (-> Real Flonum))
  (lambda [#:threshold [T 0.15] #:k [k 20.0] #:dark-range [drk-rng (cons 0.2 0.9)] #:light-range [lgt-rng (cons 0.1 0.8)] #:name [name #false] sigmoid]
    (define-values (Tdrk Tlgt)
      (cond [(pair? T) (values (car T) (cdr T))]
            [else (values (max 0.0 (- 0.5 T)) (min 1.0 (+ 0.5 T)))]))

    (define (weights [L : Real]) : (Values Flonum Flonum Flonum)
      (define Wdrk (sigmoid (* k (- L Tdrk))))
      (define Wlgt (sigmoid (* k (- L Tlgt))))
      (define sum (+ Wdrk Wlgt))

      (if (> sum 1.0)
          (let ([d (/ Wdrk sum)]
                [l (/ Wlgt sum)])
            (values d l (- 1.0 d l)))
          (values Wdrk Wlgt (- 1.0 Wdrk Wlgt))))

    (define (sigmoid-smooth-interpolate [L : Real]) : Flonum
      (define-values (Wdrk Wlgt Wneu) (weights L))
      (define Ldrk (~clamp (+ L 0.4) (car drk-rng) (cdr drk-rng)))
      (define Llgt (~clamp (- L 0.3) (car lgt-rng) (cdr lgt-rng)))
      (define Lneu 0.5)
      
      (+ (* Wdrk Ldrk) (* Wlgt Llgt) (* Wneu Lneu)))
    
    (procedure-rename sigmoid-smooth-interpolate
                      (or name (string->symbol
                                (format "~a-interpolate"
                                  (object-name sigmoid)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define palette-sigmoid/logistic : (-> Flonum Flonum)
  (lambda [x]
    (/ 1.0 (+ 1.0 (exp (- x))))))

(define palette-sigmoid/tanh : (-> Flonum Flonum)
  (lambda [x]
    (* (+ (tanh x) 1.0) 0.5)))

(define palette-sigmoid/algebraic : (-> Flonum Flonum)
  (lambda [x]
    (* 0.5 (+ 1.0 (/ x (magnitude (make-rectangular 1.0 x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define palette-contrast-ratio : (-> Flonum Flonum Flonum)
  (lambda [Lf bgL]
    (/ (+ (max Lf bgL) 0.05)
       (+ (min Lf bgL) 0.05))))
