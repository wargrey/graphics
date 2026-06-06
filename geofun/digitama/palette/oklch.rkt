#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require colorspace/digitama/harmony/base)
(require colorspace/digitama/harmony/plot)
(require colorspace/digitama/interpolator)

(require colorspace/ok)
(require colorspace/misc)

(require "base.rkt")
(require "misc.rkt")
(require "../../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
; T: threshold for determine the dark and light theme
; α: the amplification of chroma, increase it for higher contrast
; k: the steepness of the smoothing function, descrease it for more natural change
;
; high α and high k are good for information visualization, whereas
; low α and low k are good for animation and UI.
(define oklch-plot-palette-create
  (lambda [#:sigmoid [sigmoid : (-> Flonum Flonum) ~logistic]
           #:α [α : Nonnegative-Flonum 0.20] #:k [k : Nonnegative-Flonum 20.0]
           #:T [T : Brightness-Threshold 0.20] #:delta-brightness [ΔL : Nonnegative-Flonum 0.15]
           #:dark-range [drk-rng : (Pairof Nonnegative-Flonum Nonnegative-Flonum) (cons 0.2 0.9)]
           #:light-range [lgt-rng : (Pairof Nonnegative-Flonum Nonnegative-Flonum) (cons 0.1 0.8)]
           #:chroma0 [chroma0 : Real +nan.0] #:hue0 [hue0 : Real +nan.0] #:hue-count [N : Real +nan.0]] : Palette-Index->Colors
    (define color-db : (HashTable Any (Pairof FlRGBA FlRGBA)) (make-weak-hash))
    (define hue-delta : Flonum (if (and (rational? N) (not (zero? N))) (/ 360.0 (real->double-flonum N)) (* 360.0 1/phi 1/phi)))
    (define C0 : Nonnegative-Flonum (real->ok-chroma (if (rational? chroma0) chroma0 40/100)))

    (define-values (Tdark Tlight) (brightness-threshold T))
    (define interpolate : (-> Flonum Flonum)
      (sigmoid-interpolator #:threshold T #:k k #:dark-range drk-rng #:light-range lgt-rng
                            sigmoid))

    (define (index->hue [idx : Natural] [bgH : Flonum]) : Nonnegative-Flonum
      (real->hue (+ (cond [(rational? hue0) hue0]
                          [(rational? bgH) (+ bgH 145 #| [ 135, 145 ] |#)]
                          [else hue-delta])
                    (* idx hue-delta))))
  
    (define (gen-color [H : Nonnegative-Flonum] [bgL : Flonum] [bgC : Flonum]) : (Pairof FlRGBA FlRGBA)
      (define Lf (interpolate bgL))
      (define Ls (stroke-lightness Tdark Tlight Lf bgL ΔL))
      (define Cf (fill-chroma Tdark Tlight C0 H bgL bgC α))
      (define Cs (* Cf 0.9))
      (define-values (Rs Gs Bs) (oklch->rgb (~clamp Ls 0.1 0.9) (~clamp Cs 0.05 0.35) H))
      (define-values (Rf Gf Bf) (oklch->rgb (~clamp Lf 0.2 0.8) (~clamp Cf 0.05 0.35) H))
      
      (cons (rgba Rs Gs Bs 1.0)
            (rgba Rf Gf Bf 1.0)))
    
    (λ [idx bg]
      (define-values (bgL bgC bgH) (oklch-background-extract bg))
      (hash-ref! color-db (list idx bgL bgC bgH)
                 (λ [] (gen-color (index->hue idx bgH) bgL bgC))))))

(define oklch-palette-adapter-create
  (lambda [#:T [T : Brightness-Threshold 0.20] #:delta-brightness [ΔL : Nonnegative-Flonum 0.15]] : Palette-Color-Adapter
    (define-values (Tdark Tlight) (brightness-threshold T))
    
    (define (adjust-color [src : FlRGBA] [bgL : Flonum]) : FlRGBA
      (define-values (L a b) (rgb->oklab (rgba-red src) (rgba-green src) (rgba-blue src)))
      (define adjusted-L (contrast-lightness Tdark Tlight L bgL ΔL))

      (if (and adjusted-L)
          (let-values ([(R G B) (oklab->rgb (~clamp adjusted-L 0.1 0.9) a b)])
            (rgba R G B (rgba-alpha src)))
          src))
    
    (λ [c bg]
      (define-values (bgL _C _H) (oklch-background-extract bg))
      (adjust-color (rgb* c) bgL))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-plot-oklch-palette : Palette-Index->Colors (oklch-plot-palette-create))
(define the-plot-oklch-adapter : Palette-Color-Adapter (oklch-palette-adapter-create))
