#lang typed/racket

(provide (all-defined-out) transparent)

(require colorspace)

(require "digitama/digicore.rkt")
(require "digitama/color.rkt")

(struct hexa flcolor ([digits : Index] [alpha : Flonum]) #:transparent)
(define-color-space hsl ([hue : real->hue] [saturation : real->gamut] [luminosity : real->gamut]))
(define-color-space hsv ([hue : real->hue] [saturation : real->gamut] [value : real->gamut]))
(define-color-space hsi ([hue : real->hue] [saturation : real->gamut] [intensity : real->gamut]))
(define-color-space hwb ([hue : real->hue] [white : real->gamut] [black : real->gamut]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rgb : (->* (Real Real Real) (Real) FlRGBA)
  (lambda [red green blue [alpha 1.0]]
    (rgba (real->gamut red)
          (real->gamut green)
          (real->gamut blue)
          (real->double-flonum alpha))))

(define rgb* : (->* (Color) (Real) FlRGBA)
  (lambda [src [alpha 1.0]]
    (define flalpha : Flonum (real->double-flonum alpha))
    (cond [(symbol? src) (or (named-rgba src flalpha rgb*) (rgb* fallback-color flalpha))]
          [(exact-integer? src) (let-values ([(r g b) (hex->rgb-gamuts src)]) (rgba r g b flalpha))]
          [(hexa? src) (let-values ([(r g b) (hex->rgb-gamuts (hexa-digits src))]) (rgba r g b (fl* (hexa-alpha src) flalpha)))]
          [(rgba? src) (if (= flalpha 1.0) src (rgba (rgba-red src) (rgba-green src) (rgba-blue src) (fl* (rgba-alpha src) flalpha)))]
          [(hsla? src) ($ hsl->rgb (hsla-hue src) (hsla-saturation src) (hsla-luminosity src) (hsla-alpha src) flalpha)]
          [(hsva? src) ($ hsv->rgb (hsva-hue src) (hsva-saturation src) (hsva-value src) (hsva-alpha src) flalpha)]
          [(hsia? src) ($ hsi->rgb (hsia-hue src) (hsia-saturation src) (hsia-intensity src) (hsia-alpha src) flalpha)]
          [(hwba? src) ($ hwb->rgb (hwba-hue src) (hwba-white src) (hwba-black src) (hwba-alpha src) flalpha)]
          [else (rgb* fallback-color flalpha)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define list-color-names : (-> (Listof Symbol))
  (lambda []
    (hash-keys css-named-colors)))

(define in-color-names : (-> (Sequenceof Symbol))
  (lambda []
    (in-hash-keys css-named-colors)))

(define named-color? : (-> Symbol Boolean)
  (lambda [name]
    (and (named-rgba name +nan.0 (λ _ transparent))
         #true)))

(define flcolor->byte-list : (-> Color (List Byte Byte Byte))
  (lambda [src]
    (define flrgba : FlRGBA (rgb* src))
    (list (gamut->byte (rgba-red flrgba))
          (gamut->byte (rgba-green flrgba))
          (gamut->byte (rgba-blue flrgba)))))
