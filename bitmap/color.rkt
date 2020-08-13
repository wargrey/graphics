#lang typed/racket/base

(provide (except-out (all-defined-out) define-color-model))
(provide (struct-out FlRGBA) (struct-out FlColor) Color rgba color?)
(provide transparent hilite black)

(require colorspace)

(require "digitama/base.rkt")
(require "digitama/color.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-color-model stx)
  (syntax-case stx [:]
    [(_ clr ([com : real->flonum] ...) #:* rgb->clr)
     (with-syntax ([clra (format-id #'clr "~aa" (syntax-e #'clr))]
                   [FlCLRA (format-id #'clr "Fl~aA" (string-upcase (symbol->string (syntax-e #'clr))))]
                   [clra? (format-id #'clr "~aa?" (syntax-e #'clr))]
                   [clr* (format-id #'clr "~a*" (syntax-e #'clr))]
                   [clr-alpha (format-id #'clr "~aa-alpha" (syntax-e #'clr))]
                   [(clr-com ...) (for/list ([<c> (in-list (syntax->list #'(com ...)))])
                                    (format-id <c> "~aa-~a" (syntax-e #'clr) (syntax-e <c>)))])
       #'(begin (struct clra flcolor ([com : Flonum] ... [alpha : Flonum])
                  #:transparent #:type-name FlCLRA)
                (define (clr [com : Real] ... [alpha : Real 1.0]) : clra
                  (clra (real->flonum com) ... (real->alpha alpha)))
                (define (clr* [src : Color] [alpha : Real 1.0]) : clra
                  (cond [(clra? src) (if (= alpha 1.0) src (clra (clr-com src) ... (* (clr-alpha src) (real->alpha alpha))))]
                        [else (let ([flrgba (rgb* src alpha)])
                                (define-values (com ...) (rgb->clr (rgba-red flrgba) (rgba-green flrgba) (rgba-blue flrgba)))
                                (clra com ... (rgba-alpha flrgba)))]))))]))

(struct hexa flcolor ([digits : Index] [alpha : Flonum]) #:transparent #:type-name Hexa)
(struct xterma flcolor ([index : Byte] [alpha : Flonum]) #:transparent #:type-name Xterma)

(define-color-model hsl ([hue : real->hue] [saturation : real->gamut] [luminosity : real->gamut]) #:* rgb->hsl)
(define-color-model hsv ([hue : real->hue] [saturation : real->gamut] [value : real->gamut])      #:* rgb->hsv)
(define-color-model hsi ([hue : real->hue] [saturation : real->gamut] [intensity : real->gamut])  #:* rgb->hsi)
(define-color-model hwb ([hue : real->hue] [white : real->gamut] [black : real->gamut])           #:* rgb->hwb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rgb : (->* (Real Real Real) (Real) FlRGBA)
  (lambda [red green blue [alpha 1.0]]
    (rgba (real->gamut red)
          (real->gamut green)
          (real->gamut blue)
          (real->alpha alpha))))

(define rgb* : (->* (Color) (Real) FlRGBA)
  (lambda [src [alpha 1.0]]
    (define flalpha : Flonum (real->alpha alpha))
    (cond [(symbol? src) (or (named-rgba src flalpha rgb*) (rgb* fallback-color flalpha))]
          [(exact-integer? src) (let-values ([(r g b) (hex->rgb-gamuts src)]) (rgba r g b flalpha))]
          [(hexa? src) (let-values ([(r g b) (hex->rgb-gamuts (hexa-digits src))]) (rgba r g b (* (hexa-alpha src) flalpha)))]
          [(rgba? src) (if (= flalpha 1.0) src (rgba (rgba-red src) (rgba-green src) (rgba-blue src) (* (rgba-alpha src) flalpha)))]
          [(hsla? src) ($ hsl->rgb (hsla-hue src) (hsla-saturation src) (hsla-luminosity src) (hsla-alpha src) flalpha)]
          [(hsva? src) ($ hsv->rgb (hsva-hue src) (hsva-saturation src) (hsva-value src) (hsva-alpha src) flalpha)]
          [(hsia? src) ($ hsi->rgb (hsia-hue src) (hsia-saturation src) (hsia-intensity src) (hsia-alpha src) flalpha)]
          [(hwba? src) ($ hwb->rgb (hwba-hue src) (hwba-white src) (hwba-black src) (hwba-alpha src) flalpha)]
          [(xterma? src) (xterm256-rgba (xterma-index src) (* (xterma-alpha src) flalpha) rgb*)]
          [else (rgb* fallback-color flalpha)])))

(define xterm : (->* (Byte) (Real) Xterma)
  (lambda [idx [alpha 1.0]]
    (xterma idx (real->alpha alpha))))

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

(define flcolor->hex : (-> Color Index)
  (lambda [src]
    (define flrgba : FlRGBA (rgb* src))
    (rgb-gamuts->hex (rgba-red flrgba)
                     (rgba-green flrgba)
                     (rgba-blue flrgba))))

(define flcolor->byte-list : (-> Color (List Byte Byte Byte))
  (lambda [src]
    (define flrgba : FlRGBA (rgb* src))
    (list (gamut->byte (rgba-red flrgba))
          (gamut->byte (rgba-green flrgba))
          (gamut->byte (rgba-blue flrgba)))))

(define flcolor->uint16-list : (-> Color (List Index Index Index))
  (lambda [src]
    (define flrgba : FlRGBA (rgb* src))
    (list (gamut->uint16 (rgba-red flrgba))
          (gamut->uint16 (rgba-green flrgba))
          (gamut->uint16 (rgba-blue flrgba)))))
