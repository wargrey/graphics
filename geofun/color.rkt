#lang typed/racket/base

(provide (except-out (all-defined-out) define-color-model))
(provide (struct-out FlRGBA) (struct-out FlColor) Color rgba color?)
(provide transparent hilite black)

(require colorspace)

(require racket/format)

(require file/convertible)

(require "digitama/base.rkt")
(require "digitama/color.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/symbol))
(require (for-syntax racket/sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-color-model stx)
  (syntax-case stx [:]
    [(_ clr ([com : real->flonum] ...) #:* rgb->clr clr->rgb)
     (with-syntax* ([clra (format-id #'clr "~aa" (syntax-e #'clr))]
                    [FlCLRA (format-id #'clr "Fl~aA" (string-upcase (symbol->immutable-string (syntax-e #'clr))))]
                    [clra? (format-id #'clr "~aa?" (syntax-e #'clr))]
                    [clr* (format-id #'clr "~a*" (syntax-e #'clr))]
                    [clr-alpha (format-id #'clr "~aa-alpha" (syntax-e #'clr))]
                    [clr-replace (format-id #'clr "~a-transform-replace" (syntax-e #'clr))]
                    [clr-modulate (format-id #'clr "~a-transform-modulate" (syntax-e #'clr))]
                    [clr-offset (format-id #'clr "~a-transform-offset" (syntax-e #'clr))]
                    [([kw:com a:com clr-com] ...) (for/list ([<c> (in-syntax #'(com ...))])
                                                    (list (datum->syntax <c> (string->keyword (symbol->immutable-string (syntax-e <c>))))
                                                          (format-id <c> "a:~a" (syntax-e <c>))
                                                          (format-id <c> "~aa-~a" (syntax-e #'clr) (syntax-e <c>))))]
                    [(kw-args ...) (for/fold ([args null])
                                             ([<kw:com> (in-syntax #'(kw:com ...))]
                                              [<a:com> (in-syntax #'([a:com #false] ...))])
                                     (cons <kw:com> (cons <a:com> args)))])
       (syntax/loc stx
         (begin (struct clra flcolor ([com : Flonum] ... [alpha : Flonum])
                  #:transparent #:type-name FlCLRA
                  #:property prop:convertible
                  (λ [[self : FlCLRA] [mime : Symbol] [fallback : Any]]
                    (case mime
                      [(rgb-byte-list rgb-uint8-list)
                       (let-values ([(r g b) (clr->rgb (clr-com self) ...)])
                         (list (gamut->byte r)
                               (gamut->byte g)
                               (gamut->byte b)))]
                      [(rgb-uint16-list rgb-word-list)
                       (let-values ([(r g b) (clr->rgb (clr-com self) ...)])
                         (list (gamut->uint16 r)
                               (gamut->uint16 g)
                               (gamut->uint16 b)))]
                      [(rgb-uint24 RRGGBB)
                       (let-values ([(r g b) (clr->rgb (clr-com self) ...)])
                         (rgb-gamuts->hex r g b))]
                      [else fallback])))

                (define (clr [com : Real] ... [alpha : Real 1.0]) : FlCLRA
                  (clra (real->flonum com) ... (real->alpha alpha)))

                (define (clr* [src : Color] [alpha : Real 1.0]) : FlCLRA
                  (cond [(clra? src) (if (= alpha 1.0) src (clra (clr-com src) ... (* (clr-alpha src) (real->alpha alpha))))]
                        [else (let ([flrgba (rgb* src alpha)])
                                (define-values (com ...) (rgb->clr (rgba-red flrgba) (rgba-green flrgba) (rgba-blue flrgba)))
                                (clra com ... (rgba-alpha flrgba)))]))

                (define clr-replace : (-> Color [kw:com (Option Real)] ... [#:alpha (Option Real)] FlCLRA)
                  (lambda [src kw-args ... #:alpha [alpha #false]]
                    (define c (clr* src))
                    (let ([com (clr-com c)] ...
                          [a (clr-alpha c)])
                      (clr (if (not a:com) com a:com) ...
                           (if (not alpha) a alpha)))))

                (define clr-modulate : (-> Color [kw:com (Option Nonnegative-Real)] ... [#:alpha (Option Nonnegative-Real)] FlCLRA)
                  (lambda [src kw-args ... #:alpha [amod #false]]
                    (define c (clr* src))
                    (let ([com (clr-com c)] ...
                          [a (clr-alpha c)])
                      (clr (if (not a:com) com (* com a:com)) ...
                           (if (not amod) a (* a amod))))))

                (define clr-offset : (-> Color [kw:com (Option Real)] ... [#:alpha (Option Real)] FlCLRA)
                  (lambda [src kw-args ... #:alpha [aoff #false]]
                    (define c (clr* src))
                    (let ([com (clr-com c)] ...
                          [a (clr-alpha c)])
                      (clr (if (not a:com) com (+ com a:com)) ...
                           (if (not aoff) a (+ a aoff)))))))))]))

(struct hexa flcolor ([digits : Index] [alpha : Flonum])
  #:type-name Hexa
  #:transparent

  #:property prop:custom-write
  (λ [[self : Hexa] [/dev/stdout : Output-Port] [mode : (U Zero One Boolean)]]
    (fprintf /dev/stdout "#(struct:~a #x" (object-name self))
    (write-string (string-upcase (~r (hexa-digits self) #:base 16 #:min-width 6 #:pad-string "0")) /dev/stdout)
    (write-char #\space /dev/stdout)
    (write (hexa-alpha self) /dev/stdout)
    (write-char #\) /dev/stdout))

  #:property prop:convertible
  (λ [[self : Hexa] [mime : Symbol] [fallback : Any]]
    (case mime
      [(rgb-byte-list rgb-uint8-list) (let-values ([(r g b) (hex->rgb-bytes (hexa-digits self))]) (list r g b))]
      [(rgb-uint16-list rgb-word-list) (flcolor->uint16-list self)]
      [(rgb-uint24 RRGGBB) (hexa-digits self)]
      [else fallback])))

(struct xterma flcolor ([index : Byte] [alpha : Flonum])
  #:transparent #:type-name Xterma

  #:property prop:convertible
  (λ [[self : Xterma] [mime : Symbol] [fallback : Any]]
    (case mime
      [(rgb-byte-list rgb-uint8-list) (flcolor->byte-list self)]
      [(rgb-uint16-list rgb-word-list) (flcolor->uint16-list self)]
      [(rgb-uint24 RRGGBB) (flcolor->hex self)]
      [else fallback])))

(define-color-model hsl ([hue : real->hue] [saturation : real->gamut] [luminosity : real->gamut]) #:* rgb->hsl hsl->rgb)
(define-color-model hsv ([hue : real->hue] [saturation : real->gamut] [value : real->gamut])      #:* rgb->hsv hsv->rgb)
(define-color-model hsi ([hue : real->hue] [saturation : real->gamut] [intensity : real->gamut])  #:* rgb->hsi hsi->rgb)
(define-color-model hwb ([hue : real->hue] [white : real->gamut] [black : real->gamut])           #:* rgb->hwb hwb->rgb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rgb : (->* (Real Real Real) (Real) FlRGBA)
  (lambda [red green blue [alpha 1.0]]
    (rgba (real->gamut red)
          (real->gamut green)
          (real->gamut blue)
          (real->alpha alpha))))

(define rgb* : (case-> [Color -> FlRGBA]
                       [Color Real -> FlRGBA]
                       [Real Real Real -> FlRGBA]
                       [Real Real Real Real -> FlRGBA])
  (case-lambda
    [(src) (rgb* src 1.0)]
    [(r g b) (rgb* (rgb r g b) 1.0)]
    [(r g b alpha) (rgb* (rgb r g b) alpha)]
    [(src alpha)
     (let ([flalpha (real->alpha alpha)])
       (cond [(symbol? src) (or (named-rgba src flalpha rgb*) (rgb* fallback-color flalpha))]
             [(exact-integer? src) (let-values ([(r g b) (hex->rgb-gamuts src)]) (rgba r g b flalpha))]
             [(hexa? src) (let-values ([(r g b) (hex->rgb-gamuts (hexa-digits src))]) (rgba r g b (* (hexa-alpha src) flalpha)))]
             [(rgba? src) (if (= flalpha 1.0) src (rgba (rgba-red src) (rgba-green src) (rgba-blue src) (* (rgba-alpha src) flalpha)))]
             [(hsla? src) ($# hsl->rgb (hsla-hue src) (hsla-saturation src) (hsla-luminosity src) (hsla-alpha src) flalpha)]
             [(hsva? src) ($# hsv->rgb (hsva-hue src) (hsva-saturation src) (hsva-value src) (hsva-alpha src) flalpha)]
             [(hsia? src) ($# hsi->rgb (hsia-hue src) (hsia-saturation src) (hsia-intensity src) (hsia-alpha src) flalpha)]
             [(hwba? src) ($# hwb->rgb (hwba-hue src) (hwba-white src) (hwba-black src) (hwba-alpha src) flalpha)]
             [(xterma? src) (xterm256-rgba (xterma-index src) (* (xterma-alpha src) flalpha) rgb*)]
             [(keyword? src) (or (digits-rgba src flalpha) (rgb* fallback-color flalpha))]
             [(real? src) ($# hsv->rgb (real->hue src) 1.0 1.0 1.0 flalpha)]
             [else (rgb* fallback-color flalpha)]))]))

(define xterm : (->* (Byte) (Real) Xterma)
  (lambda [idx [alpha 1.0]]
    (xterma idx (real->alpha alpha))))

(define hexa* : (case-> [Color -> Hexa]
                        [Color Real -> Hexa]
                        [Real Real Real -> Hexa]
                        [Real Real Real Real -> Hexa])
  (case-lambda
    [(src) (hexa (flcolor->hex src) 1.0)]
    [(src alpha) (hexa (flcolor->hex src) (real->alpha alpha))]
    [(r g b) (hexa (flcolor->hex (rgb r g b)) 1.0)]
    [(r g b alpha) (hexa (flcolor->hex (rgb r g b)) 1.0)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rgb-transform-replace : (-> Color [#:red (Option Real)] [#:green (Option Real)] [#:blue (Option Real)] [#:alpha (Option Real)] FlRGBA)
  (lambda [src #:red [red #false] #:green [green #false] #:blue [blue #false] #:alpha [alpha #false]]
    (define c (rgb* src))
    (define r (rgba-red c))
    (define g (rgba-green c))
    (define b (rgba-blue c))
    (define a (rgba-alpha c))

    (rgb (if (not red) r red)
         (if (not green) g green)
         (if (not blue) b blue)
         (if (not alpha) a alpha))))

(define rgb-transform-modulate : (-> Color [#:red Nonnegative-Real] [#:green Nonnegative-Real] [#:blue Nonnegative-Real] [#:alpha Nonnegative-Real] FlRGBA)
  (lambda [src #:red [rmod #false] #:green [gmod #false] #:blue [bmod #false] #:alpha [amod #false]]
    (define c (rgb* src))
    (define r (rgba-red c))
    (define g (rgba-green c))
    (define b (rgba-blue c))
    (define a (rgba-alpha c))

    (rgb (if (not rmod) r (* r rmod))
         (if (not gmod) g (* g gmod))
         (if (not bmod) b (* b bmod))
         (if (not amod) a (* a amod)))))

(define rgb-transform-offset : (-> Color [#:red Real] [#:green Real] [#:blue Real] [#:alpha Real] FlRGBA)
  (lambda [src #:red [roff #false] #:green [goff #false] #:blue [boff #false] #:alpha [aoff #false]]
    (define c (rgb* src))
    (define r (rgba-red c))
    (define g (rgba-green c))
    (define b (rgba-blue c))
    (define a (rgba-alpha c))

    (rgb (if (not roff) r (+ r roff))
         (if (not goff) g (+ g goff))
         (if (not boff) b (+ b boff))
         (if (not aoff) a (+ a aoff)))))

(define rgb-transform-scale : (-> Color Nonnegative-Real FlRGBA)
  (lambda [src s]
    (rgb-transform-modulate src #:red s #:green s #:blue s)))

(define rgb-transform-shade : (-> Color Nonnegative-Real FlRGBA)
  (lambda [src s0]
    (define s : Nonnegative-Flonum (real->gamut s0))
    (define b : Nonnegative-Flonum (max (- 1.0 s) 0.0))
    
    (rgb* (hwb-transform-modulate src #:hue s #:white s #:black b))))

(define rgb-transform-tint : (-> Color Nonnegative-Real FlRGBA)
  (lambda [src s0]
    (define s : Nonnegative-Flonum (real->gamut s0))
    (define w : Nonnegative-Flonum (max (- 1.0 s) 0.0))
    
    (rgb* (hwb-transform-modulate src #:hue s #:white w #:black s))))

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

; Scratch uses this color representation
(define flcolor->hsb-byte100-list : (-> Color (List Byte Byte Byte))
  (lambda [src]
    (define flhsva : FlHSVA (hsv* src))
    (list (gamut->byte100 (/ (hsva-hue flhsva) 360.0))
          (gamut->byte100 (hsva-saturation flhsva))
          (gamut->byte100 (hsva-value flhsva)))))
