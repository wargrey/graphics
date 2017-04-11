#lang typed/racket

;;; https://drafts.csswg.org/css-color

(provide (all-defined-out))

(require colorspace)
(require bitmap/color)
(require bitmap/digitama/color)

(require "bitmap.rkt")
(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "../recognizer.rkt")
(require "../racket.rkt")

(define-type CSS-Color-Datum (U Color+sRGB CSS-Color))

(define-css-value css-color #:as CSS-Color ())
(define-css-value hexa #:as HEXA #:=> css-color ([hex : Index] [a : Nonnegative-Flonum]))
(define-css-value rgba #:as RGBA #:=> css-color ([r : Byte] [g : Byte] [b : Byte] [a : Nonnegative-Flonum]))
(define-css-value hsba #:as HSBA #:=> css-color ([>rgb : HSB->RGB] [h : Real] [s : Real] [b : Real] [a : Nonnegative-Flonum]))

(define-css-atomic-filter <css#color> #:-> (U Index HEXA) #:with [[color-value : css:hash?]]
  (define-values (?rgb alpha) (css-hex-color->rgba (css:hash-datum color-value)))
  (cond [(false? ?rgb) (make-exn:css:range color-value)]
        [(fl= alpha 1.0) ?rgb]
        [else (hexa ?rgb alpha)])
  #:where
  [(define (css-hex-color->rgba [hash-color : Keyword]) : (Values (Option Index) Nonnegative-Flonum)
     ;;; https://drafts.csswg.org/css-color/#numeric-rgb
     (define color : String (keyword->string hash-color))
     (define digits : Index (string-length color))
     (define ?hexcolor : (Option Number)
       (case digits
         [(6 8) (string->number color 16)]
         [(3 4) (for/fold ([hexcolor : (Option Nonnegative-Fixnum) 0])
                          ([ch : Char (in-string color)])
                  (define digit : (U Integer Void)
                    (cond [(char-numeric? ch)   (fx- (char->integer ch) #x30)]
                          [(char<=? #\a ch #\f) (fx- (char->integer ch) #x37)]
                          [(char<=? #\A ch #\F) (fx- (char->integer ch) #x57)]))
                  (and hexcolor (byte? digit)
                       (fxior (fxlshift hexcolor 8)
                              (fxior (fxlshift digit 4)
                                     digit))))]
         [else #false]))
     (cond [(or (fx= digits 3) (fx= digits 6)) (values (and (index? ?hexcolor) ?hexcolor) 1.0)]
           [(not (exact-integer? ?hexcolor)) (values #false 1.0)]
           [else (let ([hex-rgb (arithmetic-shift ?hexcolor -8)])
                   (values (and (index? hex-rgb) hex-rgb)
                           (flabs (fl/ (fx->fl (fxand ?hexcolor #xFF)) 255.0))))]))])

(define-css-function-filter <css-color-notation> #:-> CSS-Color
  ;;; https://drafts.csswg.org/css-color/#rgb-functions
  ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
  ;;; https://drafts.csswg.org/css-color/#the-hwb-notation
  [(rgba rgb) #:=> [(rgba [r ? byte?] [g ? byte?] [b ? byte?] [alpha ? nonnegative-flonum?])]
   (make-parser <:rgb:> <:rgb:>)]
  [(hsla hsl) #:=> [(hsba hsl->rgb [h ? real?] [s ? single-flonum?] [l ? single-flonum?] [alpha ? nonnegative-flonum?])]
   (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
  [(hsva hsv) #:=> [(hsba hsv->rgb [h ? real?] [s ? single-flonum?] [v ? single-flonum?] [alpha ? nonnegative-flonum?])]
   (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
  [(hsia hsi) #:=> [(hsba hsi->rgb [h ? real?] [s ? single-flonum?] [i ? single-flonum?] [alpha ? nonnegative-flonum?])]
   (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
  [(hwba hwb) #:=> [(hsba hwb->rgb [h ? real?] [w ? single-flonum?] [b ? single-flonum?] [alpha ? nonnegative-flonum?])]
   (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
  #:where
  [(define-css-disjoint-filter <rgb-byte> #:-> Integer
     (<css:integer> byte?)
     (CSS:<~> (<css:percentage> 0.0f0 <= 1.0f0) (λ [[% : Single-Flonum]] (exact-round (* % 255.0))))
     (CSS:<~> (<css:flonum> 0.0 fl<= 255.0) exact-round))

   (define make-alpha-parser : (-> (-> (CSS:Filter Char)) (CSS-Parser (Listof Any)))
     (lambda [<delimiter>]
       (CSS<$> (CSS<?> [(<delimiter>) (CSS<^> (<css-%flunit>))]) 1.0)))
     
   (define make-parser : (-> (CSS-Parser (Listof Any)) (CSS-Parser (Listof Any)) (CSS-Parser (Listof Any)))
     ;;; https://github.com/w3c/csswg-drafts/issues/266
     (lambda [c1 c2]
       (CSS<&> c1 (CSS<?> [(<css-comma>) (CSS<#> c2 '(2)) (make-alpha-parser <css-comma>)]
                          [else          (CSS<*> c2 '(2)) (make-alpha-parser <css-slash>)]))))

   (define <:rgb:> (CSS<^> (<rgb-byte>)))
   (define <:hue:> (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>) (<css:angle>))))])

(define-css-atomic-filter <racket-colorbase> #:-> (Instance Color%)
  #:with [[color-value : css:string?] [px : Regexp #px"(?i:grey)$"] [to : String "gray"]]
  (define name : String (string-replace (css:string-datum color-value) px to))
  (cond [(send the-color-database find-color name) => values]
        [else (make-exn:css:range color-value)]))

(define-css-racket-value-filter <racket-color> #:with ?color #:as (U String (Instance Color%))
  [(color%? ?color) ?color]
  [(and (string? ?color) (send the-color-database find-color ?color)) ?color])

(define-css-disjoint-filter <css-color> #:-> (U CSS-Color-Datum CSS-Wide-Keyword)
  ;;; https://drafts.csswg.org/css-color/#color-type
  ;;; https://drafts.csswg.org/css-color/#named-colors
  #:with [[hint? : Any #false]]
  (CSS:<~> (<css-keyword> (cons 'currentcolor (cons 'transparent (hash-keys css-named-colors))))
           (λ [[c : Symbol]] (cond [(not (eq? c 'currentcolor)) c]
                                   [(false? hint?) c]
                                   [else css:inherit])))
  (<css#color>)
  (<css-color-notation>)
  (<racket-color>)
  (<racket-colorbase>))
