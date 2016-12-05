#lang at-exp typed/racket

;;; https://drafts.csswg.org/css-color

(provide (all-defined-out))

(require "bitmap.rkt")
(require "digicore.rkt")
(require "../recognizer.rkt")
(require "../racket.rkt")
(require "../../colorspace/main.rkt")

(define-type CSS-Color-Datum (U Color+sRGB CSS-Color))
(define-predicate css-basic-color-datum? (U Index Symbol String))
(define the-color-pool : (HashTable Fixnum Color) (make-hasheq))

(define-css-value css-color #:as CSS-Color ())
(define-css-value hexa #:as HEXA #:=> css-color ([hex : Index] [a : Nonnegative-Flonum]))
(define-css-value rgba #:as RGBA #:=> css-color ([r : Byte] [g : Byte] [b : Byte] [a : Nonnegative-Flonum]))
(define-css-value hsba #:as HSBA #:=> css-color ([>rgb : HSB->RGB] [h : Real] [s : Real] [b : Real] [a : Nonnegative-Flonum]))
  
(define css-named-colors : (HashTable Symbol Index)
  #hasheq((black . 0) (gold . #xFFD700) (palegoldenrod . #xEEE8AA) (hotpink . #xFF69B4) (darksalmon . #xE9967A) (yellow . #xFFFF00)
                      (moccasin . #xFFE4B5) (white . #xFFFFFF) (plum . #xDDA0DD) (teal . #x008080) (whitesmoke . #xF5F5F5)
                      (lightsalmon . #xFFA07A) (aquamarine . #x7FFFD4) (lavenderblush . #xFFF0F5) (palevioletred . #xDB7093)
                      (olivedrab . #x6B8E23) (dimgrey . #x696969) (navajowhite . #xFFDEAD) (darkblue . #x00008B) (coral . #xFF7F50)
                      (indigo . #x4B0082) (lightcyan . #xE0FFFF) (limegreen . #x32CD32) (oldlace . #xFDF5E6) (grey . #x808080)
                      (darkslateblue . #x483D8B) (sandybrown . #xF4A460) (mediumblue . #x0000CD) (darkolivegreen . #x556B2F)
                      (sienna . #xA0522D) (springgreen . #x00FF7F) (dimgray . #x696969) (royalblue . #x4169E1) (ivory . #xFFFFF0)
                      (rebeccapurple . #x663399) (crimson . #xDC143C) (goldenrod . #xDAA520) (gray . #x808080) (purple . #x800080)
                      (antiquewhite . #xFAEBD7) (cyan . #x00FFFF) (aliceblue . #xF0F8FF) (darkviolet . #x9400D3) (orchid . #xDA70D6)
                      (palegreen . #x98FB98) (green . #x008000) (peachpuff . #xFFDAB9) (snow . #xFFFAFA) (mediumseagreen . #x3CB371)
                      (paleturquoise . #xAFEEEE) (lightslategray . #x778899) (lightcoral . #xF08080) (ghostwhite . #xF8F8FF)
                      (azure . #xF0FFFF) (seashell . #xFFF5EE) (darkcyan . #x008B8B) (darkorchid . #x9932CC) (burlywood . #xDEB887)
                      (lightslategrey . #x778899) (thistle . #xD8BFD8) (bisque . #xFFE4C4) (darkred . #x8B0000) (darkgrey . #xA9A9A9)
                      (dodgerblue . #x1E90FF) (lavender . #xE6E6FA) (deeppink . #xFF1493) (cornflowerblue . #x6495ED) (peru . #xCD853F)
                      (orangered . #xFF4500) (darkgray . #xA9A9A9) (lightseagreen . #x20B2AA) (tomato . #xFF6347) (darkgreen . #x006400)
                      (blueviolet . #x8A2BE2) (forestgreen . #x228B22) (mediumvioletred . #xC71585) (lightyellow . #xFFFFE0)
                      (lightgray . #xD3D3D3) (mediumorchid . #xBA55D3) (darkturquoise . #x00CED1) (papayawhip . #xFFEFD5) 
                      (yellowgreen . #x9ACD32) (lawngreen . #x7CFC00) (firebrick . #xB22222) (rosybrown . #xBC8F8F) (navy . #x000080)
                      (mediumpurple . #x9370DB) (skyblue . #x87CEEB) (lightgreen . #x90EE90) (lemonchiffon . #xFFFACD) (tan . #xD2B48C)
                      (honeydew . #xF0FFF0) (seagreen . #x2E8B57) (darkseagreen . #x8FBC8F) (darkmagenta . #x8B008B) (pink . #xFFC0CB)
                      (blanchedalmond . #xFFEBCD) (darkslategrey . #x2F4F4F) (maroon . #x800000) (darkgoldenrod . #xB8860B)
                      (chocolate . #xD2691E) (mediumaquamarine . #x66CDAA) (darkkhaki . #xBDB76B) (indianred . #xCD5C5C)
                      (floralwhite . #xFFFAF0) (darkslategray . #x2F4F4F) (mediumslateblue . #x7B68EE) (chartreuse . #x7FFF00)
                      (deepskyblue . #x00BFFF) (blue . #x0000FF) (lime . #x00FF00) (darkorange . #xFF8C00) (red . #xFF0000)
                      (violet . #xEE82EE) (mintcream . #xF5FFFA) (beige . #xF5F5DC) (cornsilk . #xFFF8DC) (turquoise . #x40E0D0)
                      (brown . #xA52A2A) (magenta . #xFF00FF) (lightgoldenrodyellow . #xFAFAD2) (saddlebrown . #x8B4513)
                      (slategrey . #x708090) (lightblue . #xADD8E6) (steelblue . #x4682B4) (mediumturquoise . #x48D1CC)
                      (mistyrose . #xFFE4E1) (lightgrey . #xD3D3D3) (lightpink . #xFFB6C1) (wheat . #xF5DEB3) (linen . #xFAF0E6)
                      (powderblue . #xB0E0E6) (aqua . #x00FFFF) (khaki . #xF0E68C) (slategray . #x708090) (greenyellow . #xADFF2F)
                      (cadetblue . #x5F9EA0) (slateblue . #x6A5ACD) (olive . #x808000) (orange . #xFFA500) (lightsteelblue . #xB0C4DE)
                      (lightskyblue . #x87CEFA) (gainsboro . #xDCDCDC) (fuchsia . #xFF00FF) (mediumspringgreen . #x00FA9A)
                      (midnightblue . #x191970) (salmon . #xFA8072) (silver . #xC0C0C0)))

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
  [(define-css-disjoined-filter <rgb-byte> #:-> Integer
     (<css:integer> byte?)
     (CSS:<~> (<css:percentage> 0.0f0 <= 1.0f0) (λ [[% : Single-Flonum]] (exact-round (* % 255.0))))
     (CSS:<~> (<css:flonum> 0.0 fl<= 255.0) exact-round))

   (define make-alpha-parser : (-> (-> (CSS:Filter Char)) (CSS-Parser (Listof CSS-Datum)))
     (lambda [<delimiter>]
       (CSS<$> (CSS<?> [(<delimiter>) (CSS<^> (<css-%flunit>))]) 1.0)))
     
   (define make-parser : (-> (CSS-Parser (Listof CSS-Datum)) (CSS-Parser (Listof CSS-Datum)) (CSS-Parser (Listof CSS-Datum)))
     ;;; https://github.com/w3c/csswg-drafts/issues/266
     (lambda [c1 c2]
       (CSS<&> c1 (CSS<?> [(<css-comma>) (CSS<#> c2 '(2)) (make-alpha-parser <css-comma>)]
                          [else          (CSS<*> c2 '(2)) (make-alpha-parser <css-slash>)]))))

   (define <:rgb:> (CSS<^> (<rgb-byte>)))
   (define <:hue:> (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>) (<css:angle>))))])

(define-css-atomic-filter <racket-colorbase> #:-> Color
  #:with [[color-value : css:string?] [px : Regexp #px"(?i:grey)$"] [to : String "gray"]]
  (define name : String (string-replace (css:string-datum color-value) px to))
  (cond [(send the-color-database find-color name) => values]
        [else (make-exn:css:range color-value)]))
  
(define-css-racket-value-filter <racket-color> #:with ?color #:as (U String Symbol Index Color)
  [(color%? ?color) ?color]
  [(index? ?color) ?color]
  [(and (symbol? ?color)
        (or (and (hash-has-key? css-named-colors ?color) ?color)
            (let ([color (string->symbol (string-downcase (symbol->string ?color)))])
              (and (hash-has-key? css-named-colors color) color)))) => values]
  [(and (string? ?color) (send the-color-database find-color ?color)) ?color])

(define-css-disjoined-filter <css-color> #:-> (U CSS-Color-Datum CSS-Wide-Keyword)
  ;;; https://drafts.csswg.org/css-color/#color-type
  ;;; https://drafts.csswg.org/css-color/#named-colors
  #:with [[hint? : Any #false]]
  (CSS:<~> (<css-keyword> (cons 'currentcolor (cons 'transparent (hash-keys css-named-colors))))
           (λ [[c : Symbol]] (cond [(not (eq? c 'currentcolor)) c]
                                   [(false? hint?) c]
                                   [else css:inherit])))
  (<css#color>)
  (<css-color-notation>)
  (<racket-colorbase>)
  (<racket-color>))
