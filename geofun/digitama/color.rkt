#lang typed/racket/base

(provide (all-defined-out))

(require "base.rkt")

(require colorspace)

(require digimon/number)
(require digimon/symbol)

(require digimon/digitama/unsafe/release/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rgban rgba ([name : Symbol]) #:transparent #:type-name RGBAN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-make-currentcolor : (Parameterof (-> Color)) (make-parameter (λ [] #x000000)))
(define fallback-color : Color ((default-make-currentcolor)))
(define transparent : FlRGBA (rgba 0.0 0.0 0.0 0.0))
(define hilite : FlRGBA (rgba 0.0 0.0 0.0 0.3))
(define black : FlRGBA (rgba 0.0 0.0 0.0 1.0))
(define white : FlRGBA (rgba 1.0 1.0 1.0 1.0))
(define grey : FlRGBA (rgba 0.5 0.5 0.5 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define $# : (-> (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)) Flonum Flonum Flonum Flonum Flonum FlRGBA)
  (lambda [->rgb h s b a alpha]
    (define-values (flr flg flb) (->rgb h s b))
    (rgba flr flg flb (* alpha a))))

(define named-rgba : (->* (Symbol Flonum (-> Color Real FlRGBA)) (Boolean) (Option FlRGBA))
  (lambda [name flalpha rgb* [downcased? #false]]
    (cond [(hash-has-key? css-named-colors name) (rgba-attach-name (rgb* (hash-ref css-named-colors name) flalpha) name)]
          [(not downcased?) (named-rgba (symbol-downcase name) flalpha rgb* #true)]
          [(eq? name 'transparent) transparent]
          [(eq? name 'currentcolor) (rgb* ((default-make-currentcolor)) flalpha)]
          [else #false])))

(define xterm256-rgba : (-> Byte Flonum (-> Color Real FlRGBA) FlRGBA)
  (lambda [sgr flalpha rgb*]
    (cond [(>= sgr #xE8) ; 24 colors of grayscale
           (define grey : Flonum (/ (real->double-flonum (+ (* (- sgr #xE8) 10) 8)) 255.0))
           (rgba grey grey grey flalpha)]
          [(>= sgr #x10) ; 216 colors of a 6x6x6 cubic RGB
           (define-values (rg b) (quotient/remainder (- sgr #x10) 6))
           (define-values (r g) (quotient/remainder rg 6))
           (rgba (/ (vector-ref xterm-color-tuples r) 255.0)
                 (/ (vector-ref xterm-color-tuples g) 255.0)
                 (/ (vector-ref xterm-color-tuples b) 255.0)
                 flalpha)]
          [else (or (named-rgba (vector-ref xterm-system-colors sgr) flalpha rgb* #true)
                    (rgb* fallback-color flalpha))])))

(define digits-rgba : (-> Keyword Flonum (Option FlRGBA))
  (lambda [src flalpha]
    (define-values (hex a) (css-#hex-color->rgba src))

    (and (integer? hex)
         (let-values ([(r g b) (hex->rgb-gamuts hex)])
           (rgba r g b (* (/ (exact->inexact a) 255.0) flalpha))))))

(define xterm-color-tuples : (Immutable-Vectorof Flonum) #(0.0 95.0 135.0 175.0 215.0 255.0))
(define xterm-system-colors : (Immutable-Vectorof Symbol)
  (vector-immutable 'black 'maroon 'green 'olive 'navy 'purple 'teal 'silver
                    'grey 'red 'lime 'yellow 'blue 'fuchsia 'aqua 'white))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://drafts.csswg.org/css-color/#named-colors
(define css-named-colors : (HashTable Symbol Index)
  #hasheq((aliceblue . #xF0F8FF)
          (antiquewhite . #xFAEBD7)
          (aqua . #xFFFF)
          (aquamarine . #x7FFFD4)
          (azure . #xF0FFFF)
          (beige . #xF5F5DC)
          (bisque . #xFFE4C4)
          (black . #x0)
          (blanchedalmond . #xFFEBCD)
          (blue . #xFF)
          (blueviolet . #x8A2BE2)
          (brown . #xA52A2A)
          (burlywood . #xDEB887)
          (cadetblue . #x5F9EA0)
          (chartreuse . #x7FFF00)
          (chocolate . #xD2691E)
          (coral . #xFF7F50)
          (cornflowerblue . #x6495ED)
          (cornsilk . #xFFF8DC)
          (crimson . #xDC143C)
          (cyan . #xFFFF)
          (darkblue . #x8B)
          (darkcyan . #x8B8B)
          (darkgoldenrod . #xB8860B)
          (darkgray . #xA9A9A9)
          (darkgreen . #x6400)
          (darkgrey . #xA9A9A9)
          (darkkhaki . #xBDB76B)
          (darkmagenta . #x8B008B)
          (darkolivegreen . #x556B2F)
          (darkorange . #xFF8C00)
          (darkorchid . #x9932CC)
          (darkred . #x8B0000)
          (darksalmon . #xE9967A)
          (darkseagreen . #x8FBC8F)
          (darkslateblue . #x483D8B)
          (darkslategray . #x2F4F4F)
          (darkslategrey . #x2F4F4F)
          (darkturquoise . #xCED1)
          (darkviolet . #x9400D3)
          (deeppink . #xFF1493)
          (deepskyblue . #xBFFF)
          (dimgray . #x696969)
          (dimgrey . #x696969)
          (dodgerblue . #x1E90FF)
          (firebrick . #xB22222)
          (floralwhite . #xFFFAF0)
          (forestgreen . #x228B22)
          (fuchsia . #xFF00FF)
          (gainsboro . #xDCDCDC)
          (ghostwhite . #xF8F8FF)
          (gold . #xFFD700)
          (goldenrod . #xDAA520)
          (gray . #x808080)
          (green . #x8000)
          (greenyellow . #xADFF2F)
          (grey . #x808080)
          (honeydew . #xF0FFF0)
          (hotpink . #xFF69B4)
          (indianred . #xCD5C5C)
          (indigo . #x4B0082)
          (ivory . #xFFFFF0)
          (khaki . #xF0E68C)
          (lavender . #xE6E6FA)
          (lavenderblush . #xFFF0F5)
          (lawngreen . #x7CFC00)
          (lemonchiffon . #xFFFACD)
          (lightblue . #xADD8E6)
          (lightcoral . #xF08080)
          (lightcyan . #xE0FFFF)
          (lightgoldenrodyellow . #xFAFAD2)
          (lightgray . #xD3D3D3)
          (lightgreen . #x90EE90)
          (lightgrey . #xD3D3D3)
          (lightpink . #xFFB6C1)
          (lightsalmon . #xFFA07A)
          (lightseagreen . #x20B2AA)
          (lightskyblue . #x87CEFA)
          (lightslategray . #x778899)
          (lightslategrey . #x778899)
          (lightsteelblue . #xB0C4DE)
          (lightyellow . #xFFFFE0)
          (lime . #xFF00)
          (limegreen . #x32CD32)
          (linen . #xFAF0E6)
          (magenta . #xFF00FF)
          (maroon . #x800000)
          (mediumaquamarine . #x66CDAA)
          (mediumblue . #xCD)
          (mediumorchid . #xBA55D3)
          (mediumpurple . #x9370DB)
          (mediumseagreen . #x3CB371)
          (mediumslateblue . #x7B68EE)
          (mediumspringgreen . #xFA9A)
          (mediumturquoise . #x48D1CC)
          (mediumvioletred . #xC71585)
          (midnightblue . #x191970)
          (mintcream . #xF5FFFA)
          (mistyrose . #xFFE4E1)
          (moccasin . #xFFE4B5)
          (navajowhite . #xFFDEAD)
          (navy . #x80)
          (oldlace . #xFDF5E6)
          (olive . #x808000)
          (olivedrab . #x6B8E23)
          (orange . #xFFA500)
          (orangered . #xFF4500)
          (orchid . #xDA70D6)
          (palegoldenrod . #xEEE8AA)
          (palegreen . #x98FB98)
          (paleturquoise . #xAFEEEE)
          (palevioletred . #xDB7093)
          (papayawhip . #xFFEFD5)
          (peachpuff . #xFFDAB9)
          (peru . #xCD853F)
          (pink . #xFFC0CB)
          (plum . #xDDA0DD)
          (powderblue . #xB0E0E6)
          (purple . #x800080)
          (rebeccapurple . #x663399)
          (red . #xFF0000)
          (rosybrown . #xBC8F8F)
          (royalblue . #x4169E1)
          (saddlebrown . #x8B4513)
          (salmon . #xFA8072)
          (sandybrown . #xF4A460)
          (seagreen . #x2E8B57)
          (seashell . #xFFF5EE)
          (sienna . #xA0522D)
          (silver . #xC0C0C0)
          (skyblue . #x87CEEB)
          (slateblue . #x6A5ACD)
          (slategray . #x708090)
          (slategrey . #x708090)
          (snow . #xFFFAFA)
          (springgreen . #xFF7F)
          (steelblue . #x4682B4)
          (tan . #xD2B48C)
          (teal . #x8080)
          (thistle . #xD8BFD8)
          (tomato . #xFF6347)
          (turquoise . #x40E0D0)
          (violet . #xEE82EE)
          (violetred . #xC71585)
          (wheat . #xF5DEB3)
          (white . #xFFFFFF)
          (whitesmoke . #xF5F5F5)
          (yellow . #xFFFF00)
          (yellowgreen . #x9ACD32)))

;;; https://drafts.csswg.org/css-color/#numeric-rgb
(define css-#hex-color->rgb : (-> (U Keyword String) (U False Symbol Index))
  (lambda [hex]
    (define color : String (if (keyword? hex) (keyword->immutable-string hex) hex))
    (case (string-length color)
      [(6) (string->index color 16)]
      [(3) (css-short-color->number color)]
      [else 'exn:digit])))

(define css-#hex-color->rgba : (-> (U Keyword String) (Values (U False Symbol Index) Byte))
  (lambda [hex]
    (define color : String (if (keyword? hex) (keyword->immutable-string hex) hex))
    (case (string-length color)
      [(3) (values (css-short-color->number color) #xFF)]
      [(4) (hex-rgba->rgb-a (css-short-color->number color))]
      [(6) (values (string->index color 16)  #xFF)]
      [(8) (hex-rgba->rgb-a (string->index color 16))]
      [else (values 'exn:digit  #xFF)])))

(define hex-rgba->rgb-a : (-> (Option Index) (Values (Option Index) Byte))
  (lambda [hex]
    (cond [(not hex) (values #false #xFF)]
          [else (values (unsafe-idxrshift hex 8)
                        (bitwise-and hex #xFF))])))

(define css-short-color->number : (-> String (Option Index))
  (lambda [color]
    (let color->hex ([chs : (Listof Char) (string->list color)]
                     [hex : Index 0])
      (cond [(null? chs) hex]
            [else (let ([ch (car chs)])
                    (define digit : (Option Integer)
                      (cond [(char-numeric? ch)   (- (char->integer ch) #x30)]
                            [(char<=? #\A ch #\F) (- (char->integer ch) #x37)]
                            [(char<=? #\a ch #\f) (- (char->integer ch) #x57)]
                            [else #false]))
                    (and (byte? digit)
                         (color->hex (cdr chs)
                                     (unsafe-idxior (unsafe-idxlshift hex 8)
                                                    (unsafe-idxior (unsafe-idxlshift digit 4)
                                                                   digit)))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rgba-attach-name : (-> FlRGBA Symbol RGBAN)
  (lambda [c name]
    (rgban (rgba-red c)
           (rgba-green c)
           (rgba-blue c)
           (rgba-alpha c)
           name)))
