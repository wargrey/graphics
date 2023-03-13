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
    (cond [(>= sgr #xE8)
           (define grey : Flonum (/ (real->double-flonum (+ (* (- sgr #xE8) 10) 8)) 255.0))
           (rgba grey grey grey flalpha)]
          [(>= sgr #x10)
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

(define xterm-color-tuples : (Vectorof Flonum) (vector 0.0 95.0 135.0 175.0 215.0 255.0))
(define xterm-system-colors : (Vectorof Symbol)
  (vector 'black 'maroon 'green 'olive 'navy 'purple 'teal 'silver
          'grey 'red 'lime 'yellow 'blue 'fuchsia 'aqua 'white))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://drafts.csswg.org/css-color/#named-colors
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
