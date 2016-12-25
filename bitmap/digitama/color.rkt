#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out colorspace))
(provide (all-from-out racket/string racket/bool))

(require racket/string)
(require racket/bool)

(require colorspace)

(require "bitmap.rkt")

(define the-color-pool : (HashTable Fixnum Color) (make-hasheq))
(define default-make-currentcolor : (Parameterof (-> Color+sRGB)) (make-parameter (λ [] #x000000)))

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

(define select-rgba-color : (->* (Color+sRGB) (Nonnegative-Flonum) Color)
  (lambda [representation [alpha 1.0]]
    (define opaque? : Boolean (fl= alpha 1.0))
    (cond [(fixnum? representation)
           (define hashcode : Nonnegative-Fixnum (fxand representation #xFFFFFF))
           (hash-ref! the-color-pool
                      (if opaque? hashcode (eqv-hash-code (make-rectangular hashcode alpha)))
                      (λ [] (let-values ([(r g b) (hex->rgb-bytes representation)])
                              (make-color r g b alpha))))]
          [(symbol? representation)
           (let try-again ([color-name : Symbol representation]
                           [downcased? : Boolean #false])
             (cond [(hash-has-key? css-named-colors color-name)
                    (hash-ref! the-color-pool
                               (cond [(and opaque?) (eq-hash-code color-name)]
                                     [else (equal-hash-code (cons color-name alpha))])
                               (λ [] (select-rgba-color (hash-ref css-named-colors color-name) alpha)))]
                   [(not downcased?) (try-again (string->symbol (string-downcase (symbol->string color-name))) #true)]
                   [(eq? color-name 'currentcolor) (select-rgba-color ((default-make-currentcolor)))]
                   [else (select-rgba-color #x000000 (if (eq? color-name 'transparent) 0.0 alpha))]))]
          [(string? representation)
           (let* ([color-name (string-downcase (string-replace representation #px"(?i:grey)" "gray"))]
                  [color (send the-color-database find-color color-name)])
             (cond [(false? color) (select-rgba-color #x000000 alpha)]
                   [else (hash-ref! the-color-pool
                                    (equal-hash-code (if opaque? color-name (cons color-name alpha)))
                                    (λ [] (select-rgba-color (rgb-bytes->hex (send color red) (send color green) (send color blue))
                                                             alpha)))]))]
          [else representation])))
