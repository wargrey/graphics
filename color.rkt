#lang typed/racket

(provide (all-defined-out) <css-color>)
(provide (all-from-out "digitama/bitmap.rkt" "colorspace.rkt"))

(require "digitama/digicore.rkt")
(require "digitama/color.rkt")
(require "digitama/bitmap.rkt")
(require "colorspace.rkt")

(define select-rgba-color : (->* (Color+sRGB) (Nonnegative-Flonum) Color)
  (lambda [representation [alpha 1.0]]
    (define opaque? : Boolean (fl= alpha 1.0))
    (cond [(fixnum? representation)
           (define hashcode : Nonnegative-Fixnum (fxand representation #xFFFFFF))
           (hash-ref! the-color-pool
                      (if opaque? hashcode (eqv-hash-code (make-rectangular hashcode alpha)))
                      (thunk (let-values ([(r g b) (hex->rgb-bytes representation)])
                               (make-color r g b alpha))))]
          [(symbol? representation)
           (let try-again ([color-name : Symbol representation]
                           [downcased? : Boolean #false])
             (cond [(hash-has-key? css-named-colors color-name)
                    (hash-ref! the-color-pool
                               (cond [(and opaque?) (eq-hash-code color-name)]
                                     [else (equal-hash-code (cons color-name alpha))])
                               (thunk (select-rgba-color (hash-ref css-named-colors color-name) alpha)))]
                   [(not downcased?) (try-again (string->symbol (string-downcase (symbol->string color-name))) #true)]
                   [(eq? color-name 'currentcolor) (select-rgba-color (current-css-element-color))]
                   [else (select-rgba-color #x000000 (if (eq? color-name 'transparent) 0.0 alpha))]))]
          [(string? representation)
           (let* ([color-name (string-downcase (string-replace representation #px"(?i:grey)" "gray"))]
                  [color (send the-color-database find-color color-name)])
             (cond [(false? color) (select-rgba-color #x000000 alpha)]
                   [else (hash-ref! the-color-pool
                                    (equal-hash-code (if opaque? color-name (cons color-name alpha)))
                                    (thunk (select-rgba-color (rgb-bytes->hex (send color red) (send color green) (send color blue))
                                                              alpha)))]))]
          [else representation])))

(define current-css-element-color : (Parameterof CSS-Datum Color+sRGB)
  (make-parameter (select-rgba-color #x000000)
                  (λ [[c : CSS-Datum]]
                    (define color : (U Color CSS-Wide-Keyword 'currentcolor) (css->color 'color c))
                    (if (object? color) color (current-css-element-color)))))

(define css-color-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  (lambda [name [px.names #px"-color$"]]
    (or (and (eq? name 'color) (CSS<^> (<css-color> '#:inherit-currentcolor)))
        (and (or (and (list? px.names) (memq name px.names))
                 (and (regexp? px.names) (regexp-match? px.names (symbol->string name))))
             (CSS<^> (<css-color>))))))

(define css->color : (-> Symbol CSS-Datum (U Color CSS-Wide-Keyword 'currentcolor))
  (lambda [desc-name color]
    (cond [(color%? color) color]
          [(eq? color 'currentcolor) color #| evaluated at used-value time |#]
          [(css-basic-color-datum? color) (select-rgba-color color)]
          [(hexa? color) (select-rgba-color (hexa-hex color) (hexa-a color))]
          [(rgba? color) (select-rgba-color (rgb-bytes->hex (rgba-r color) (rgba-g color) (rgba-b color)) (rgba-a color))]
          [(hsba? color) (select-rgba-color (hsb->rgb-hex (hsba->rgb color) (hsba-h color) (hsba-s color) (hsba-b color)) (hsba-a color))]
          [else css:initial])))
