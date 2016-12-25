#lang typed/racket

(provide (all-defined-out) <css-color>)

(require "digitama/digicore.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/color.rkt")
(require "recognizer.rkt")

(define current-css-element-color : (Parameterof CSS-Datum Color+sRGB)
  (make-parameter (select-rgba-color #x000000)
                  (λ [[c : CSS-Datum]]
                    (define color : (U Color CSS-Wide-Keyword 'currentcolor) (css->color 'color c))
                    (if (object? color) color (current-css-element-color)))))

(default-make-currentcolor current-css-element-color)

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
