#lang typed/racket

(provide (all-defined-out) <css-color>)
(provide (all-from-out bitmap/color))
(provide (all-from-out colorspace))

(require bitmap/digitama/color)
(require bitmap/color)
(require colorspace)

(require "digitama/syntax/digicore.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/color.rkt")
(require "recognizer.rkt")

(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;; TODO: should the current-css-element-color be a Racket Parameter? CSS does not have this concept.
(define current-css-element-color : (Parameterof Color+sRGB Color) (make-parameter (select-color #x000000) select-color))
(default-make-currentcolor current-css-element-color)

(define css-color-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  (lambda [name [px.names #px"-color$"]]
    (or (and (eq? name 'color) (<css-color> '#:inherit-currentcolor))
        (and (or (memq name '(background-color border-color))
                 (and (list? px.names) (memq name px.names))
                 (and (regexp? px.names) (regexp-match? px.names (symbol->string name))))
             (<css-color>)))))

(define css->color : (CSS->Racket (U Color CSS-Wide-Keyword 'currentcolor))
  (lambda [desc-name color]
    (cond [(color%? color) (select-color color)]
          [(eq? color 'currentcolor) color #| evaluated at used-value time |#]
          [(css-basic-color-datum? color) (select-color color)]
          [(hexa? color) (select-color (hexa-hex color) (hexa-a color))]
          [(rgba? color) (select-color (rgb-bytes->hex (rgba-r color) (rgba-g color) (rgba-b color)) (rgba-a color))]
          [(hsba? color) (select-color (hsb->rgb-hex (hsba->rgb color) (hsba-h color) (hsba-s color) (hsba-b color)) (hsba-a color))]
          [else css:initial])))

(define css-color-ref : (case-> [CSS-Values (Option CSS-Values) -> Color]
                                [CSS-Values (Option CSS-Values) Symbol -> (CSS-Maybe Color)]
                                [CSS-Values (Option CSS-Values) Symbol Color+sRGB -> Color])
  ;;; NOTE
  ;; `css-ref` will save all the values as computed value if it knows how to transform the cascaded values,
  ;; hence the `css-color-ref` to generate a more useful used value for clients so that clients do not need
  ;; to trace the `currentcolor` all the time. The correct current color may escape from the `parameterize`.
  (case-lambda
    [(declared-values inherited-values)
     (define color : (CSS-Maybe (U 'currentcolor Color)) (css-ref declared-values inherited-values 'color css->color))
     (if (css-wide-keyword? color) (current-css-element-color) (select-color color))]
    [(declared-values inherited-values property)
     (define xxx-color : (CSS-Maybe (U 'currentcolor Color)) (css-ref declared-values inherited-values property css->color))
     (if (eq? xxx-color 'currentcolor) (current-css-element-color) xxx-color)]
    [(declared-values inherited-values property defcolor)
     (define xxx-color : (CSS-Maybe (U 'currentcolor Color)) (css-ref declared-values inherited-values property css->color))
     (cond [(eq? xxx-color 'currentcolor) (current-css-element-color)]
           [(css-wide-keyword? xxx-color) (select-color defcolor)]
           [else xxx-color])]))
