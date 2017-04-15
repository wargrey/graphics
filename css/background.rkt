#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out bitmap/background))

(require bitmap/background)

(require "digitama/syntax/digicore.rkt")
(require "digitama/background.rkt")
(require "recognizer.rkt")
(require "color.rkt")

;;; WARNING
;; All properties in this specification are *not* inheritable.

(define css-simple-box-property-parsers : (-> Symbol (Option CSS-Declaration-Parser))
  ;;; https://www.w3.org/TR/CSS2/box.html#box-model
  ;;; https://drafts.csswg.org/css-backgrounds/#borders
  (lambda [suitcased-name]
    (case suitcased-name
      [(margin) <:margin:>]
      [(padding) <:padding:>]
      [(border) <:border:>]
      [(border-top) <:border-top:>]
      [(border-right) <:border-right:>]
      [(border-bottom) <:border-bottom:>]
      [(border-left) <:border-left:>]
      [(border-color) <:border-color:>]
      [(border-width) <:border-width:>]
      [(border-style) <:border-style:>]
      [(width height) (<css-size>)]
      [(margin-top margin-right margin-bottom margin-left) (<css-size>)]
      [(padding-top padding-right padding-bottom padding-left) (<css-size>)]
      [(border-top-color border-right-color border-bottom-color border-left-color) (<css-color>)]
      [(border-top-width border-right-width border-bottom-width border-left-width) (<border-width>)]
      [(border-top-style border-right-style border-bottom-style border-left-style) (<css-keyword> css-border-style-option)]
      [(background-color) (<css-color>)]
      [(background-style) (<css-keyword> brush-style?)]
      [else #false])))

(define css-extract-border-pen : (case-> [CSS-Values (Option CSS-Values) -> (Listof Pen)]
                                         [CSS-Values (Option CSS-Values) Symbol -> Pen])
  (case-lambda
    [(declared-values inherited-values)
     (list (css-extract-border-pen declared-values #false 'top)
           (css-extract-border-pen declared-values #false 'right)
           (css-extract-border-pen declared-values #false 'bottom)
           (css-extract-border-pen declared-values #false 'left))]
    [(declared-values inherited-values which-side)
     (define-values (color-key width-key style-key)
       (case which-side
         [(right)  (values 'border-right-color  'border-right-width  'border-right-style)]
         [(bottom) (values 'border-bottom-color 'border-bottom-width 'border-bottom-style)]
         [(left)   (values 'border-left-color   'border-left-width   'border-left-style)]
         [else     (values 'border-top-color    'border-top-width    'border-top-style)]))
     (make-css-pen #:color (css-color-ref declared-values #false color-key #false)
                   #:width (css-ref declared-values #false width-key css->border-width)
                   #:style (css-ref declared-values #false style-key symbol? #false))]))

(define css-extract-background-brush : (-> CSS-Values (Option CSS-Values) Brush)
  (lambda [declared-values inherited-values]
    (make-css-brush #:color (css-color-ref declared-values #false 'background-color #false)
                    #:style (css-ref declared-values #false 'background-style brush-style? #false))))
