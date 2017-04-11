#lang typed/racket

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "digitama/cheat.rkt")
(require "digitama/color.rkt")
(require "digitama/inspectable.rkt")
(require "color.rkt")

(define-predicate pen-style? Pen-Style)

(define-type Pen (Instance CSS-Pen%))

(define-type CSS-Pen%
  (Class #:implements Inspectable-Pen%
         (init-field [color Color])
         (init [width Real #:optional]
               [style Pen-Style #:optional])
         (init-field [immutable? Boolean #:optional])
         (init [cap Pen-Cap-Style #:optional]
               [join Pen-Join-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional])))

(define/make-is-a? css-pen% : CSS-Pen%
  (class inspectable-pen%
    (init-field color)
    (init [width 1.0] [style 'solid])
    (init-field [immutable? #true])
    (init [cap 'round] [join 'round] [stipple #false])

    (super-make-object color width style cap join stipple)

    (define/override set-color
      (case-lambda
       [(color/name)
        (when immutable? (error 'css-pen% "pen is immutable"))
        (super set-color color/name)]
       [(r g b)
        (when immutable? (error 'css-pen% "pen is immutable"))
        (super set-color r g b)]))
    
    (define/override-immutable 'css-pen% immutable? "pen is immutable"
      ([set-width width]
       [set-style style]
       [set-cap cap]
       [set-join join]
       [set-stipple bmp]))

    (define/override (is-immutable?)
      immutable?)
    
    (define/override (inspect)
      (list (send this get-color) (send this get-width) (send this get-style)
            (send this get-cap) (send this get-join)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-css-pen : (->* ()
                            ((Instance Pen%) #:color (Option Color+sRGB) #:width (U Nonnegative-Real Symbol False)
                                             #:style (Option Symbol) #:cap (Option Pen-Cap-Style) #:join (Option Pen-Join-Style)
                                             #:stipple (Option (Instance Bitmap%)))
                            (Instance CSS-Pen%))
  (let ([pen%? : (-> Any Boolean : #:+ (Instance Fixed-Pen%)) ((inst make-cheat-is-a? Fixed-Pen%) pen% 'pen%?)]
        [penbase : (HashTable (Listof Any) Pen) (make-hash)]
        [rootpen : (Instance Pen%) (make-pen #:color (select-color #x000000))])
    (lambda [[basepen rootpen] #:color [color #false] #:width [width #false] #:style [style #false]
                               #:cap [cap #false] #:join [join #false] #:stipple [stipple 'inherit]]
      (define pen-color : Color
        (cond [(rgba%? color) color]
              [(css-basic-color-datum? color) (select-color color)]
              [(color%? color) (select-color color)]
              [else (select-color (send basepen get-color))]))
      (define pen-width : Real
        (cond [(memq style '(hidden none)) 0.0]
              [(real? width) (min 255.0 width)]
              [(eq? width 'thin) 1.0]
              [(eq? width 'medium) 3.0]
              [(eq? width 'thick) 5.0]
              [else (send basepen get-width)]))
      (define pen-style : Pen-Style
        (cond [(pen-style? style) style]
              [else (case style ; TODO: deal with other CSS style options
                      [(hidden none) 'transparent]
                      [(dotted) 'dot]
                      [(dashed) 'long-dash]
                      [else (if (pen%? basepen) (send basepen get-style) 'solid)])]))
      ; TODO: deal with cap, join and stipple
      (hash-ref! penbase
                 (list pen-color pen-width pen-style)
                 (λ [] (make-object css-pen% pen-color pen-width pen-style))))))
