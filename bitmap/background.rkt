#lang typed/racket

(provide (all-defined-out))

(require colorspace/misc)

(require "digitama/digicore.rkt")
(require "digitama/cheat.rkt")
(require "digitama/color.rkt")
(require "digitama/inspectable.rkt")
(require "color.rkt")

(define-predicate pen-style? Pen-Style)
(define-predicate brush-style? Brush-Style)

(define-type Pen (Instance CSS-Pen%))
(define-type Brush (Instance CSS-Brush%))

(define-type CSS-Pen%
  (Class #:implements Inspectable-Pen%
         (init-field [color Color])
         (init [width Real #:optional]
               [style Pen-Style #:optional]
               [cap Pen-Cap-Style #:optional]
               [join Pen-Join-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional])
         (init-field [immutable? Boolean #:optional])))

(define-type CSS-Brush%
  (Class #:implements Inspectable-Brush%
         (init-field [color Color])
         (init [style Brush-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional]
               [gradient (U (Instance CSS-Linear-Gradient%) (Instance CSS-Radial-Gradient%) False) #:optional]
               [transformation (Option (Vector (Vector Real Real Real Real Real Real) Real Real Real Real Real)) #:optional])
         (init-field [immutable? Boolean #:optional])))

(define-type CSS-Linear-Gradient%
  (Class #:implements Inspectable-Linear-Gradient%
         (init [x0 Real] [y0 Real]
               [x1 Real] [y1 Real]
               [stops (Listof (List Real Color))])))

(define-type CSS-Radial-Gradient%
  (Class #:implements Inspectable-Radial-Gradient%
         (init [x0 Real] [y0 Real] [r0 Real]
               [x1 Real] [y1 Real] [r1 Real]
               [stops (Listof (List Real Color))])))

(define/make-is-a? css-pen% : CSS-Pen%
  (class inspectable-pen%
    (init-field color)
    (init [width 1.0] [style 'solid] [cap 'round] [join 'round] [stipple #false])
    (init-field [immutable? #true])

    (super-make-object color width style cap join stipple)

    (define/override set-color
      (case-lambda
       [(color/name)
        (when immutable? (error 'css-pen% "pen is immutable"))
        (set! color (select-color color/name))
        (super set-color color)]
       [(r g b)
        (when immutable? (error 'css-pen% "pen is immutable"))
        (set! color (select-color (rgb-bytes->hex r g b)))
        (super set-color color)]))
    
    (define/override-immutable 'css-pen% immutable? "pen is immutable"
      ([set-width width]
       [set-style style]
       [set-cap cap]
       [set-join join]
       [set-stipple bmp]))

    (define/override (is-immutable?)
      immutable?)

    (define/override (get-color)
      ;;; Awkward, it copys the color instance everytime to make it immutable and opaque again
      ;;; since there is no way to tell it "It's already immutable".
      color)
    
    (define/override (inspect)
      (list color (send this get-width) (send this get-style)
            (send this get-cap) (send this get-join)))))

(define/make-is-a? css-brush% : CSS-Brush%
  (class inspectable-brush%
    (init-field color)
    (init [style 'solid] [stipple #false] [gradient #false] [transformation #false])
    (init-field [immutable? #true])

    (super-make-object color style stipple gradient transformation)

    (define/override set-color
      (case-lambda
       [(color/name)
        (when immutable? (error 'css-brush% "brush is immutable"))
        (set! color (select-color color/name))
        (super set-color color)]
       [(r g b)
        (when immutable? (error 'css-brush% "brush is immutable"))
        (set! color (select-color (rgb-bytes->hex r g b)))
        (super set-color color)]))

    (define/override (set-style style)
      (when immutable? (error 'css-brush% "brush is immutable"))
      (super set-style style))

    (define/override (set-stipple bmp [transformation #false])
      (when immutable? (error 'css-brush% "brush is immutable"))
      (super set-stipple bmp transformation))
    
    (define/override (is-immutable?)
      immutable?)

    (define/override (get-color)
      ;;; Awkward, it copys the color instance everytime to make it immutable and opaque again
      ;;; since there is no way to tell it "It's already immutable".
      color)
    
    (define/override (inspect)
      (list color (send this get-style) (send this get-gradient) (send this get-transformation)))))

(define/make-is-a? css-linear-gradient% : CSS-Linear-Gradient%
  (class inspectable-linear-gradient%
    (init x0 y0 x1 y1 stops)
    (super-make-object x0 y0 x1 y1 stops)

    (define/override (inspect)
      (define-values (x0 y0 x1 y1) (send this get-line))
      (list x0 y0 x1 y1 (send this get-stops)))))

(define/make-is-a? css-radial-gradient% : CSS-Radial-Gradient%
  (class inspectable-radial-gradient%
    (init x0 y0 r0 x1 y1 r1 stops)
    (super-make-object x0 y0 r0 x1 y1 r1 stops)

    (define/override (inspect)
      (define-values (x0 y0 r0 x1 y1 r1) (send this get-circles))
      (list x0 y0 r0 x1 y1 r1 (send this get-stops)))))

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
      (define pen-color : Color (select-color (or color (send basepen get-color))))
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

(define make-css-brush : (->* ()
                              ((Instance Brush%) #:color (Option Color+sRGB) #:style (Option Brush-Style)
                                                 #:gradient (U False CSS-Linear-Gradient% CSS-Radial-Gradient%)
                                                 #:stipple (Option (Instance Bitmap%))
                                                 #:transformation (U False (Vector (Vector Real Real Real Real Real Real)
                                                                                   Real Real Real Real Real)))
                              (Instance CSS-Brush%))
  (let ([brushbase : (HashTable (Listof Any) Brush) (make-hash)]
        [rootbrush : (Instance Brush%) (make-brush #:color (select-color #x000000))])
    (lambda [[basebrush rootbrush] #:color [color #false] #:style [style #false] #:gradient [gradient #false]
                                   #:stipple [stipple 'inherit] #:transformation [transformation 'inherit]]
      (define brush-color : Color (select-color (or color (send basebrush get-color))))
      (define brush-style : Brush-Style (or style (send basebrush get-style)))
      ; TODO: deal with stipple gradient and transformation
      (hash-ref! brushbase
                 (list brush-color brush-style)
                 (λ [] (make-object css-brush% brush-color brush-style))))))
