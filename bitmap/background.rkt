#lang typed/racket

(provide (all-defined-out))

(require colorspace/misc)

(require "digitama/digicore.rkt")
(require "digitama/cheat.rkt")
(require "digitama/inspectable.rkt")
(require "digitama/background.rkt")
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
               [stipple (Option (Instance Bitmap%)) #:optional])))

(define-type CSS-Brush%
  (Class #:implements Inspectable-Brush%
         (init-field [color Color])
         (init [style Brush-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional]
               [gradient (U (Instance CSS-Linear-Gradient%) (Instance CSS-Radial-Gradient%) False) #:optional]
               [transformation (Option (Vector (Vector Real Real Real Real Real Real) Real Real Real Real Real)) #:optional])))

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

    (super-make-object color width style cap join stipple)
    
    (define/override-immutable 'css-pen% "pen is immutable"
      ([set-width width]
       [set-style style]
       [set-cap cap]
       [set-join join]
       [set-stipple bmp]))
    
    (define/override set-color
      (case-lambda
       [(color/name) (error 'css-pen% "pen is immutable")]
       [(r g b) (error 'css-pen% "pen is immutable")]))

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
    
    (super-make-object color style stipple gradient transformation)

    (define/override set-color
      (case-lambda
       [(color/name) (error 'css-brush% "brush is immutable")]
       [(r g b) (error 'css-brush% "brush is immutable")]))

    (define/override-immutable 'css-brush% "brush is immutable"
      ([set-style style]
       [set-stipple bmp [transformation #false]]))

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
(define default-css-pen : (Parameterof (Instance Pen%))
  (make-parameter (make-pen #:color (select-color 'transparent) #:width (generic-pen-width-map 'medium) #:style 'transparent)))

(define default-css-brush : (Parameterof (Instance Brush%))
  (make-parameter (make-brush #:color (select-color 'transparent) #:style 'transparent)))

(define make-css-pen : (->* ()
                            ((Instance Pen%) #:color (Option Color+sRGB) #:width (U Real Symbol False)
                                             #:style (Option Symbol) #:cap (Option Pen-Cap-Style) #:join (Option Pen-Join-Style)
                                             #:stipple (Option (Instance Bitmap%)))
                            (Instance CSS-Pen%))
  (let ([pen%? : (-> Any Boolean : #:+ (Instance Fixed-Pen%)) ((inst make-cheat-is-a? Fixed-Pen%) pen% 'pen%?)]
        [penbase : (HashTable (Listof Any) Pen) (make-hash)])
    (lambda [[basepen (default-css-pen)] #:color [color #false] #:width [width #false] #:style [style #false]
                                         #:cap [cap #false] #:join [join #false] #:stipple [stipple 'inherit]]
      (define pen-color : Color (select-color (or color (send basepen get-color))))
      (define pen-width : Real
        (cond [(memq style '(hidden none)) 0.0]
              [(and (real? width) (>= width 0.0)) (min 255.0 width)]
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
  (let ([brushbase : (HashTable (Listof Any) Brush) (make-hash)])
    (lambda [[basebrush (default-css-brush)] #:color [color #false] #:style [style #false] #:gradient [gradient #false]
                                             #:stipple [stipple 'inherit] #:transformation [transformation 'inherit]]
      (define brush-color : Color (select-color (or color (send basebrush get-color))))
      (define brush-style : Brush-Style (or style (send basebrush get-style)))
      ; TODO: deal with stipple gradient and transformation
      (hash-ref! brushbase
                 (list brush-color brush-style)
                 (λ [] (make-object css-brush% brush-color brush-style))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-pen : (-> Pen+Color Pen)
  (lambda [pen-hint]
    (cond [(css-pen%? pen-hint) pen-hint]
          [else ((inst smart-pen Pen) pen-hint make-css-pen)])))

(define select-brush : (-> Brush+Color Brush)
  (lambda [brush-hint]
    (cond [(css-brush%? brush-hint) brush-hint]
          [else ((inst smart-brush Brush) brush-hint make-css-brush)])))
