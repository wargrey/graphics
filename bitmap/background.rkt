#lang typed/racket

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "digitama/cheat.rkt")
(require "digitama/background.rkt")
(require "color.rkt")

(require "digitama/unsafe/source.rkt")

(define-predicate pen-style? Pen-Style)
(define-predicate brush-style? Brush-Style)

(define-type Pen (Instance CSS-Pen%))
(define-type Brush (Instance CSS-Brush%))

(define-type CSS-Gradient-Stop-Color (Pairof Real (Instance Color%)))
(define-type CSS-Linear-Gradient (Vector Real Real Real Real (Listof CSS-Gradient-Stop-Color)))
(define-type CSS-Radial-Gradient (Vector Real Real Real Real Real Real (Listof CSS-Gradient-Stop-Color)))

(define-type CSS-Pen%
  (Class #:implements Pen%
         (init-field [color Color])
         (init [width Real #:optional]
               [style Pen-Style #:optional]
               [cap Pen-Cap-Style #:optional]
               [join Pen-Join-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional])))

(define-type CSS-Brush%
  (Class #:implements Brush%
         (init-field [color Color])
         (init [style Brush-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional]
               [gradient (U (Instance Linear-Gradient%) (Instance Radial-Gradient%) False) #:optional]
               [transformation (Option (Vector (Vector Real Real Real Real Real Real) Real Real Real Real Real)) #:optional])
         [get-source (-> Bitmap-Source)]))

(define/make-is-a? css-pen% : CSS-Pen%
  (class pen%
    (init-field color)
    (init [width 1.0] [style 'solid] [cap 'round] [join 'round] [stipple #false])

    (super-make-object color width style cap join stipple)))

(define/make-is-a? css-brush% : CSS-Brush%
  (class brush%
    (init-field color)
    (init [style 'solid] [stipple #false] [gradient #false] [transformation #false])
    
    (super-make-object color style stipple gradient transformation)

    (define source : Bitmap-Source (generic-brush->pattern this))

    (define/public (get-source) source)))

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
  (let ([penbase : (HashTable (Listof Any) Pen) (make-hash)])
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
                      [else (send basepen get-style)])]))
      ; TODO: deal with cap, join and stipple
      (hash-ref! penbase
                 (list pen-color pen-width pen-style)
                 (λ [] (make-object css-pen% pen-color pen-width pen-style))))))

(define make-css-brush : (->* ()
                              ((Instance Brush%) #:color (Option Color+sRGB) #:style (Option Brush-Style)
                                                 #:gradient (U False CSS-Linear-Gradient CSS-Radial-Gradient)
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

(define brush->source : (-> (Instance Brush%) Bitmap-Source)
  (lambda [brush]
    (cond [(css-brush%? brush) (send brush get-source)]
          [else (generic-brush->pattern brush)])))
