#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require geofun/color)
(require geofun/fill)

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/paint/self)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/sticker)

(require geofun/digitama/dc/resize)
(require geofun/digitama/dc/adjuster)

(require "../self.rkt")
(require "../interface.rkt")
(require "../vaid/self.rkt")
(require "../../axis/view.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sticker
  (lambda [#:id [id : (Option Symbol) #false]
           #:color [color : (Option Color) #false]
           #:width [strk-width : (Option Real) #false]
           #:dash [strk-dash : (Option Stroke-Dash+Offset) #false]
           #:stroke-opacity [strk-opacity : (Option Real) #false]
           #:fill-opacity [fll-opacity : (Option Real) #false]
           #:fill-rule [fll-rule : (Option Fill-Rule) #false]
           #:scale? [scale? : Boolean #false]
           #:skip-palette? [skip-palette? : Boolean #false]
           #:offset [offset : Complex 0.0+0.0i]
           [self : Geo] [pos : Complex] [anchor : Geo-Pin-Anchor 'cc]] : Plot-Visualizer
    (define-values (width height) (geo-flsize self))
    (define-values (px py) (values (real-part pos) (imag-part pos)))
    (define-values (xrange yrange)
      (cond [(not scale?) (plot-range-normalize px px py py)]
            [else (plot-range-normalize px (+ px width)
                                        py (+ py height))]))

    (define flpos  (make-rectangular (real->double-flonum px) (real->double-flonum py)))
    (define-values (xoff yoff) (values (real->double-flonum (real-part offset)) (real->double-flonum (imag-part offset))))

    (define sticker-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define pen : Pen
          (plot-desc-pen #:dash strk-dash #:width strk-width #:opacity strk-opacity
                         #:color (plot-select-pen-color color idx bg-color)
                         (default-plot-sticker-pen)))
        (define brush : Brush
          (plot-desc-brush #:color (plot-select-brush-color color idx bg-color) #:rule fll-rule #:opacity fll-opacity
                           (default-plot-sticker-brush)))

        (define xunit (real-part (plot-vxunit transform)))
        (define yunit (imag-part (plot-vyunit transform)))
        (define adjusted-anchor (if (< yunit 0.0) (geo-anchor-vertical-flip anchor) anchor))

        (and (<= xmin px xmax) (<= ymin py ymax)
             (let ([the-sticker (geo-try-repaint self #:id id #:stroke pen #:fill brush)]
                   [descriptor (desc-geo:visualizer #:position (transform flpos)
                                                    #:color (pen-color pen)
                                                    #:visual-aids (list))])
               (if (or scale?)      
                   (cons (make-sticker (geo-scale the-sticker (abs xunit) (abs yunit))
                                       adjusted-anchor
                                       (make-rectangular (* xoff xunit) (* yoff yunit)))
                         descriptor)
                   (cons (make-sticker the-sticker
                                       adjusted-anchor
                                       (make-rectangular xoff (* yoff (sgn yunit))))
                         descriptor))))))
      
    (plot-visualizer sticker-realize xrange yrange
                     (cond [(not scale?) (sticker-range px py)]
                           [else (sticker-range px py width height)])
                     (and (or color skip-palette?) #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sticker-range : (case-> [Real Real -> Plot-Visualizer-Data-Range]
                                [Real Real Nonnegative-Flonum Nonnegative-Flonum -> Plot-Visualizer-Data-Range])
  (case-lambda
    [(px py)
     (λ [xmin xmax]
       (if (<= xmin px xmax)
           (cons py py)
           (cons -nan.0 +nan.0)))]
    [(px py width height)
     (λ [xmin xmax]
       (if (<= xmin px xmax)
           (cons py (+ py height))
           (cons -nan.0 +nan.0)))]))
