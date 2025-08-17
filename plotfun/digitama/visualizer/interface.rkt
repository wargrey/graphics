#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [Palette-Index->Pen+Brush-Colors Plot-Palette]))

(require geofun/color)
(require geofun/stroke)

(require geofun/digitama/base)
(require geofun/digitama/paint/self)

(require colorspace/palette)

(require "style.rkt")
(require "reference.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Visualizer-Label-Placement (U 'left 'right 'auto 'flip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-cartesian-view-width : (Parameterof Real) (make-parameter 400.0))
(define default-plot-cartesian-view-height : (Parameterof Real) (make-parameter +inf.0))
(define default-plot-visualizer-domain-range : (Parameterof (Pairof Real Real)) (make-parameter (cons -5 5)))
(define default-plot-visualizer-sample-density : (Parameterof Byte) (make-parameter 0))
(define default-plot-visualizer-label-position : (Parameterof Real) (make-parameter +nan.0))
(define default-plot-visualizer-label-position-range : (Parameterof (Pairof Real Real)) (make-parameter (cons 0.0 1.0)))
(define default-plot-visualizer-label-placement : (Parameterof Plot-Visualizer-Label-Placement) (make-parameter 'auto))

(define default-plot-function-stroke : (Parameterof Stroke) (make-parameter default-function-stroke))
(define default-plot-palette : (Parameterof Palette-Index->Pen+Brush-Colors) (make-parameter the-oklch-palette))
(define plot-sampling? : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-desc-pen : (->* ()
                             (Stroke #:color (Option Color) #:opacity (Option Real) #:width (Option Real) #:dash (Option Stroke-Dash+Offset))
                             Stroke)
  (lambda [#:color [color #false] #:opacity [opacity #false] #:width [width #false] #:dash [dash+offset #false]
           [baseline (default-plot-function-stroke)]]
    (define-values (dash offset)
      (if (pair? dash+offset)
          (values (car dash+offset) (cdr dash+offset))
          (values dash+offset #false)))
    
    (desc-stroke #:color color #:opacity opacity #:width width #:dash dash #:offset offset
                 baseline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For the sake of simplicity
;  the color source referred to should already be defined before the referring one.
(define plot-select-pen-color : (-> (Option Color) Natural (Option FlRGBA) FlRGBA)
  (lambda [alt-color idx bg-color]
    (cond [(not alt-color) (car ((default-plot-palette) idx bg-color))]
          [(symbol? alt-color)
           (let* ([cpool (current-visualizer-color-pool)]
                  [prev-c (and cpool (hash-ref cpool alt-color (λ _ #false)))])
             (cond [(or prev-c) prev-c]
                   [else ((default-plot-palette) (rgb* alt-color) bg-color)]))]
          [else ((default-plot-palette) (rgb* alt-color) bg-color)])))

(define plot-select-brush-color : (-> (Option Color) Natural (Option FlRGBA) FlRGBA)
  (lambda [alt-color idx bg-color]
    (cond [(not alt-color) (cdr ((default-plot-palette) idx bg-color))]
          [(symbol? alt-color)
           (let* ([cpool (current-visualizer-color-pool)] ; TODO: set another pool for brush colors
                  [prev-c (and cpool (hash-ref cpool alt-color (λ _ #false)))])
             (cond [(or prev-c) prev-c]
                   [else ((default-plot-palette) (rgb* alt-color) bg-color)]))]
          [else ((default-plot-palette) (rgb* alt-color) bg-color)])))

(define plot-adjust-pen-color : (-> Palette-Index->Pen+Brush-Colors Color (Option FlRGBA) FlRGBA)
  (lambda [palette color bg-color]
    (palette (rgb* color) bg-color)))

(define plot-adjust-brush-color : (-> Palette-Index->Pen+Brush-Colors Color (Option FlRGBA) FlRGBA)
  (lambda [palette color bg-color]
    (palette (rgb* color) bg-color)))
