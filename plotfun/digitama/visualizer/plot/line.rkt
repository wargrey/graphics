#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/paint)
(require geofun/digitama/richtext/self)

(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/dot)
(require geofun/digitama/nice/pairable)

(require "../dot.rkt")
(require "../self.rkt")
(require "../guard.rkt")
(require "../interface.rkt")

(require "../../marker/self.rkt")
(require "../../unsafe/line.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:lines geo:line:visualizer
  ([close? : Boolean])
  #:type-name Plot:Lines
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lines
  (lambda [#:id [id : (Option Symbol) #false]
           #:label [label : (U False Geo-Rich-Text Plot:Mark) #false]
           #:label-position [at-frac : Real (default-plot-visualizer-label-position)]
           #:label-position-range [rng-frac : (Pairof Real Real) (default-plot-visualizer-label-position-range)]
           #:label-placement [placement : Plot-Visualizer-Label-Placement (default-plot-visualizer-label-placement)]
           #:scale [scale : Pairable-Real 1.0]
           #:offset [offset : Complex 0.0+0.0i]
           #:color [strk-color : (Option Color) #false]
           #:width [strk-width : (Option Real) #false]
           #:opacity [opacity : (Option Real) #false]
           #:dash [strk-dash : (Option Stroke-Dash+Offset) #false]
           #:close? [close? : Boolean #false]
           [pts : (Listof Point2D)]
           [maybe-xmin : (Option Real) #false] [maybe-xmax : (Option Real) #false]
           [maybe-ymin : (Option Real) #false] [maybe-ymax : (Option Real) #false]] : Plot-Visualizer
    (define-values (sx sy) (2d-scale-values scale))
    (define-values (points lx ty rx by) (~point2ds pts offset sx sy))
    (define df/dx : (-> Real (Option Real)) (plot-lines-df/dx points))
    (define safe-f : (-> Real Real) (plot-lines->function points close?))
    (define-values (xrange yrange)
      (plot-range-normalize (or maybe-xmin lx) (or maybe-xmax rx)
                            (or maybe-ymin ty) (or maybe-ymax by)))

    (define lines-realize : Plot-Visualizer-Realize
      (λ [idx total xmin xmax ymin ymax transform bg-color]
        (define-values (dots x y width height) (~cartesian2ds points xmin xmax ymin ymax transform))
        (define dynamic-angle : (-> Real (Option Real))
          (plot-function-pin-angle df/dx 0 placement))

        (define pen : Pen
          (plot-desc-pen #:width strk-width #:dash strk-dash #:opacity opacity
                         #:color (plot-select-pen-color strk-color idx bg-color)
                         (default-plot-function-pen)))

        (create-visualizer plot:lines
                           #:with [id (geo-draw-lines pen)
                                      (geo-shape-extent width height 0.0 0.0)
                                      (geo-shape-outline pen #true #true)

                                      #:position (make-rectangular x y)
                                      #:color (pen-color pen)
                                      #:label (plot-function-mark-guard safe-f label (+ idx 1) total xmin xmax ymin ymax at-frac rng-frac)
                                      #:pin-angle dynamic-angle
                                      #:gap-angle dynamic-angle]
                           dots close?)))

    (plot-visualizer lines-realize xrange yrange
                     (plot-lines-range points)
                     (and strk-color #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-lines-range : (-> (Listof Complex) Plot-Visualizer-Data-Range)
  (lambda [pts]
    (λ [xmin xmax]
      (~y-bounds pts xmin xmax))))

(define plot-lines-samples : (-> (Listof Complex) Real Real (Listof Real))
  (lambda [pts xmin xmax]
    (let sample ([pts : (Listof Complex) pts]
                 [sx : (Listof Real) null])
      (if (pair? pts)
          (let*-values ([(self rest) (values (car pts) (cdr pts))]
                        [(this-x) (real-part self)])
            (cond [(> this-x xmax) (sample null sx)]
                  [(< this-x xmin) (sample rest sx)]
                  [else (sample rest (cons this-x sx))]))
          (reverse sx)))))

(define plot-lines->function : (-> (Listof Complex) Boolean (-> Real Real))
  (lambda [pts0 close?]
    (define pts
      (cond [(null? pts0) pts0]
            [(not close?) pts0]
            [else (append pts0 (list (car pts0)))]))
    
    (define ys : (HashTable Real Real)
      (for/hasheqv : (HashTable Real Real) ([pt (in-list pts)])
        (values (real-part pt)
                (imag-part pt))))
    
    (λ [[x : Real]]
      (hash-ref ys x
                (λ [] (let f : Real ([pts : (Listof Complex) pts])
                        (if (pair? pts)
                            (let*-values ([(self rest) (values (car pts) (cdr pts))]
                                          [(this-x) (real-part self)])
                              (cond [(< x this-x) (f rest)]
                                    [(pair? rest)
                                     (let* ([v (- (car rest) self)]
                                            [d (* v (/ (- this-x x) (real-part v)))])
                                       (imag-part (+ self d)))]
                                    [else +nan.0]))
                            +nan.0)))))))

(define plot-lines-df/dx : (-> (Listof Complex) (-> Real (Option Real)))
  (lambda [pts]
    (λ [[x : Real]]
      (let slope ([pts : (Listof Complex) pts])
        (and (pair? pts)
             (let-values ([(self rest) (values (car pts) (cdr pts))])
               (cond [(< x (real-part self)) (slope rest)]
                     [(pair? rest)
                      (let ([v (- (car rest) self)])
                        (cond [(zero? (real-part v)) +inf.0]
                              [else (/ (imag-part v)
                                       (real-part v))]))]
                     [else #false])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-lines : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (plot:lines? self)
        (define pos (geo:visualizer-position self))
        (dc_line cr (- x0 (real-part pos)) (- y0 (imag-part pos)) width height
                 (geo:line:visualizer-dots self)
                 (geo-select-stroke-paint* alt-stroke)
                 (plot:lines-close? self))))))
