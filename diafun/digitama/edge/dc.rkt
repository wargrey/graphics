#lang typed/racket/base

(provide (all-defined-out))

(require "tip.rkt")
(require "style.rkt")
(require "label.rkt")
(require "metrics.rkt")

(require "../unsafe/edge.rkt")

(require racket/math)

(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/paint)
(require geofun/digitama/dc/composite)

(require geofun/digitama/convert)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/geometry/dot)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/geometry/computation/line)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:edge geo
  ([footprints : Geo-Path-Clean-Prints]
   [source : (Pairof Float-Complex Flonum)]
   [target : (Pairof Float-Complex Flonum)]
   [origin : Float-Complex]
   [bbox-offset : Float-Complex]
   [source-shape : Geo-Path-Prints]
   [target-shape : Geo-Path-Prints]
   [adjust-offset : (Pairof Float-Complex Float-Complex)])
  #:type-name Dia:Edge
  #:transparent)

(struct dia:labeled-edge geo:group ()
  #:type-name Dia:Labeled-Edge
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge : (->* (Geo-Path-Clean-Prints+)
                        (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:source-shape (Option Dia-Edge-Tip-Shape) #:target-shape (Option Dia-Edge-Tip-Shape))
                         Dia:Edge)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:source-shape [src-shape #false] #:target-shape [tgt-shape #false] footprints]
    (define thickness : Nonnegative-Flonum (let ([s (dia-edge-select-line-paint stroke)]) (if (stroke? s) (stroke-width s) 0.0)))
    (define-values (spt srad ept erad) (geo-path-end-points footprints))
    (define-values (e.x e.y e.w e.h) (geo-path-ink-box footprints))

    ;; NOTE: we move the end shapes to their absolute positions, and no need to translate them when drawing
    (define-values (src-prints s.x s.y s.w s.h s.off s.fill?) (dia-edge-tip-metrics src-shape thickness srad spt #false))
    (define-values (tgt-prints t.x t.y t.w t.h t.off t.fill?) (dia-edge-tip-metrics tgt-shape thickness erad ept #true))

    (define-values (lx ty) (values (min e.x s.x t.x) (min e.y s.y t.y)))
    (define-values (rx by) (values (max (+ e.x e.w) (+ s.x s.w) (+ t.x t.w)) (max (+ e.y e.h) (+ s.y s.h) (+ t.y t.h))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))

    (create-geometry-object dia:edge
                            #:with [id (dia-draw-edge! stroke s.fill? t.fill?)
                                       (geo-shape-extent width height 0.0 0.0)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            footprints (cons spt srad) (cons ept erad)
                            (make-rectangular lx ty) (make-rectangular xoff yoff)
                            src-prints tgt-prints (cons s.off t.off))))

(define dia-edge-attach-label : (-> (U Dia:Edge Dia:Labeled-Edge) (U Dia-Edge-Label (Listof Dia-Edge-Label)) (U Dia:Edge Dia:Labeled-Edge))
  (lambda [self label]
    (cond [(null? label) self]
          [(dia:edge? self)
           (let-values ([(w h) (geo-flsize self)])
             (dia-edge-attach-label (create-geometry-group dia:labeled-edge #false #false #false (geo-own-layers self)) label))]
          [else (let* ([edge (dia-edge-unlabel self)]
                       [O (dia:edge-origin edge)])
                  (let attach ([labels : (Listof Dia-Edge-Label) (if (list? label) label (list label))]
                               [layers : (Listof (GLayerof Geo)) null]
                               [op : (Option Symbol) 'source])
                    (if (pair? labels)
                        (let* ([label (car labels)]
                               [dir (dia-edge-label-dir label)]
                               [distance (dia-edge-label-distance label)])

                          (attach (cdr labels)
                                  (cons (geo-sticker->layer (dia-edge-label-sticker label)
                                                            (geo-perpendicular-point (- (dia-edge-label-pos label) O) dir
                                                                                     distance (dia-edge-label-t label)))
                                        layers)
                                  (if (and distance (zero? distance)) op #false)))
                        (create-geometry-group dia:labeled-edge #false #false op
                                               (geo-path-layers-merge (geo:group-selves self) layers)))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-self-pin-position : (->* ((U Dia:Edge Dia:Labeled-Edge)) ((Option Float-Complex)) Float-Complex)
  (lambda [self [maybe-pt #false]]
    (if (dia:edge? self)
        (let* ([S (car (dia:edge-source self))]
               [O (dia:edge-origin self)]
               [->S (- S O)])
          (- (or maybe-pt S)
             (make-rectangular (abs (real-part ->S)) (abs (imag-part ->S)))))
        (let ([slayer (car (glayer-group-layers (geo:group-selves self)))])
          (- (dia-edge-self-pin-position (assert (glayer-master slayer) dia:edge?))
             (make-rectangular (glayer-x slayer) (glayer-y slayer)))))))

(define dia-edge-unlabel : (-> Dia:Labeled-Edge Dia:Edge)
  (lambda [g]
    (assert (glayer-master (car (glayer-group-layers (geo:group-selves g)))) dia:edge?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-draw-edge! : (-> Maybe-Stroke-Paint Boolean Boolean Geo-Surface-Draw!)
  (lambda [alt-stroke sfill? tfill?]
    (λ [self cr x0 y0 width height]
      (with-asserts ([self dia:edge?])
        (define paint (geo-select-stroke-paint alt-stroke))
        (define color (and paint (stroke-color paint)))
        (define (shape-stroke [fill? : Boolean]) : (Option Stroke)
          (if (not fill?)
              (and paint (desc-stroke paint #:color color #:dash 'solid))
              (desc-stroke #:width 1.0 #:color color)))

        (dc_edge cr x0 y0 width height
                 (dia:edge-footprints self) (dia:edge-bbox-offset self) paint
                 (vector-immutable (dia:edge-source-shape self) (shape-stroke sfill?) (and sfill? color))
                 (vector-immutable (dia:edge-target-shape self) (shape-stroke tfill?) (and tfill? color))
                 (dia:edge-adjust-offset self))))))
