#lang typed/racket/base

(provide (all-defined-out))

(require "plain.rkt")

(require "../edge/label.rkt")
(require "../edge/marker/self.rkt")
(require "../edge/metrics.rkt")

(require "../../paint.rkt")
(require "../../stroke.rkt")

(require "../paint.rkt")
(require "../paint/self.rkt")
(require "../dc/composite.rkt")

(require "../convert.rkt")
(require "../layer/type.rkt")
(require "../layer/combine.rkt")
(require "../layer/sticker.rkt")
(require "../geometry/dot.rkt")
(require "../geometry/footprint.rkt")
(require "../geometry/computation/line.rkt")
(require "../unsafe/dc/edge.rkt")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:edge geo
  ([footprints : Geo-Path-Clean-Prints+]
   [source : (Pairof Float-Complex Flonum)]
   [target : (Pairof Float-Complex Flonum)]
   [origin : Float-Complex]
   [bbox-offset : Float-Complex]
   [source-marker : Geo-Path-Prints]
   [target-marker : Geo-Path-Prints]
   [adjust-offset : (Pairof Float-Complex Float-Complex)])
  #:type-name Geo:Edge
  #:transparent)

(struct geo:labeled-edge geo:group ()
  #:type-name Geo:Labeled-Edge
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge : (->* ((Listof Point2D))
                        (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:source-marker (Option Geo-Marker) #:target-marker (Option Geo-Marker)
                         #:marker-placement Geo-Marker-Placement #:source-marker-placement (Option Geo-Marker-Placement) #:target-marker-placement (Option Geo-Marker-Placement))
                        (U Geo:Edge Geo:Blank))
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:source-marker [src-mkr #false] #:target-marker [tgt-mkr #false]
           #:marker-placement [mkr-pos 'center] #:source-marker-placement [src-pos #false] #:target-marker-placement [tgt-pos #false]
           dots]
    (define footprints (geo-path-cleanse (map ~point2d dots)))
    (if (and (pair? footprints) (pair? (cdr footprints)))
        (geo-edge* #:id id #:stroke stroke #:source-marker src-mkr #:target-marker tgt-mkr
                   #:marker-placement mkr-pos #:source-marker-placement src-pos #:target-marker-placement tgt-pos
                   footprints)
        (geo-blank #:id id))))

(define geo-edge* : (->* (Geo-Path-Clean-Prints+)
                         (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:source-marker (Option Geo-Marker) #:target-marker (Option Geo-Marker)
                          #:marker-placement Geo-Marker-Placement #:source-marker-placement (Option Geo-Marker-Placement) #:target-marker-placement (Option Geo-Marker-Placement))
                         Geo:Edge)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:source-marker [src-mkr #false] #:target-marker [tgt-mkr #false]
           #:marker-placement [mkr-pos 'center] #:source-marker-placement [src-pos #false] #:target-marker-placement [tgt-pos #false]
           footprints]
    (define-values (spt srad ept erad) (geo-path-end-points footprints))
    (define-values (e.x e.y e.w e.h) (geo-path-ink-box footprints))
    
    ;; NOTE: we move the end shapes to their absolute positions, and no need to translate them when drawing
    (define thickness : Nonnegative-Flonum (stroke-width (if (stroke? stroke) stroke (default-stroke))))
    (define-values (src-prints s.x s.y s.w s.h s.off s.fill?) (geo-edge-marker-metrics src-mkr thickness srad spt #false (or src-pos mkr-pos)))
    (define-values (tgt-prints t.x t.y t.w t.h t.off t.fill?) (geo-edge-marker-metrics tgt-mkr thickness erad ept #true  (or tgt-pos mkr-pos)))
    
    (define-values (lx ty) (values (min e.x s.x t.x) (min e.y s.y t.y)))
    (define-values (rx by) (values (max (+ e.x e.w) (+ s.x s.w) (+ t.x t.w)) (max (+ e.y e.h) (+ s.y s.h) (+ t.y t.h))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))
    
    (create-geometry-object geo:edge
                            #:with [id (geo-draw-edge! stroke s.fill? t.fill?)
                                       (geo-shape-extent width height 0.0 0.0)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            footprints (cons spt srad) (cons ept erad)
                            (make-rectangular lx ty) (make-rectangular xoff yoff)
                            src-prints tgt-prints (cons s.off t.off))))

(define geo-edge-attach-label : (-> (U Geo:Edge Geo:Labeled-Edge) (U Geo-Edge-Label (Listof Geo-Edge-Label)) (U Geo:Edge Geo:Labeled-Edge))
  (lambda [self label]
    (cond [(null? label) self]
          [(geo:edge? self)
           (let-values ([(w h) (geo-flsize self)])
             (geo-edge-attach-label (create-geometry-group geo:labeled-edge #false #false #false (geo-own-layers self)) label))]
          [else (let* ([edge (geo-edge-unlabel self)]
                       [O (geo:edge-origin edge)])
                  (let attach ([labels : (Listof Geo-Edge-Label) (if (list? label) label (list label))]
                               [layers : (Listof (GLayerof Geo)) null]
                               [op : (Option Symbol) 'source])
                    (if (pair? labels)
                        (let* ([label (car labels)]
                               [dir (geo-edge-label-dir label)]
                               [distance (geo-edge-label-distance label)])

                          (attach (cdr labels)
                                  (cons (geo-sticker->layer (geo-edge-label-sticker label)
                                                            (geo-perpendicular-point (- (geo-edge-label-pos label) O) dir
                                                                                     distance (geo-edge-label-t label)))
                                        layers)
                                  (if (and distance (zero? distance)) op #false)))
                        (create-geometry-group geo:labeled-edge #false #false op
                                               (geo-path-layers-merge (geo:group-selves self) layers)))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-self-pin-position : (->* ((U Geo:Edge Geo:Labeled-Edge)) ((Option Float-Complex)) Float-Complex)
  (lambda [self [maybe-pt #false]]
    (if (geo:edge? self)
        (let* ([S (car (geo:edge-source self))]
               [O (geo:edge-origin self)]
               [->S (- S O)])
          (- (or maybe-pt S)
             (make-rectangular (abs (real-part ->S)) (abs (imag-part ->S)))))
        (let ([slayer (car (glayer-group-layers (geo:group-selves self)))])
          (- (geo-edge-self-pin-position (assert (glayer-master slayer) geo:edge?))
             (make-rectangular (glayer-x slayer) (glayer-y slayer)))))))

(define geo-edge-endpoints : (-> (U Geo:Edge Geo:Labeled-Edge) (Values Float-Complex Float-Complex))
  (lambda [self]
    (if (geo:edge? self)
        (values (car (geo:edge-source self))
                (car (geo:edge-target self)))
        (geo-edge-endpoints (geo-edge-unlabel self)))))

(define geo-edge-endpoint-offsets : (-> (U Geo:Edge Geo:Labeled-Edge) (Values Float-Complex Float-Complex))
  (lambda [self]
    (if (geo:edge? self)
        (values (car (geo:edge-adjust-offset self))
                (cdr (geo:edge-adjust-offset self)))
        (geo-edge-endpoints (geo-edge-unlabel self)))))

(define geo-edge-unlabel : (-> Geo:Labeled-Edge Geo:Edge)
  (lambda [g]
    (assert (glayer-master (car (glayer-group-layers (geo:group-selves g)))) geo:edge?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-edge! : (-> Maybe-Stroke-Paint Boolean Boolean Geo-Surface-Draw!)
  (lambda [alt-stroke sfill? tfill?]
    (λ [self cr x0 y0 width height]
      (with-asserts ([self geo:edge?])
        (define paint (geo-select-stroke-paint alt-stroke))
        (define color (and paint (stroke-color paint)))
        (define (shape-stroke [fill? : Boolean]) : (Option Stroke)
          (if (not fill?)
              (and paint (desc-stroke paint #:color color #:dash 'solid))
              (desc-stroke #:width 1.0 #:color color)))

        (dc_edge cr x0 y0 width height
                 (geo:edge-footprints self) (geo:edge-bbox-offset self) paint
                 (geo:edge-source-marker self) (shape-stroke sfill?) (and sfill? color)
                 (geo:edge-target-marker self) (shape-stroke tfill?) (and tfill? color)
                 (geo:edge-adjust-offset self))))))
