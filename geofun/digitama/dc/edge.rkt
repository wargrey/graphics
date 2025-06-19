#lang typed/racket/base

(provide (all-defined-out))

(require "../edge/label.rkt")
(require "../edge/tip/self.rkt")
(require "../edge/tip.rkt")
(require "../edge/tips.rkt")

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
(require "../unsafe/dc/edge.rkt")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:edge geo
  ([footprints : Geo-Path-Clean-Prints+]
   [source : (Pairof Float-Complex Flonum)]
   [target : (Pairof Float-Complex Flonum)]
   [origin : Float-Complex]
   [bbox-offset : Float-Complex]
   [source-tip : Geo-Path-Prints]
   [target-tip : Geo-Path-Prints]
   [adjust-offset : (Pairof Float-Complex Float-Complex)])
  #:type-name Geo:Edge
  #:transparent)

(struct geo:labeled-edge geo:group ()
  #:type-name Geo:Labeled-Edge
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge : (->* ((Listof Point2D))
                        (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:source-tip (Option Geo-Tip) #:target-tip (Option Geo-Tip)
                         #:tip-placement Geo-Tip-Placement #:source-tip-placement (Option Geo-Tip-Placement) #:target-tip-placement (Option Geo-Tip-Placement))
                        Geo:Edge)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:source-tip [src-tip #false] #:target-tip [tgt-tip #false]
           #:tip-placement [tip-pos 'center] #:source-tip-placement [src-pos #false] #:target-tip-placement [tgt-pos #false]
           dots]
    (geo-edge* #:id id #:stroke stroke #:source-tip src-tip #:target-tip tgt-tip
               #:tip-placement tip-pos #:source-tip-placement src-pos #:target-tip-placement tgt-pos
               (geo-path-cleanse (map ~point2d dots)))))

(define geo-edge* : (->* (Geo-Path-Clean-Prints)
                         (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:source-tip (Option Geo-Tip) #:target-tip (Option Geo-Tip)
                          #:tip-placement Geo-Tip-Placement #:source-tip-placement (Option Geo-Tip-Placement) #:target-tip-placement (Option Geo-Tip-Placement))
                         Geo:Edge)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:source-tip [src-tip #false] #:target-tip [tgt-tip #false]
           #:tip-placement [tip-pos 'center] #:source-tip-placement [src-pos #false] #:target-tip-placement [tgt-pos #false]
           footprints0]
    (define footprints : (Pairof GPath:Print Geo-Path-Clean-Prints) (if (pair? footprints0) footprints0 (list the-M0)))
    (define-values (spt srad ept erad) (geo-path-end-points footprints))
    (define-values (e.x e.y e.w e.h) (geo-path-ink-box footprints))
    
    ;; NOTE: we move the end shapes to their absolute positions, and no need to translate them when drawing
    (define thickness : Nonnegative-Flonum (stroke-width (if (stroke? stroke) stroke (default-stroke))))
    (define-values (src-prints s.x0 s.y0 s.w s.h s.off s.cfg) (geo-tip-path (geo-tip-filter src-tip) thickness srad #false (or src-pos tip-pos)))
    (define-values (tgt-prints t.x0 t.y0 t.w t.h t.off t.cfg) (geo-tip-path (geo-tip-filter tgt-tip) thickness erad #true  (or tgt-pos tip-pos)))
    (define-values (s.x s.y) (values (+ (real-part spt) s.x0) (+ (imag-part spt) s.y0)))
    (define-values (t.x t.y) (values (+ (real-part ept) t.x0) (+ (imag-part ept) t.y0)))
    
    (define-values (lx ty) (values (min e.x s.x t.x) (min e.y s.y t.y)))
    (define-values (rx by) (values (max (+ e.x e.w) (+ s.x s.w) (+ t.x t.w)) (max (+ e.y e.h) (+ s.y s.h) (+ t.y t.h))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))
    
    (create-geometry-object geo:edge
                            #:with [id (geo-draw-edge! stroke s.cfg t.cfg)
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
                               [distance (geo-edge-label-distance label)])
                          (attach (cdr labels)
                                  (cons (geo-edge-label-layer label O) layers)
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
(define geo-draw-edge! : (-> Maybe-Stroke-Paint Geo-Tip-Config Geo-Tip-Config Geo-Surface-Draw!)
  (lambda [alt-stroke scfg tcfg]
    (λ [self cr x0 y0 width height]
      (with-asserts ([self geo:edge?])
        (define paint (geo-select-stroke-paint alt-stroke))
        (define color (and paint (stroke-color paint)))
        (define (shape-stroke [cfg : Geo-Tip-Config]) : (Option Stroke)
          (if (geo-tip-config-fill? cfg)
              (desc-stroke #:width 1.0 #:color color)
              (and paint (desc-stroke paint #:color color #:dash 'solid #:width (geo-tip-config-thickness cfg)))))

        (dc_edge cr x0 y0 width height
                 (geo:edge-footprints self) (geo:edge-bbox-offset self) paint
                 (geo:edge-source-tip self) (shape-stroke scfg) (and (geo-tip-config-fill? scfg) color) (car (geo:edge-source self))
                 (geo:edge-target-tip self) (shape-stroke tcfg) (and (geo-tip-config-fill? tcfg) color) (car (geo:edge-target self))
                 (geo:edge-adjust-offset self))))))
