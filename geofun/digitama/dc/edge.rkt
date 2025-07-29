#lang typed/racket/base

(provide (all-defined-out))

(require "../edge/label.rkt")
(require "../edge/tip/self.rkt")
(require "../edge/tip.rkt")
(require "../edge/tips.rkt")

(require "../../paint.rkt")
(require "../../color.rkt")
(require "../../stroke.rkt")

(require "../base.rkt")
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
(define geo-edge*
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:source-tip [src-tip : (Option Geo-Tip) #false]
           #:target-tip [tgt-tip : (Option Geo-Tip) #false]
           #:tip-color [tip-clr : (Option Color) #false]
           #:source-color [src-clr : (Option Color) #false]
           #:target-color [tgt-clr : (Option Color) #false]
           #:tip-placement [tip-plm : Geo-Tip-Placement 'center]
           #:source-placement [src-plm : (Option Geo-Tip-Placement) #false]
           #:target-placement [tgt-plm : (Option Geo-Tip-Placement) #false]
           [footprints0 : Geo-Path-Clean-Prints]] : Geo:Edge
    (define footprints : (Pairof GPath:Print Geo-Path-Clean-Prints) (if (pair? footprints0) footprints0 (list the-M0)))
    (define-values (spt srad ept erad) (geo-path-endpoint-vectors footprints))
    (define-values (ik.x ik.y ik.w ik.h) (geo-path-ink-box footprints))
    
    (define thickness : Nonnegative-Flonum (stroke-width (if (stroke? stroke) stroke (default-stroke))))
    (define-values (src-shape s.x0 s.y0 s.w s.h s.off s.cfg) (geo-tip-path (geo-tip-filter src-tip) thickness srad #false (or src-plm tip-plm)))
    (define-values (tgt-shape t.x0 t.y0 t.w t.h t.off t.cfg) (geo-tip-path (geo-tip-filter tgt-tip) thickness erad  #true (or tgt-plm tip-plm)))
    (define-values (s.x s.y) (values (+ (real-part spt) (real-part s.off) s.x0) (+ (imag-part spt) (imag-part s.off) s.y0)))
    (define-values (t.x t.y) (values (+ (real-part ept) (real-part t.off) t.x0) (+ (imag-part ept) (imag-part t.off) t.y0)))
    
    (define-values (lx ty) (values (min ik.x s.x t.x) (min ik.y s.y t.y)))
    (define-values (rx by) (values (max (+ ik.x ik.w) (+ s.x s.w) (+ t.x t.w)) (max (+ ik.y ik.h) (+ s.y s.h) (+ t.y t.h))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))

    (create-geometry-object geo:edge
                            #:with [id (geo-draw-edge! stroke (or src-clr tip-clr) (or tgt-clr tip-clr) s.cfg t.cfg)
                                       (geo-shape-extent width height 0.0 0.0)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            footprints (cons spt srad) (cons ept erad)
                            (make-rectangular lx ty) (make-rectangular xoff yoff)
                            src-shape tgt-shape (cons s.off t.off))))

(define geo-edge-attach-label : (-> (U Geo:Edge Geo:Labeled-Edge) (U Geo-Edge-Label (Listof Geo-Edge-Label)) (U Geo:Edge Geo:Labeled-Edge))
  (lambda [self label]
    (cond [(null? label) self]
          [(geo:edge? self)
           (let-values ([(w h) (geo-flsize self)])
             (geo-edge-attach-label (create-geometry-group geo:labeled-edge #false #false #false (geo-own-layers self)) label))]
          [else (let* ([edge (geo-edge-unlabel self)]
                       [O (geo:edge-origin edge)])
                  (define labels-vectors : (HashTable Any (Option (Pairof Float-Complex Float-Complex))) (make-hash))
                  (define (this-label-vector [idx : Integer] [t : Flonum]) : (Option (Pairof Float-Complex Float-Complex))
                    (hash-ref! labels-vectors (cons idx t)
                               (λ [] (geo-path-directional-vector (geo:edge-footprints edge) idx t))))
                  (let attach ([labels : (Listof Geo-Edge-Label) (if (list? label) label (list label))]
                               [layers : (Listof (GLayerof Geo)) null]
                               [op : (Option Symbol) 'source])
                    (if (pair? labels)
                        (let* ([label (car labels)]
                               [V (this-label-vector (geo-edge-label-idx label) (geo-edge-label-t label))])
                          (if (and V)
                              (let-values ([(layer distance) (geo-edge-label-layer+distance label (- (car V) O) (cdr V))])
                                (attach (cdr labels) (cons layer layers) (if (zero? distance) op #false)))
                              (attach labels layers op)))
                        (create-geometry-group geo:labeled-edge #false #false op
                                               (geo-path-layers-merge (geo:group-selves self) layers)))))])))

(define geo-edge
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:source-tip [src-tip : (Option Geo-Tip) #false]
           #:target-tip [tgt-tip : (Option Geo-Tip) #false]
           #:tip-color [tip-clr : (Option Color) #false]
           #:source-color [src-clr : (Option Color) #false]
           #:target-color [tgt-clr : (Option Color) #false]
           #:tip-placement [tip-plm : Geo-Tip-Placement 'center]
           #:source-placement [src-plm : (Option Geo-Tip-Placement) #false]
           #:target-placement [tgt-plm : (Option Geo-Tip-Placement) #false]
           #:bezier-samples [samples : Index (default-bezier-samples)]
           #:scale [scale : Point2D 1.0]
           #:offset [offset : Float-Complex 0.0+0.0i]
           #:labels [labels : (U Geo-Edge-Label (Listof Geo-Edge-Label) False) #false]
           [segments : (Listof PolyCurve2D)]] : (U Geo:Edge Geo:Labeled-Edge)
    (define me : Geo:Edge
      (geo-edge* #:id id #:stroke stroke #:source-tip src-tip #:target-tip tgt-tip
                 #:tip-color tip-clr #:source-color src-clr #:target-color tgt-clr
                 #:tip-placement tip-plm #:source-placement src-plm #:target-placement tgt-plm
                 (let-values ([(curves lx ty rx by) (~polycurves segments offset scale)])
                   (geo-path-cleanse curves #:bezier-samples samples))))

    (cond [(or (not labels) (null? labels)) me]
          [else (geo-edge-attach-label me labels)])))

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
(define geo-edge-self-pin-layer : (->* ((U Geo:Edge Geo:Labeled-Edge)) (Float-Complex) (GLayerof Geo))
  (lambda [self [offset 0.0+0.0i]]
    (define ppos : Float-Complex (+ (geo-edge-self-pin-position self #false) offset))
    (define-values (width height) (geo-flsize self))

    (glayer self (real-part ppos) (imag-part ppos) width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-edge! : (-> Maybe-Stroke-Paint (Option Color) (Option Color) Geo-Tip-Config Geo-Tip-Config Geo-Surface-Draw!)
  (lambda [alt-stroke alt-sclr alt-tclr scfg tcfg]
    (define alt-srgba (and alt-sclr (rgb* alt-sclr)))
    (define alt-trgba (and alt-tclr (rgb* alt-tclr)))
    
    (λ [self cr x0 y0 width height]
      (with-asserts ([self geo:edge?])
        (define paint (geo-select-stroke-paint alt-stroke))
        (define color (and paint (stroke-color paint)))
        (define-values (sclr tclr) (values (or alt-srgba color) (or alt-trgba color)))
        (define (tip-stroke [cfg : Geo-Tip-Config] [clr : (Option FlRGBA)]) : (Option Stroke)
          (if (geo-tip-config-fill? cfg)
              (desc-stroke #:width 1.0 #:color clr)
              (and paint (desc-stroke paint #:color clr #:dash 'solid #:width (geo-tip-config-thickness cfg)))))

        (dc_edge cr x0 y0 width height
                 (geo:edge-footprints self) (geo:edge-bbox-offset self) paint
                 (geo:edge-source-tip self) (tip-stroke scfg sclr) (and (geo-tip-config-fill? scfg) sclr) (car (geo:edge-source self))
                 (geo:edge-target-tip self) (tip-stroke tcfg tclr) (and (geo-tip-config-fill? tcfg) tclr) (car (geo:edge-target self))
                 (geo:edge-adjust-offset self))))))
