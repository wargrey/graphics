#lang typed/racket/base

(provide (all-defined-out))

(require "../path/label.rkt")
(require "../path/tip/self.rkt")
(require "../path/tip.rkt")
(require "../path/tips.rkt")

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
(require "../layer/merge.rkt")
(require "../geometry/dot.rkt")
(require "../geometry/footprint.rkt")
(require "../unsafe/dc/edge.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path (U Geo:Path:Self Geo:Path))

(struct geo:path:self geo
  ([footprints : Geo-Path-Clean-Prints+]
   [source : (Pairof Float-Complex Flonum)]
   [target : (Pairof Float-Complex Flonum)]
   [origin : Float-Complex]
   [bbox-offset : Float-Complex]
   [source-tip : Geo-Path-Prints]
   [target-tip : Geo-Path-Prints]
   [tip-sizes : (Pairof Float-Complex Float-Complex)]
   [adjust-offset : (Pairof Float-Complex Float-Complex)])
  #:type-name Geo:Path:Self
  #:transparent)

(struct geo:path geo:group
  ([self : Geo:Path:Self])
  #:type-name Geo:Path
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path*
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
           [footprints0 : Geo-Path-Clean-Prints]] : Geo:Path:Self
    (define footprints : (Pairof GPath:Print Geo-Path-Clean-Prints) (if (pair? footprints0) footprints0 (list the-M0)))
    (define-values (spt srad ept erad) (gpp-endpoint-vectors footprints))
    (define-values (ik.x ik.y ik.w ik.h) (gpp-ink-box footprints))

    (define thickness : Nonnegative-Flonum (stroke-width (if (stroke? stroke) stroke (default-stroke))))
    (define-values (src-shape s.x0 s.y0 s.w s.h s.off s.cfg) (geo-tip-shape (geo-tip-filter src-tip) thickness srad #false (or src-plm tip-plm)))
    (define-values (tgt-shape t.x0 t.y0 t.w t.h t.off t.cfg) (geo-tip-shape (geo-tip-filter tgt-tip) thickness erad  #true (or tgt-plm tip-plm)))
    (define-values (s.x s.y) (values (+ (real-part spt) (real-part s.off) s.x0) (+ (imag-part spt) (imag-part s.off) s.y0)))
    (define-values (t.x t.y) (values (+ (real-part ept) (real-part t.off) t.x0) (+ (imag-part ept) (imag-part t.off) t.y0)))
    
    (define-values (lx ty) (values (min ik.x s.x t.x) (min ik.y s.y t.y)))
    (define-values (rx by) (values (max (+ ik.x ik.w) (+ s.x s.w) (+ t.x t.w)) (max (+ ik.y ik.h) (+ s.y s.h) (+ t.y t.h))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))

    (create-geometry-object geo:path:self
                            #:with [id (geo-draw-path-self! stroke (or src-clr tip-clr) (or tgt-clr tip-clr) s.cfg t.cfg)
                                       (geo-shape-extent width height 0.0 0.0)
                                       (geo-shape-outline stroke x-stroke? y-stroke?)]
                            footprints (cons spt srad) (cons ept erad)
                            (make-rectangular lx ty) (make-rectangular xoff yoff)
                            src-shape tgt-shape
                            (cons (make-rectangular s.w s.h) (make-rectangular t.w t.h))
                            (cons s.off t.off))))

(define geo-path-attach-label : (-> Geo-Path (U Geo:Path:Label (Listof Geo:Path:Label)) [#:id (Option Symbol)] Geo:Path)
  (let ([labels-vectors : (HashTable Any (Option (Pairof Float-Complex Float-Complex))) (make-weak-hash)])
    (lambda [master label #:id [name #false]]
      (cond [(geo:path:self? master)
             (geo-path-attach-label #:id name
                                    (create-geometry-group geo:path name #false #false
                                                           (geo-own-layers master) master)
                                    label)]
            [(or (pair? label) (geo:path:label? label))
             (let* ([self (geo-path-ungroup master)]
                    [footprints (geo:path:self-footprints self)]
                    [O (geo:path:self-origin self)])
               (define (this-label-vector [idx : (Option Integer)] [t : Flonum]) : (Option (Pairof Float-Complex Float-Complex))
                 (hash-ref! labels-vectors (list footprints idx t)
                            (λ [] (gpp-directional-vector footprints idx t))))
               (let attach ([labels : (Listof Geo:Path:Label) (if (list? label) label (list label))]
                            [layers : (Listof (GLayerof Geo)) null]
                            [op : (Option Symbol) 'source])
                 (if (pair? labels)
                     (let* ([label (car labels)]
                            [V (this-label-vector (geo:path:label-idx label) (geo:path:label-time label))])
                       (if (and V)
                           (let-values ([(layer distance) (geo-path-label-layer+distance label (- (car V) O) (cdr V))])
                             (attach (cdr labels) (cons layer layers) (if (zero? distance) op #false)))
                           (attach labels layers op)))
                     (create-geometry-group geo:path name #false op
                                            (geo-layers-merge (geo:group-selves master) layers)
                                            self))))]
            [else master]))))

(define geo-path
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
           #:labels [labels : (U Geo:Path:Label (Listof Geo:Path:Label) False) #false]
           [segments : (Listof PolyCurve2D)]] : (U Geo:Path:Self Geo:Path)
    (define me : Geo:Path:Self
      (geo-path* #:id id #:stroke stroke #:source-tip src-tip #:target-tip tgt-tip
                 #:tip-color tip-clr #:source-color src-clr #:target-color tgt-clr
                 #:tip-placement tip-plm #:source-placement src-plm #:target-placement tgt-plm
                 (let-values ([(curves lx ty rx by) (~polycurves segments offset scale)])
                   (gpp-cleanse curves #:bezier-samples samples))))

    (cond [(or (not labels) (null? labels)) me]
          [else (geo-path-attach-label me labels)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-self-pin-position : (->* (Geo-Path) ((Option Float-Complex)) Float-Complex)
  (lambda [master [maybe-pt #false]]
    (if (geo:path:self? master)
        (let* ([S (car (geo:path:self-source master))]
               [O (geo:path:self-origin master)]
               [->S (- S O)])
          (- (or maybe-pt S)
             (make-rectangular (abs (real-part ->S)) (abs (imag-part ->S)))))
        (let ([slayer (car (glayer-group-layers (geo:group-selves master)))])
          (- (geo-path-self-pin-position (geo:path-self master))
             (make-rectangular (glayer-x slayer) (glayer-y slayer)))))))

(define geo-path-endpoints : (-> Geo-Path (Values Float-Complex Float-Complex))
  (lambda [master]
    (if (geo:path:self? master)
        (values (car (geo:path:self-source master))
                (car (geo:path:self-target master)))
        (geo-path-endpoints (geo-path-ungroup master)))))

(define geo-path-endpoint-offsets : (-> Geo-Path (Values Float-Complex Float-Complex))
  (lambda [master]
    (if (geo:path:self? master)
        (values (car (geo:path:self-adjust-offset master))
                (cdr (geo:path:self-adjust-offset master)))
        (geo-path-endpoint-offsets (geo-path-ungroup master)))))

(define geo-path-tip-sizes : (-> Geo-Path (Values Float-Complex Float-Complex))
  (lambda [master]
    (if (geo:path:self? master)
        (values (car (geo:path:self-tip-sizes master))
                (cdr (geo:path:self-tip-sizes master)))
        (geo-path-tip-sizes (geo-path-ungroup master)))))

(define geo-path-length : (-> Geo-Path Nonnegative-Flonum)
  (lambda [master]
    (if (geo:path:self? master)
        (gpp-arclength (geo:path:self-footprints master))
        (geo-path-length (geo-path-ungroup master)))))

(define geo-path-tangent-vector : (-> Geo-Path (-> Real (Option Float-Complex)))
  (lambda [master]
    (if (geo:path:self? master)
        (λ [[t : Real]] : (Option Float-Complex)
          (define v (gpp-directional-vector (geo:path:self-footprints master) t))
          (and v (cdr v)))
        (geo-path-tangent-vector (geo-path-ungroup master)))))

(define geo-path-normal-vector : (-> Geo-Path (-> Real (Option Float-Complex)))
  (lambda [master]
    (if (geo:path:self? master)
        (λ [[t : Real]] : (Option Float-Complex)
          (define v (gpp-directional-vector (geo:path:self-footprints master) t))
          (and v (* (cdr v) 0.0+1.0i)))
        (geo-path-normal-vector (geo-path-ungroup master)))))

(define geo-path-ungroup : (-> Geo:Path Geo:Path:Self)
  (lambda [master]
    (geo:path-self master)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-self-pin-layer : (->* ((U Geo:Path Geo:Path:Self)) (Float-Complex) (GLayerof Geo))
  (lambda [master [offset 0.0+0.0i]]
    (define ppos : Float-Complex (+ (geo-path-self-pin-position master #false) offset))
    (define-values (width height) (geo-flsize master))

    (glayer master (real-part ppos) (imag-part ppos) width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-path-self! : (-> Maybe-Stroke-Paint (Option Color) (Option Color) Geo-Tip-Config Geo-Tip-Config Geo-Surface-Draw!)
  (lambda [alt-stroke alt-sclr alt-tclr scfg tcfg]
    (define alt-srgba (and alt-sclr (rgb* alt-sclr)))
    (define alt-trgba (and alt-tclr (rgb* alt-tclr)))
    
    (λ [self cr x0 y0 width height]
      (with-asserts ([self geo:path:self?])
        (define paint (geo-select-stroke-paint alt-stroke))
        (define color (and paint (stroke-color paint)))
        (define-values (sclr tclr) (values (or alt-srgba color) (or alt-trgba color)))

        (define (tip-stroke [cfg : Geo-Tip-Config] [clr : (Option FlRGBA)]) : (Option Stroke)
          (if (geo-tip-config-fill? cfg)
              (desc-stroke #:width 1.0 #:color clr)
              (and paint (desc-stroke paint #:color clr #:dash 'solid #:width (geo-tip-config-thickness cfg)))))

        (dc_edge cr x0 y0 width height
                 (geo:path:self-footprints self) (geo:path:self-bbox-offset self) paint
                 (geo:path:self-source-tip self) (tip-stroke scfg sclr) (and (geo-tip-config-fill? scfg) sclr) (car (geo:path:self-source self))
                 (geo:path:self-target-tip self) (tip-stroke tcfg tclr) (and (geo-tip-config-fill? tcfg) tclr) (car (geo:path:self-target self))
                 (geo:path:self-adjust-offset self))))))
