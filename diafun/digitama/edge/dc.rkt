#lang typed/racket/base

(provide (all-defined-out))

(require "tip.rkt")
(require "style.rkt")
(require "metrics.rkt")

(require "../unsafe/edge.rkt")

(require geofun/paint)
(require geofun/stroke)
(require geofun/font)

(require geofun/digitama/dc/paint)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)

(require geofun/digitama/convert)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/geometry/dot)
(require geofun/digitama/geometry/footprint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-edge-label
  ([pt : Float-Complex]
   [v : Float-Complex]
   [sticker : Geo]
   [t : Flonum])
  #:type-name Dia-Edge-Label
  #:constructor-name unsafe-dia-edge-label
  #:transparent)

(define make-dia-edge-label : (->* (Float-Complex Float-Complex Geo) (Flonum) Dia-Edge-Label)
  (lambda [start end label [t 0.5]]
    (unsafe-dia-edge-label start end label t)))

(define make-dia-edge-labels : (->* (Float-Complex Float-Complex (U String (Pairof (Option String) (Option String))))
                                    (Flonum #:font (Option Font) #:font-paint Option-Fill-Paint)
                                    (Listof Dia-Edge-Label))
  (lambda [start end label [head-position 0.2] #:font [font #false] #:font-paint [font-paint #false]]
    (define v : Float-Complex (- end start))
    (if (pair? label)
        
        (let-values ([(head tail) (values (car label) (cdr label))])
          (cond [(and head tail)
                 (list (unsafe-dia-edge-label start v (geo-text head font #:color font-paint) head-position)
                       (unsafe-dia-edge-label start v (geo-text tail font #:color font-paint) (- 1.0 head-position)))]
                [(and head) (list (unsafe-dia-edge-label start v (geo-text head font #:color font-paint) head-position))]
                [(and tail) (list (unsafe-dia-edge-label start v (geo-text tail font #:color font-paint) (- 1.0 head-position)))]
                [else null]))

        (list (unsafe-dia-edge-label start v (geo-text label font #:color font-paint) 0.5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:edge geo
  ([footprints : Geo-Path-Clean-Prints]
   [source : (Pairof Float-Complex Flonum)]
   [target : (Pairof Float-Complex Flonum)]
   [origin : Float-Complex]
   [bbox-offset : Float-Complex]
   [line-offset : (Option Float-Complex)]
   [source-shape : (Listof Geo-Path-Clean-Print)]
   [target-shape : (Listof Geo-Path-Clean-Print)])
  #:type-name Dia:Edge
  #:transparent)

(struct dia:labeled-edge geo:group ()
  #:type-name Dia:Labeled-Edge
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge : (->* (Geo-Path-Clean-Prints)
                        (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:source-shape (Option Dia-Edge-Tip-Shape) #:target-shape (Option Dia-Edge-Tip-Shape))
                         Dia:Edge)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:source-shape [src-shape #false] #:target-shape [tgt-shape #false] footprints]
    (define thickness : Nonnegative-Flonum (let ([s (dia-edge-select-line-paint stroke)]) (if (stroke? s) (stroke-width s) 0.0)))
    (define line-offset : Nonnegative-Flonum (* thickness 0.5))
    (define-values (spt srad ept erad) (geo-path-end-points footprints))
    (define-values (e.x e.y e.w e.h) (geo-path-ink-box footprints))

    ;; NOTE: we move the end shapes to their absolute positions, and no need to translate them when drawing
    (define-values (src-prints s.x s.y s.w s.h) (dia-edge-tip-metrics src-shape thickness srad spt))
    (define-values (tgt-prints t.x t.y t.w t.h) (dia-edge-tip-metrics tgt-shape thickness erad ept))

    ;; NOTE: for shapes having outline stroke, simply add the thickness here
    (define ssoffset : Nonnegative-Flonum (* (stroke-width default-dia-shape-stroke) 0.5))
    (define-values (lx ty) (values (min e.x (- s.x ssoffset) (- t.x ssoffset)) (min e.y (- s.y ssoffset) (- t.y ssoffset))))
    (define-values (rx by) (values (max (+ e.x e.w) (+ s.x s.w ssoffset) (+ t.x t.w ssoffset)) (max (+ e.y e.h) (+ s.y s.h ssoffset) (+ t.y t.h ssoffset))))
    (define-values (xoff yoff width height x-stroke? y-stroke?) (point2d->window +nan.0+nan.0i lx ty rx by))

    (create-geometry-object dia:edge
                            #:surface (dia-edge-surface width height x-stroke? y-stroke?) stroke
                            #:extent (geo-stroke-extent-wrapper (geo-shape-plain-extent width height 0.0 0.0) stroke x-stroke? y-stroke?)
                            #:id id
                            footprints (cons spt srad) (cons ept erad) (make-rectangular lx ty)
                            (make-rectangular xoff yoff) (make-rectangular line-offset line-offset)
                            src-prints tgt-prints)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-self-pin-position : (->* ((U Dia:Edge Dia:Labeled-Edge)) ((Option Float-Complex)) Float-Complex)
  (lambda [self [maybe-pt #false]]
    (if (dia:edge? self)
        (let* ([S (car (dia:edge-source self))]
               [O (dia:edge-origin self)]
               [->S (- S O)])
          (- (or maybe-pt S)
             (make-rectangular (abs (real-part ->S)) (abs (imag-part ->S)))
             (or (dia:edge-line-offset self) 0.0+0.0i)))
        (let ([slayer (car (vector-ref (geo:group-layers self) 2))])
          (- (dia-edge-self-pin-position (assert (vector-ref slayer 0) dia:edge?))
             (make-rectangular (vector-ref slayer 1) (vector-ref slayer 2)))))))

(define dia-edge-attach-label : (-> (U Dia:Edge Dia:Labeled-Edge) (U Dia-Edge-Label (Listof Dia-Edge-Label)) (U Dia:Edge Dia:Labeled-Edge))
  (lambda [self label]
    (cond [(null? label) self]
          [(dia:edge? self)
           (let-values ([(w h) (geo-flsize self)])
             (dia-edge-attach-label (create-geometry-group dia:labeled-edge #false (geo-own-layers self)) label))]
          [else (let* ([edge (dia-edge-unlabel self)]
                       [-O (- (dia:edge-origin edge))])
                  (let attach ([labels : (Listof Dia-Edge-Label) (if (list? label) label (list label))]
                               [layers : (Listof (GLayerof Geo)) null])
                    (if (pair? labels)
                        (let ([label (car labels)])
                          (attach (cdr labels)
                                  (cons (geo-sticker->layer (dia-edge-label-sticker label)
                                                            (+ (dia-edge-label-pt label)
                                                               (* (dia-edge-label-v label)
                                                                  (dia-edge-label-t label))
                                                               -O))
                                        layers)))
                        (create-geometry-group dia:labeled-edge #false (geo-path-layers-merge (geo:group-layers self) layers)))))])))

(define dia-edge-unlabel : (-> Dia:Labeled-Edge Dia:Edge)
  (lambda [g]
    (assert (vector-ref (car (vector-ref (geo:group-layers g) 2)) 0) dia:edge?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Boolean Boolean Geo-Surface-Create)
  (lambda [width height x-stroke? y-stroke?]
    (λ [self]
      (with-asserts ([self dia:edge?])
        (define paint (current-stroke-source))
        (define color (and paint (stroke-color paint)))
        (define shape-stroke (desc-stroke default-dia-shape-stroke #:color color))
        (define offset (dia:edge-bbox-offset self))

        (dc_edge create-abstract-surface
                 width height (dia:edge-footprints self) (real-part offset) (imag-part offset) x-stroke? y-stroke? paint
                 (vector-immutable (dia:edge-source-shape self) shape-stroke color)
                 (vector-immutable (dia:edge-target-shape self) shape-stroke color)
                 (default-geometry-density))))))

(define edge
  (dia-edge (list (cons #\M 100.0+0.0i)
                  (cons #\L 100.0+100.0i)
                  (cons #\L 50.0+100.0i)
                  (cons #\L 50.0+50.0i))))

(dia-edge-attach-label edge
                       (append (make-dia-edge-labels 50.0+100.0i 50.0+50.0i "Yes")
                               (make-dia-edge-labels 100.0+0.0i 100.0+100.0i (cons "0" "1"))))
