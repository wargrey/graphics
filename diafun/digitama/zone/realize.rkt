#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require digimon/function)

(require geofun/constructor)
(require geofun/font)

(require geofun/digitama/self)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/position)

(require geofun/digitama/path/dc)
(require geofun/digitama/track/self)
(require geofun/digitama/track/anchor)

(require "../block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flex-zone-realize : (-> Geo:Track:Zone:Flex (HashTable Geo-Anchor-Name Float-Complex) (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block)))
                                    (Listof (GLayerof Geo-Path)) (Option Nonnegative-Flonum) 
                                    (Option (GLayerof Geo)))
  (lambda [self positions blockdb tracks opacity]
    (let resolve-zone-boundary ([lt : Flonum +inf.0]
                                [ty : Flonum +inf.0]
                                [rx : Flonum -inf.0]
                                [by : Flonum -inf.0]
                                [anchors : (Listof Geo-Anchor-Name) (geo:track:zone:flex-anchors self)])
      (if (pair? anchors)
          (let*-values ([(anchor rest) (values (car anchors) (cdr anchors))]
                        [(rect) (dia-anchor->boundary anchor blockdb positions)])
            (cond [(not rect) (resolve-zone-boundary lt ty rx by rest)]
                  [else (let-values ([(x y) (values (vector-ref rect 0) (vector-ref rect 1))]
                                     [(w h) (values (vector-ref rect 2) (vector-ref rect 3))])
                          (resolve-zone-boundary (min x lt) (min y ty)
                                              (max rx (+ x w)) (max by (+ y h))
                                              rest))]))
          (geo-sticker->layer #:default-anchor 'lt
                              (geo-rectangle (- rx lt) (- by ty))
                              (make-rectangular lt ty))))))

(define dia-fixed-zone-realize : (-> Geo:Track:Zone:Fixed (HashTable Geo-Anchor-Name Float-Complex) (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block)))
                                     (Listof (GLayerof Geo-Path)) (Option Nonnegative-Flonum) 
                                     (Option (GLayerof Geo)))
  (lambda [self positions blockdb tracks opacity]
    #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-anchor->boundary : (-> Geo-Anchor-Name (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block))) (HashTable Geo-Anchor-Name Float-Complex)
                                   (Option (Immutable-Vector Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum)))
  (lambda [anchor blockdb positions]
    (define block (hash-ref blockdb anchor λfalse))

    (if (not block)
        (let ([pos (hash-ref positions anchor λfalse)])
          (and pos
               (vector-immutable (real-part pos) (imag-part pos) 0.0 0.0)))
        (let-values ([(x y) (geo-layer-position-values block)]
                     [(w h) (geo-layer-size block)])
          (vector-immutable x y w h)))))
