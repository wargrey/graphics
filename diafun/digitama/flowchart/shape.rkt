#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/digitama/self)
(require geofun/digitama/convert)
(require geofun/digitama/paint)

(require "../unsafe/flowchart.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:flow:storage geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [aradius : Nonnegative-Flonum])
  #:type-name Dia:Flow:Storage
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-sort-vertices : (-> Nonnegative-Flonum Nonnegative-Flonum (Listof Float-Complex))
  (lambda [width height]
     (define w/2 : Nonnegative-Flonum (* width 0.5))
     (define h/2 : Nonnegative-Flonum (* height 0.5))
     
     (list (make-rectangular 0.0 h/2)
           (make-rectangular w/2 0.0) (make-rectangular width h/2)
           (make-rectangular w/2 height) (make-rectangular 0.0 h/2)
           (make-rectangular width h/2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flow-storage : (->* (Real-Length Length+%)
                                (Length+% #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                                Dia:Flow:Storage)
  (lambda [width height [aradius (&% 38.4)] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    (define fla : Nonnegative-Flonum (~dimension aradius (* flheight 0.5)))
    
    (create-geometry-object dia:flow:storage
                            #:with [id (dia-flow-draw-storage stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight fla)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flow-draw-storage : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (dia:flow:storage? self)
        (dc_general_storage cr x0 y0 width height (dia:flow:storage-aradius self)
                            (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))
