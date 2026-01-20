#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../self.rkt")
(require "../convert.rkt")

(require "../paint.rkt")
(require "../unsafe/dc/dingbat.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:stadium geo
  ([length : Nonnegative-Flonum]
   [radius : Nonnegative-Flonum])
  #:type-name Geo:Stadium
  #:transparent)

(struct geo:document geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [wave-height : Nonnegative-Flonum]
   [gapsize : Nonnegative-Flonum]
   [extra-n : Index])
  #:type-name Geo:Document
  #:transparent)

(struct geo:database geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [bradius : Nonnegative-Flonum]
   [gapsize : Nonnegative-Flonum]
   [extra-n : Index])
  #:type-name Geo:Database
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-stadium : (->* (Real-Length Length+%)
                           (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                           Geo:Stadium)
  (lambda [length radius #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~dimension length))
    (define flradius : Nonnegative-Flonum (~dimension radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:with [id (geo-draw-stadium stroke pattern)
                                       (geo-shape-extent (+ d flength) d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flength flradius)))

(define geo-document : (->* (Real-Length Length+%)
                            (Length+% #:id (Option Symbol) #:extra-n Index #:gapsize Length+% #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Geo:Document)
  (lambda [#:extra-n [extra-n 0] #:id [id #false] #:gapsize [gapsize (&% 61.8)] #:stroke [stroke (void)] #:fill [pattern (void)]
           width height [wave (&% 16.18)]]
    (define-values (flwidth flheight) (~extent width height))
    (define flwave : Nonnegative-Flonum (~dimension wave flheight))
    
    (create-geometry-object geo:document
                            #:with [id (geo-draw-document stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight flwave (~dimension gapsize flwave) extra-n)))

(define geo-database : (->* (Real-Length Length+%)
                            (Length+% #:id (Option Symbol) #:extra-n Index #:gapsize Length+% #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Geo:Database)
  (lambda [#:extra-n [extra-n 2] #:id [id #false] #:gapsize [gapsize (&% 61.8)] #:stroke [stroke (void)] #:fill [pattern (void)]
           width height [bradius (&% 16.18)]]
    (define-values (flwidth flheight) (~extent width height))
    (define flb : Nonnegative-Flonum (~dimension bradius flheight))
    
    (create-geometry-object geo:database
                            #:with [id (geo-draw-database stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight flb (~dimension gapsize flb) extra-n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-stadium : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:stadium? self)
        (dc_stadium cr x0 y0 width height
                    (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))

(define geo-draw-document : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:document? self)
        (dc_document cr x0 y0 width height (geo:document-wave-height self)
                     (geo:document-gapsize self) (geo:document-extra-n self)
                     (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))

(define geo-draw-database : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:database? self)
        (dc_database cr x0 y0 width height (geo:database-bradius self)
                     (geo:database-gapsize self) (geo:database-extra-n self)
                     (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))
