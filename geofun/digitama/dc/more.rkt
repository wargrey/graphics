#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/more.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:stadium geo
  ([length : Nonnegative-Flonum]
   [radius : Nonnegative-Flonum]
   [side : Symbol])
  #:type-name Geo:Stadium
  #:transparent)

(struct geo:bullet geo
  ([ogive-length : Nonnegative-Flonum]
   [barrel-length : Nonnegative-Flonum]
   [radius : Nonnegative-Flonum])
  #:type-name Geo:Bullet
  #:transparent)

(struct geo:sandglass geo
  ([neck-width : Nonnegative-Flonum]
   [neck-height : Nonnegative-Flonum]
   [tube-height : Nonnegative-Flonum])
  #:type-name Geo:Sandglass
  #:transparent)

(struct geo:storage geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [aradius : Nonnegative-Flonum])
  #:type-name Geo:Storage
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
(define geo-stadium : (->* (Real Real+%)
                           (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                           Geo:Stadium)
  (lambda [length radius #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~length length))
    (define flradius : Nonnegative-Flonum (~length radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:with [id (geo-draw-stadium stroke pattern)
                                       (geo-shape-extent (+ d flength) d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flength flradius 'both)))

(define geo-lstadium : (->* (Real Real+%)
                            (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Geo:Stadium)
  (lambda [length radius #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~length length))
    (define flradius : Nonnegative-Flonum (~length radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:with [id (geo-draw-stadium stroke pattern)
                                       (geo-shape-extent (+ flradius flength) d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flength flradius 'left)))

(define geo-rstadium : (->* (Real Real+%)
                            (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Geo:Stadium)
  (lambda [length radius #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~length length))
    (define flradius : Nonnegative-Flonum (~length radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:with [id (geo-draw-stadium stroke pattern)
                                       (geo-shape-extent (+ flradius flength) d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flength flradius 'right)))

(define geo-bullet : (->* (Real Real+%)
                          (Real+% #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                          Geo:Bullet)
  (lambda [ogive radius [barrel '(38.4 %)] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (flogive flbarrel) (~extent ogive barrel))
    (define flradius : Nonnegative-Flonum (~length radius (+ flogive flbarrel)))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:bullet
                            #:with [id (geo-draw-bullet stroke pattern)
                                       (geo-shape-extent (+ flogive flbarrel) d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flogive flbarrel flradius)))

(define geo-sandglass : (->* (Real)
                             (Real+% #:id (Option Symbol) #:neck-width Real+% #:neck-height Real+% #:tube-height Real+%
                                     #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                             Geo:Sandglass)
  (lambda [#:neck-width [neck-width '(16.18 %)] #:neck-height [neck-height '(6.18 %)] #:tube-height [tube-height 0]
           #:stroke [stroke (void)] #:fill [pattern (void)] #:id [id #false]
           width [height '(161.8 %)]]
    (define-values (flwidth flheight) (~extent width height))
    (define neck-flwidth (~length neck-width flwidth))
    (define neck-flheight (~length neck-height flheight))
    (define tube-flheight (~length tube-height flheight))
    
    (create-geometry-object geo:sandglass
                            #:with [id (geo-draw-sandglass stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            neck-flwidth neck-flheight tube-flheight)))

(define geo-storage : (->* (Real Real+%)
                           (Real+% #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                           Geo:Storage)
  (lambda [width height [aradius '(38.4 %)] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~extent width height))
    (define fla : Nonnegative-Flonum (~length aradius (* flheight 0.5)))
    
    (create-geometry-object geo:storage
                            #:with [id (geo-draw-storage stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight fla)))

(define geo-document : (->* (Real Real+%)
                            (Real+% #:id (Option Symbol) #:extra-n Index #:gapsize Real+% #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Geo:Document)
  (lambda [#:extra-n [extra-n 0] #:id [id #false] #:gapsize [gapsize '(61.8 %)] #:stroke [stroke (void)] #:fill [pattern (void)]
           width height [wave '(16.18 %)]]
    (define-values (flwidth flheight) (~extent width height))
    (define flwave : Nonnegative-Flonum (~length wave flheight))
    
    (create-geometry-object geo:document
                            #:with [id (geo-draw-document stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight flwave (~length gapsize flwave) extra-n)))

(define geo-database : (->* (Real Real+%)
                            (Real+% #:id (Option Symbol) #:extra-n Index #:gapsize Real+% #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Geo:Database)
  (lambda [#:extra-n [extra-n 2] #:id [id #false] #:gapsize [gapsize '(61.8 %)] #:stroke [stroke (void)] #:fill [pattern (void)]
           width height [bradius '(16.18 %)]]
    (define-values (flwidth flheight) (~extent width height))
    (define flb : Nonnegative-Flonum (~length bradius flheight))
    
    (create-geometry-object geo:database
                            #:with [id (geo-draw-database stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight flb (~length gapsize flb) extra-n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-stadium : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:stadium? self)
        (define semicircle-side (geo:stadium-side self))
        (cond [(eq? semicircle-side 'left)
               (dc_half_stadium cr x0 y0 width height
                                (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                                #true)]
              [(eq? semicircle-side 'right)
               (dc_half_stadium cr x0 y0 width height
                                (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                                #false)]
              [else (dc_stadium cr x0 y0 width height
                                (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))])))))

(define geo-draw-bullet : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:bullet? self)
        (dc_bullet cr x0 y0 width height (geo:bullet-ogive-length self)
                   (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))

(define geo-draw-sandglass : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:sandglass? self)
        (dc_sandglass cr x0 y0 width height
                      (geo:sandglass-neck-width self) (geo:sandglass-neck-height self) (geo:sandglass-tube-height self)
                      (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))

(define geo-draw-storage : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:storage? self)
        (dc_general_storage cr x0 y0 width height (geo:storage-aradius self)
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
