#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/more.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:stadium geo
  ([length : Nonnegative-Flonum]
   [radius : Nonnegative-Flonum]
   [half? : Boolean])
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
(define geo-stadium : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Stadium)
  (lambda [length radius #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~length length))
    (define flradius : Nonnegative-Flonum (~length radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:surface geo-stadium-surface stroke pattern
                            #:extent (geo-shape-plain-extent (+ d flength) d 0.0 0.0)
                            #:id id
                            flength flradius #false)))

(define geo-half-stadium : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Stadium)
  (lambda [length radius #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~length length))
    (define flradius : Nonnegative-Flonum (~length radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:surface geo-stadium-surface stroke pattern
                            #:extent (geo-shape-plain-extent (+ flradius flength) d 0.0 0.0)
                            #:id id
                            flength flradius #true)))

(define geo-bullet : (->* (Real Real) (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Bullet)
  (lambda [ogive radius [barrel -0.384] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (flogive flbarrel) (~size ogive barrel))
    (define flradius : Nonnegative-Flonum (~length radius (+ flogive flbarrel)))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:bullet
                            #:surface geo-bullet-surface stroke pattern
                            #:extent (geo-shape-plain-extent (+ flogive flbarrel) d 0.0 0.0)
                            #:id id
                            flogive flbarrel flradius)))

(define geo-sandglass : (->* (Real)
                             (Real #:id (Option Symbol) #:neck-width Real #:neck-height Real #:tube-height Real
                                   #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint)
                             Geo:Sandglass)
  (lambda [#:neck-width [neck-width -0.1618] #:neck-height [neck-height -0.0618] #:tube-height [tube-height 0]
           #:stroke [stroke (void)] #:fill [pattern (void)] #:id [id #false]
           width [height -1.618]]
    (define-values (flwidth flheight) (~size width height))
    (define neck-flwidth (~length neck-width flwidth))
    (define neck-flheight (~length neck-height flheight))
    (define tube-flheight (~length tube-height flheight))
    
    (create-geometry-object geo:sandglass
                            #:surface (geo-sandglass-surface flwidth flheight) stroke pattern
                            #:extent (geo-shape-plain-extent flwidth flheight 0.0 0.0)
                            #:id id
                            neck-flwidth neck-flheight tube-flheight)))

(define geo-document : (->* (Real Real) (Real #:id (Option Symbol) #:extra-n Index #:gapsize Real #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Document)
  (lambda [width height [wave -0.384] #:extra-n [extra-n 0] #:id [id #false] #:gapsize [gapsize -0.618] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~size width height))
    (define flwave : Nonnegative-Flonum (~length wave flheight))
    
    (create-geometry-object geo:document
                            #:surface geo-document-surface stroke pattern
                            #:extent (geo-shape-plain-extent flwidth flheight 0.0 0.0)
                            #:id id
                            flwidth flheight flwave (~length gapsize flwave) extra-n)))

(define geo-database : (->* (Real Real) (Real #:id (Option Symbol) #:extra-n Index #:gapsize Real #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Database)
  (lambda [width height [bradius -0.1618] #:extra-n [extra-n 2] #:id [id #false] #:gapsize [gapsize -0.618] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (flwidth flheight) (~size width height))
    (define flb : Nonnegative-Flonum (~length bradius flheight))
    
    (create-geometry-object geo:database
                            #:surface geo-database-surface stroke pattern
                            #:extent (geo-shape-plain-extent flwidth flheight 0.0 0.0)
                            #:id id
                            flwidth flheight flb (~length gapsize flb) extra-n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-stadium-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:stadium?])
      (if (geo:stadium-half? self)
          (dc_half_stadium create-abstract-surface
                           (geo:stadium-length self) (geo:stadium-radius self)
                           (current-stroke-source) (current-fill-source)
                           (default-geometry-density))
          (dc_stadium create-abstract-surface
                      (geo:stadium-length self) (geo:stadium-radius self)
                      (current-stroke-source) (current-fill-source)
                      (default-geometry-density))))))

(define geo-bullet-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:bullet?])
      (dc_bullet create-abstract-surface
                 (geo:bullet-ogive-length self) (geo:bullet-barrel-length self) (geo:bullet-radius self)
                 (current-stroke-source) (current-fill-source)
                 (default-geometry-density)))))

(define geo-sandglass-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Create)
  (lambda [width height]
    (λ [self]
      (with-asserts ([self geo:sandglass?])
        (dc_sandglass create-abstract-surface
                      width height
                      (geo:sandglass-neck-width self) (geo:sandglass-neck-height self) (geo:sandglass-tube-height self)
                      (current-stroke-source) (current-fill-source)
                      (default-geometry-density))))))

(define geo-document-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:document?])
      (dc_document create-abstract-surface
                   (geo:document-width self) (geo:document-height self) (geo:document-wave-height self)
                   (geo:document-gapsize self) (geo:document-extra-n self)
                   (current-stroke-source) (current-fill-source)
                   (default-geometry-density)))))

(define geo-database-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:database?])
      (dc_database create-abstract-surface
                   (geo:database-width self) (geo:database-height self) (geo:database-bradius self)
                   (geo:database-gapsize self) (geo:database-extra-n self)
                   (current-stroke-source) (current-fill-source)
                   (default-geometry-density)))))
