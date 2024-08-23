#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../convert.rkt")
(require "../unsafe/dc/shape.rkt")
(require "../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:line geo
  ([x : Flonum]
   [y : Flonum]
   [dx : Flonum]
   [dy : Flonum])
  #:type-name Geo:Line
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-hline : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint) Geo:Line)
  (lambda [#:id [id #false] #:stroke [stroke (void)] width height]
    (define-values (flwidth flheight) (~size width height))
    
    (create-geometry-object geo:line
                            #:surface geo-line-surface stroke
                            #:bbox (geo-shape-plain-bbox flwidth flheight)
                            #:id id
                            0.0 (* flheight 0.5) flwidth 0.0)))

(define geo-vline : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint) Geo:Line)
  (lambda [#:id [id #false] #:stroke [stroke (void)] width height]
    (define-values (flwidth flheight) (~size width height))
    
    (create-geometry-object geo:line
                            #:surface geo-line-surface stroke
                            #:bbox (geo-shape-plain-bbox flwidth flheight)
                            #:id id
                            (* flwidth 0.5) 0.0 0.0 flheight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-line-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:line?])
      (define-values (w h) (geo-flsize self))
      
      (dc_line create-abstract-surface
              (geo:line-x self) (geo:line-y self) (geo:line-dx self) (geo:line-dy self)
              w h (current-stroke-source*)
              (default-geometry-density)))))
