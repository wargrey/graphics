#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "ink.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/plain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:blank geo
  ([body : (Option Geo)])
  #:type-name Geo:Blank
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-blank : (->* () (Real (Option Real) #:id (Option Symbol)) Geo:Blank)
  (lambda [[width 0.0] [height #false] #:id [id #false]]
    (define-values (flwidth flheight) (~size width (or height width)))
    
    (create-geometry-object geo:blank
                            #:surface (geo-blank-surface flwidth flheight)
                            #:extent (geo-blank-extent flwidth flheight)
                            #:id id
                            #false)))

(define geo-ghost : (-> Geo [#:id (Option Symbol)] Geo:Blank)
  (lambda [geo #:id [id #false]]
    (define-values (flwidth flheight) (geo-flsize geo))
    (create-geometry-object geo:blank
                            #:surface geo-ghost-surface
                            #:extent geo-ghost-extent
                            #:id id
                            geo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the size of target geometry might be affected by stroke and border
(define geo-ghost-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:blank?])
      (define-values (w h ?ink) (geo-extent (assert (geo:blank-body self))))
      (values w h geo-null-ink))))

(define geo-ghost-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:blank?])
      (define-values (flwidth flheight) (geo-flsize (assert (geo:blank-body self))))
      (dc_blank create-abstract-surface
                flwidth flheight
                (default-geometry-density)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-blank-extent : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Calculate-Extent)
  (lambda [flwidth flheight]
    (λ [self]
      (values flwidth flheight geo-null-ink))))

(define geo-blank-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Create)
  (lambda [flwidth flheight]
    (λ [self]
      (dc_blank create-abstract-surface
                flwidth flheight
                (default-geometry-density)))))
