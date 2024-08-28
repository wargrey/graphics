#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require digimon/sequence)

(require "ink.rkt")

(require "../../paint.rkt")
(require "paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/plain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:blank geo
  ([body : (Option Geo)])
  #:type-name Geo:Blank
  #:transparent)

(struct geo:frame geo
  ([body : Geo]
   [margins : (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum)]
   [pads :  (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum)])
  #:type-name Geo:Frame
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

(define geo-frame : (-> Geo [#:id (Option Symbol)]
                        [#:border Maybe-Stroke-Paint] [#:background Maybe-Fill-Paint]
                        [#:margin (U Nonnegative-Real (Listof Nonnegative-Real))]
                        [#:padding (U Nonnegative-Real (Listof Nonnegative-Real))]
                        Geo:Frame)
  (lambda [geo #:id [id #false] #:margin [margin 0.0] #:padding [inset 0.0] #:border [border (void)] #:background [bg-fill (void)]]
    (define-values (mtop mright mbottom mleft)
      (cond [(list? margin) (list->4:values (map real->double-flonum margin) 0.0)]
            [else (let ([fl (real->double-flonum margin)]) (values fl fl fl fl))]))
    (define-values (ptop pright pbottom pleft)
      (cond [(list? inset) (list->4:values (map real->double-flonum inset) 0.0)]
            [else (let ([fl (real->double-flonum inset)]) (values fl fl fl fl))]))
    (define-values (flwidth flheight) (geo-flsize geo))

    (define geo-frame-extent : Geo-Calculate-Extent
      (lambda [self]
        (define-values (W H lx ty w h)
          (dc_frame_size flwidth flheight
                         mtop mright mbottom mleft ptop pright pbottom pleft
                         (geo-select-border-paint border)))
        (values W H (make-geo-ink lx ty w h))))
    
    (create-geometry-object geo:frame
                            #:surface (geo-frame-surface border bg-fill)
                            #:extent geo-frame-extent
                            #:id id
                            geo
                            (vector-immutable mtop mright mbottom mleft)
                            (vector-immutable ptop pright pbottom pleft))))

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

(define geo-frame-surface : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-bdr alt-bg]
    (lambda [self]
      (with-asserts ([self geo:frame?])
        (define body (geo:frame-body self))
        (define margins (geo:frame-margins self))
        (define pads (geo:frame-pads self))
        (define-values (flwidth flheight) (geo-flsize body))
        (define-values (mtop mright mbottom mleft ptop pright pbottom pleft)
          (values (vector-ref margins 0) (vector-ref margins 1) (vector-ref margins 2) (vector-ref margins 3)
                  (vector-ref pads 0)    (vector-ref pads 1)    (vector-ref pads 2)    (vector-ref pads 3)))
    
        (dc_frame create-abstract-surface (geo-create-surface body) flwidth flheight
                  mtop mright mbottom mleft ptop pright pbottom pleft
                  (geo-select-border-paint alt-bdr) (geo-select-background-source alt-bg)
                  (default-geometry-density))))))
