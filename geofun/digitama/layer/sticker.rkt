#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)
(require racket/symbol)

(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")

(require "../convert.rkt")
(require "../composite.rkt")

(require "../dc/text.rkt")
(require "../dc/path.rkt")
(require "../dc/composite.rkt")

(require "type.rkt")
(require "position.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-sticker
  ([self : Geo]
   [port : Geo-Pin-Port]
   [offset : Float-Complex])
  #:type-name Geo-Sticker
  #:transparent)

(define make-sticker : (->* (Geo) (Geo-Pin-Port Real Real) Geo-Sticker)
  (lambda [self [port 'cc] [dx 0] [dy 0]]
    (geo-sticker self port
                 (make-rectangular (real->double-flonum dx)
                                   (real->double-flonum dy)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Sticker-Datum (U Geo-Sticker Geo))
(define-type Geo-Trusted-Anchors (U (Listof Geo-Anchor-Name) (-> Geo-Anchor-Name Boolean)))
(define-type Geo-Anchor->Sticker (-> Geo:Path Geo-Anchor-Name Float-Complex Nonnegative-Flonum Nonnegative-Flonum
                                     (U Geo-Sticker-Datum (Listof Geo-Sticker-Datum) Void False)))

(define default-anchor->sticker : Geo-Anchor->Sticker
  (lambda [self anchor pos Width Height]
    (if (symbol? anchor)
        (geo-text (symbol->immutable-string anchor) #:color 'RoyalBlue)
        (geo-text (keyword->immutable-string anchor) #:color 'Gray))))

(define geo:path-stick : (-> Geo:Path Geo-Anchor->Sticker (-> Geo:Path Geo:Path) (Option Geo-Trusted-Anchors) Boolean
                             (Option Symbol) (Option Geo-Pin-Operator)
                             (U Geo:Group Geo:Path))
  (lambda [master anchor->sticker master->self trusted-anchors truncate? id op]
    (define self : Geo:Path (master->self master))
    (define gpath : Geo-Trail (geo:path-trail self))
    (define srohcna : (Listof Geo-Anchor-Name) (geo-trail-ranchors gpath))
    (define origin : Float-Complex (geo-bbox-position (geo:path-bbox self)))
    (define-values (Width Height) (geo-flsize self))
    (define path-id : Symbol (or id (gensym 'geo:path:)))

    (let stick ([srohcna : (Listof Geo-Anchor-Name) srohcna]
                [stickers : (Listof (GLayerof Geo)) null])
      (cond [(pair? srohcna)
             (let-values ([(anchor rest) (values (car srohcna) (cdr srohcna))])
               (if (or (not trusted-anchors)
                       (if (list? trusted-anchors)
                           (memq anchor trusted-anchors)
                           (trusted-anchors anchor)))
                   (let* ([pos (- (geo-trail-ref gpath anchor) origin)]
                          [ones (anchor->sticker self anchor pos Width Height)])
                     (if (or (geo-sticker? ones) (geo? ones) (pair? ones))
                         (stick rest (append stickers (geo-stickers->layers ones pos)))
                         (stick rest stickers)))
                   (stick rest stickers)))]
            [(pair? stickers)
             (let-values ([(self-layer) (vector-immutable self 0.0 0.0 Width Height)])
               (or (and (not truncate?)
                        (let ([maybe-group (geo-path-try-extend self-layer stickers Width Height)])
                          (and maybe-group (make-geo:group path-id op maybe-group))))
                   (make-geo:group path-id op (vector-immutable Width Height (cons self-layer stickers)))))]
            [else self]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-try-extend : (-> (GLayerof Geo) (Listof (GLayerof Geo)) Nonnegative-Flonum Nonnegative-Flonum (Option (GLayer-Groupof Geo)))
  (lambda [self stickers Width Height]
    (define-values (lx ty rx by) (geo-group-boundary stickers Width Height))

    (and (or (<= lx 0.0) (<= ty 0.0))
         (let ([xoff (if (< lx 0.0) (abs lx) 0.0)]
               [yoff (if (< ty 0.0) (abs ty) 0.0)])
           (vector-immutable (+ rx xoff) (+ by yoff)
                             (cons (geo-layer-translate self xoff yoff)
                                   (for/list : (Listof (GLayerof Geo)) ([sticker (in-list stickers)])
                                     (geo-layer-translate sticker xoff yoff))))))))

(define geo-stickers->layers : (-> (U Geo-Sticker-Datum (Listof Geo-Sticker-Datum)) Float-Complex (Listof (GLayerof Geo)))
  (lambda [self pos]
    (if (list? self)
        (for/list ([one (in-list self)])
          (geo-sticker->layer one pos))
        (list (geo-sticker->layer self pos)))))

(define geo-sticker->layer : (-> Geo-Sticker-Datum Float-Complex (GLayerof Geo))
  (lambda [self pos]
    (define-values (sticker port offset)
      (if (geo-sticker? self)
          (values (geo-sticker-self self) (geo-sticker-port self) (geo-sticker-offset self))
          (values self 'cc 0.0+0.0i)))
    (define-values (width height) (geo-flsize sticker))
    (geo-stick-layer port pos sticker width height offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-stick-layer : (-> Geo-Pin-Port Float-Complex G Nonnegative-Flonum Nonnegative-Flonum Float-Complex (GLayerof G))
  (lambda [port target self width height offset]
    (define-values (dx dy) (geo-superimpose-layer port 0.0 0.0 width height))
    (define pos (+ target offset))

    (vector-immutable self
                      (+ (real->double-flonum (real-part pos)) dx)
                      (+ (real->double-flonum (imag-part pos)) dy)
                      width height)))
