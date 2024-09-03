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
(define-type Geo-Anchor->Sticker (-> Geo:Path Geo-Anchor-Name Float-Complex Nonnegative-Flonum Nonnegative-Flonum (U Geo-Sticker-Datum Void False)))

(define default-anchor->sticker : Geo-Anchor->Sticker
  (lambda [self anchor pos Width Height]
    (if (symbol? anchor)
        (geo-text (symbol->immutable-string anchor) #:color 'RoyalBlue)
        (geo-text (keyword->immutable-string anchor) #:color 'Gray))))

(define geo:path-stick : (-> Geo:Path Geo-Anchor->Sticker (Option Geo-Trusted-Anchors) Boolean
                             (Option Symbol) (Option Geo-Pin-Operator) Float-Complex
                             (U Geo:Group Geo:Path))
  (lambda [self anchor->sticker trusted-anchors truncate? id op paint-offset]
    (define path-id : Symbol (or id (gensym 'geo:path:)))
    (define layers : (Option (GLayer-Groupof Geo)) (geo:path-stick/list self anchor->sticker trusted-anchors truncate? paint-offset))

    (cond [(or layers) (make-geo:group path-id op layers)]
          [else self])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo:path-stick/list : (-> Geo:Path Geo-Anchor->Sticker (Option Geo-Trusted-Anchors) Boolean Float-Complex (Option (GLayer-Groupof Geo)))
  (lambda [self anchor->sticker trusted-anchors truncate? paint-offset]
    (define gpath : Geo-Trail (geo:path-trail self))
    (define srohcna : (Listof Geo-Anchor-Name) (geo-trail-ranchors gpath))
    (define origin : Float-Complex (geo-bbox-position (geo:path-bbox self)))
    (define-values (Width Height) (geo-flsize self))
    
    (let stick ([srohcna : (Listof Geo-Anchor-Name) srohcna]
                [stickers : (Listof (GLayerof Geo)) null])
      (cond [(pair? srohcna)
             (let-values ([(anchor rest) (values (car srohcna) (cdr srohcna))])
               (if (or (not trusted-anchors)
                       (if (list? trusted-anchors)
                           (memq anchor trusted-anchors)
                           (trusted-anchors anchor)))
                   (let* ([pos (- (geo-trail-ref gpath anchor) origin)]
                          [stk (anchor->sticker self anchor pos Width Height)])
                     (if (or (geo-sticker? stk) (geo? stk))
                         (stick rest (cons (geo-sticker->layer stk pos paint-offset) stickers))
                         (stick rest stickers)))
                   (stick rest stickers)))]
            [(pair? stickers)
             ; don't offset the path itself, leave it to the path drawer
             (let-values ([(self-layer) (vector-immutable self 0.0 0.0 Width Height)])
               (or (and (not truncate?)
                        (geo-path-try-extend self-layer stickers Width Height))
                   (vector-immutable Width Height (cons self-layer stickers))))]
            [else #false]))))

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

(define geo-path-sticker-dictionary : (-> (Listof (GLayerof Geo)) (Immutable-HashTable Symbol (GLayerof Geo)))
  (lambda [stickers]
    (for/hasheq : (Immutable-HashTable Symbol (GLayerof Geo)) ([sticker (in-list stickers)])
      (values (geo-id (vector-ref sticker 0)) sticker))))

(define geo-sticker->layer : (-> Geo-Sticker-Datum Float-Complex Float-Complex (GLayerof Geo))
  (lambda [self pos paint-offset]
    (define-values (sticker port offset)
      (if (geo-sticker? self)
          (values (geo-sticker-self self) (geo-sticker-port self) (geo-sticker-offset self))
          (values self 'cc 0.0+0.0i)))
    (define-values (width height) (geo-flsize sticker))
    (geo-stick-layer port pos sticker width height offset paint-offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-stick-layer : (-> Geo-Pin-Port Float-Complex G Nonnegative-Flonum Nonnegative-Flonum Float-Complex Float-Complex (GLayerof G))
  (lambda [port target self width height offset paint-offset]
    (define-values (dx dy) (geo-superimpose-layer port 0.0 0.0 width height))
    (define pos (+ target offset paint-offset))

    (vector-immutable self
                      (+ (real-part pos) dx)
                      (+ (imag-part pos) dy)
                      width height)))
