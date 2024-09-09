#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)
(require racket/symbol)

(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")
(require "../geometry/anchor.rkt")

(require "../convert.rkt")
(require "../composite.rkt")

(require "../dc/text.rkt")
(require "../dc/path.rkt")
(require "../dc/composite.rkt")

(require "type.rkt")
(require "position.rkt")

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
(define-type Geo-Anchor->Sticker (-> Geo:Path Geo-Anchor-Name Float-Complex Nonnegative-Flonum Nonnegative-Flonum (U Geo-Sticker-Datum Void False)))

(define default-anchor->sticker : Geo-Anchor->Sticker
  (lambda [self anchor pos Width Height]
    (if (symbol? anchor)
        (geo-text (symbol->immutable-string anchor) #:color 'RoyalBlue)
        (geo-text (keyword->immutable-string anchor) #:color 'Gray))))

(define geo:path-stick : (-> Geo:Path Geo-Anchor->Sticker (Option Geo-Trusted-Anchors) Boolean
                             (Option Symbol) (Option Geo-Pin-Operator) Float-Complex
                             (U Geo:Group Geo:Path))
  (lambda [self anchor->sticker trusted-anchors truncate? id op offset]
    (define layers : (Option (GLayer-Groupof Geo)) (geo:path-stick/list self anchor->sticker trusted-anchors offset truncate?))
    (define gp-id : Symbol (or id (gensym 'geo:path:)))

    (cond [(not layers) self]
          [else (make-geo:group gp-id op layers)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo:path-stick/list : (-> Geo:Path Geo-Anchor->Sticker (Option Geo-Trusted-Anchors) Float-Complex Boolean (Option (GLayer-Groupof Geo)))
  (lambda [self anchor->sticker trusted-anchors offset truncate?]
    (define gpath : Geo-Trail (geo:path-trail self))
    (define srohcna : (Listof Geo-Anchor-Name) (geo-trail-ranchors gpath))
    (define origin : Float-Complex (geo-bbox-position (geo:path-bbox self)))
    (define-values (Width Height) (geo-flsize self))
    
    (let stick ([srohcna : (Listof Geo-Anchor-Name) srohcna]
                [stickers : (Listof (GLayerof Geo)) null])
      (cond [(pair? srohcna)
             (let-values ([(anchor rest) (values (car srohcna) (cdr srohcna))])
               (if (geo-anchor-trusted? anchor trusted-anchors)
                   (let ([slayer (geo-sticker-layer self anchor->sticker anchor (geo-trail-ref gpath anchor) origin offset Width Height)])
                     (stick rest (if (not slayer) stickers (cons slayer stickers))))
                   (stick rest stickers)))]
            [(pair? stickers)
             ; don't offset the `self` path itself, leave it to the path drawer
             (let-values ([(self-layer) (vector-immutable self 0.0 0.0 Width Height)])
               (or (and (not truncate?)
                        (geo-path-try-extend/list self-layer stickers))
                   (vector-immutable Width Height (cons self-layer stickers))))]
            [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-try-extend/list : (case-> [(GLayerof Geo) (Listof (GLayerof Geo))  -> (Option (GLayer-Groupof Geo))]
                                           [(Pairof (GLayerof Geo) (Listof (GLayerof Geo))) Nonnegative-Flonum Nonnegative-Flonum -> (Option (GLayer-Groupof Geo))])
  (case-lambda
    [(master stickers)
     ; TODO: deal with the initial position of the `master` sticker
     (define-values (Width Height) (values (vector-ref master 3) (vector-ref master 4)))
     (let-values ([(lx ty rx by) (geo-group-boundary stickers Width Height)])
       (and (or (< lx 0.0) (< ty 0.0) (> rx Width) (> by Height))
            (let ([xoff (if (< lx 0.0) (abs lx) 0.0)]
                  [yoff (if (< ty 0.0) (abs ty) 0.0)])
              (vector-immutable (+ (max rx Width) xoff) (+ (max by Height) yoff)
                                (cons (geo-layer-translate master xoff yoff)
                                      (for/list : (Listof (GLayerof Geo)) ([sticker (in-list stickers)])
                                        (geo-layer-translate sticker xoff yoff)))))))]
    [(stickers Width Height)
     (let-values ([(lx ty rx by) (geo-group-boundary stickers Width Height)])
       (and (or (< lx 0.0) (< ty 0.0) (> rx Width) (> by Height))
            (let ([xoff (if (< lx 0.0) (abs lx) 0.0)]
                  [yoff (if (< ty 0.0) (abs ty) 0.0)])
              (vector-immutable (+ (max rx Width) xoff) (+ (max by Height) yoff)
                                (cons (geo-layer-translate (car stickers) xoff yoff)
                                      (for/list : (Listof (GLayerof Geo)) ([sticker (in-list (cdr stickers))])
                                        (geo-layer-translate sticker xoff yoff)))))))]))

(define geo-sticker-layer : (-> Geo:Path Geo-Anchor->Sticker Geo-Anchor-Name
                                Float-Complex Float-Complex Float-Complex Nonnegative-Flonum Nonnegative-Flonum
                                (Option (GLayerof Geo)))
  (lambda [self anchor->sticker anchor position origin offset Width Height]
    (define pos (- position origin))
    (define stk (anchor->sticker self anchor pos Width Height))
    
    (and (or (geo-sticker? stk) (geo? stk))
         (geo-sticker->layer stk pos offset))))

(define geo-sticker->layer : (-> Geo-Sticker-Datum Float-Complex Float-Complex (GLayerof Geo))
  (lambda [self pos offset]
    (if (geo? self)
        (geo-own-layer 'cc pos self offset)
        (geo-own-layer (geo-sticker-port self) pos (geo-sticker-self self)
                       (+ (geo-sticker-offset self) offset)))))
