#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "trail.rkt")
(require "trace.rkt")
(require "anchor.rkt")

(require "../self.rkt")
(require "../composite.rkt")
(require "../geometry/bbox.rkt")
(require "../nice/box.rkt")

(require "../dc/text.rkt")
(require "../dc/composite.rkt")

(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../layer/merge.rkt")
(require "../layer/combine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Track-Anchor->Sticker
  (-> Geo-Anchor-Name Float-Complex Nonnegative-Flonum Nonnegative-Flonum
      (U Geo-Sticker-Datum Void False)))

(define current-master-track : (Parameterof (Option Geo:Track)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-track-anchor->sticker : Geo-Track-Anchor->Sticker
  (lambda [anchor pos Width Height]
    (if (symbol? anchor)
        (geo-text (geo-anchor->string anchor) #:color 'RoyalBlue)
        (geo-text (geo-anchor->string anchor) #:color 'Gray))))

(define geo:track-stick : (-> Geo:Track Geo-Track-Anchor->Sticker (Option Geo-Trusted-Anchors) Boolean
                              (Option Symbol) (Option String) (Option Geo-Pin-Operator) (Option Geo-Pin-Operator) Float-Complex
                              Geo-Frame-Datum
                              Geo:Trail)
  (lambda [self anchor->sticker trusted-anchors truncate? id desc base-op sibs-op offset frame]
    (parameterize ([current-master-track self])
      (define-values (border background margin padding open-sides) (geo-frame-values frame))
      (define layers : (Option (GLayer-Groupof Geo)) (geo-track-stick/list self anchor->sticker trusted-anchors offset truncate?))
      (define gp-id : Symbol (or id (gensym 'geo:track:)))
      
      (create-geometry-group geo:trail gp-id base-op sibs-op
                             #:desc desc #:open-sides open-sides
                             #:border border #:background background
                             #:margin margin #:padding padding
                             (if (not layers) (geo-own-layers self) layers)
                             self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-stick/list : (-> Geo:Track Geo-Track-Anchor->Sticker (Option Geo-Trusted-Anchors) Float-Complex Boolean (Option (GLayer-Groupof Geo)))
  (lambda [self anchor->sticker trusted-anchors offset truncate?]
    (define gpath : Geo-Trace (geo:track-trace self))
    (define srohcna : (Listof Geo-Anchor-Name) (geo-trace-ranchors gpath))
    (define origin : Float-Complex (geo-bbox-position (geo:track-bbox self)))
    (define-values (Width Height) (geo-flsize self))

    (define user-stickers : (Listof (GLayerof Geo))
      (let ->layer ([srekcits (geo:track-stickers self)]
                    [stickers : (Listof (GLayerof Geo)) null])
        (if (pair? srekcits)
            (let-values ([(s rest) (values (car srekcits) (cdr srekcits))])
              (->layer rest (cons (geo-sticker->layer (car s) (- (cdr s) origin) offset) stickers)))
            stickers)))
    
    (let stick ([srohcna : (Listof Geo-Anchor-Name) srohcna]
                [stickers : (Listof (GLayerof Geo)) user-stickers])
      (cond [(pair? srohcna)
             (let-values ([(anchor rest) (values (car srohcna) (cdr srohcna))])
               (if (geo-anchor-trusted? anchor trusted-anchors)
                   (let ([slayer (geo-track-sticker-layer anchor->sticker anchor (- (geo-trace-ref gpath anchor) origin) offset Width Height)])
                     (stick rest (if (not slayer) stickers (cons slayer stickers))))
                   (stick rest stickers)))]
            [(pair? stickers)
             ; don't offset the `self` path itself, leave it to the path drawer
             (let-values ([(self-layer) (glayer self 0.0 0.0 Width Height)])
               (or (and (not truncate?)
                        (geo-layers-try-extend self-layer stickers))
                   ((inst glayer-group Geo) Width Height (cons self-layer stickers))))]
            [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-sticker-layer : (-> Geo-Track-Anchor->Sticker Geo-Anchor-Name
                                      Float-Complex Float-Complex Nonnegative-Flonum Nonnegative-Flonum
                                      (Option (GLayerof Geo)))
  (lambda [anchor->sticker anchor position offset Width Height]
    (define stk (anchor->sticker anchor position Width Height))
    
    (and (geo-sticker-datum? stk)
         (geo-sticker->layer stk position offset))))
