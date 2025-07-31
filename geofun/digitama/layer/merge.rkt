#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-layers-try-extend : (case-> [(GLayerof Geo) (Listof (GLayerof Geo)) -> (Option (GLayer-Groupof Geo))]
                                        [(Pairof (GLayerof Geo) (Listof (GLayerof Geo))) Nonnegative-Flonum Nonnegative-Flonum -> (Option (GLayer-Groupof Geo))])
  (case-lambda
    [(master stickers)
     ; TODO: deal with the initial position of the `master` sticker
     (define-values (Width Height) (values (glayer-width master) (glayer-height master)))
     (let-values ([(lx ty rx by) (geo-group-boundary stickers Width Height)])
       (and (or (< lx 0.0) (< ty 0.0) (> rx Width) (> by Height))
            (let ([xoff (if (< lx 0.0) (abs lx) 0.0)]
                  [yoff (if (< ty 0.0) (abs ty) 0.0)])
              ((inst glayer-group Geo) (+ (max rx Width) xoff) (+ (max by Height) yoff)
                                       (cons (geo-layer-translate master xoff yoff)
                                             (for/list : (Listof (GLayerof Geo)) ([sticker (in-list stickers)])
                                               (geo-layer-translate sticker xoff yoff)))))))]
    [(stickers Width Height)
     (let-values ([(lx ty rx by) (geo-group-boundary stickers Width Height)])
       (and (or (< lx 0.0) (< ty 0.0) (> rx Width) (> by Height))
            (let ([xoff (if (< lx 0.0) (abs lx) 0.0)]
                  [yoff (if (< ty 0.0) (abs ty) 0.0)])
              ((inst glayer-group Geo) (+ (max rx Width) xoff) (+ (max by Height) yoff)
                                       (cons (geo-layer-translate (car stickers) xoff yoff)
                                             (for/list : (Listof (GLayerof Geo)) ([sticker (in-list (cdr stickers))])
                                               (geo-layer-translate sticker xoff yoff)))))))]))

(define geo-layers-merge : (-> (GLayer-Groupof Geo) (Listof (GLayerof Geo)) (GLayer-Groupof Geo))
  (lambda [group layers]
    (define layers++ : (Pairof (GLayerof Geo) (Listof (GLayerof Geo))) (append (glayer-group-layers group) layers))
    (define-values (W H) (values (glayer-group-width group) (glayer-group-height group)))

    (or (geo-layers-try-extend layers++ W H)
        (glayer-group W H layers++))))
