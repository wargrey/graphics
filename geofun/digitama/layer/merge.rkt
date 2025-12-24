#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "../self.rkt")

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

(define geo-layers-try-push-back : (-> (GLayerof Geo) (Listof (GLayerof Geo)) (Option (GLayer-Groupof Geo)))
  (lambda [master stickers]
    (define maybe-group (geo-layers-try-extend master stickers))

    (and maybe-group
         (let* ([layers (glayer-group-layers maybe-group)]
                [layers (append (cdr layers) (list (car layers)))])
           (and (pair? layers)
                (glayer-group (glayer-group-width maybe-group)
                              (glayer-group-height maybe-group)
                              layers))))))

(define geo-layers-merge : (case-> [(GLayer-Groupof Geo) (Listof (GLayerof Geo)) -> (GLayer-Groupof Geo)]
                                   [(Pairof (GLayerof Geo) (Listof (GLayerof Geo))) -> (GLayer-Groupof Geo)])
  (case-lambda
    [(group layers)
     (let*-values ([(layers++) (append (glayer-group-layers group) layers)]
                   [(W H) (values (glayer-group-width group) (glayer-group-height group))])
       (or (geo-layers-try-extend layers++ W H)
           (glayer-group W H layers++)))]
    [(layers)
     (let-values ([(pos) (geo-layer-position (car layers))]
                  [(W H) (geo-layer-size (car layers))])
       (or (geo-layers-try-extend layers
                                  (max (+ W (real-part pos)) 0.0)
                                  (max (+ H (imag-part pos)) 0.0))
           (glayer-group W H layers)))]))
