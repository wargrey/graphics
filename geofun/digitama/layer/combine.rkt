#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "position.rkt")
(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-own-layer : (-> Geo (GLayerof Geo))
  (lambda [base]
    (define-values (width height) (geo-flsize base))
    (glayer base 0.0 0.0 width height)))

(define geo-own-layers : (-> Geo (GLayer-Groupof Geo))
  (lambda [base]
    (define-values (width height) (geo-flsize base))
    (glayer-group width height
                  (list (glayer base 0.0 0.0 width height)))))

(define geo-composite-layers
  : (case-> [Geo Geo Flonum Flonum -> (GLayer-Groupof Geo)]
            [Geo Geo Flonum Flonum Flonum Flonum -> (GLayer-Groupof Geo)]
            [Geo Geo Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum -> (GLayer-Groupof Geo)])
  (case-lambda
    [(geo1 geo2 dx dy)
     (let-values ([(w1 h1) (geo-size geo1)]
                  [(w2 h2) (geo-size geo2)])
       (geo-composite-layers geo1 geo2 w1 h1 w2 h2 dx dy))]
    [(geo1 geo2 x1% y1% x2% y2%)
     (let-values ([(w1 h1) (geo-size geo1)]
                  [(w2 h2) (geo-size geo2)])
       (geo-composite-layers geo1 geo2 w1 h1 w2 h2
                             (- (* x1% w1) (* x2% w2))
                             (- (* y1% h1) (* y2% h2))))]
    [(geo1 geo2 w1 h1 w2 h2 dx dy)
     (let-values ([(dx1 dy1) (values (max (- 0.0 dx) 0.0) (max (- 0.0 dy) 0.0))]
                  [(dx2 dy2) (values (max dx 0.0) (max dy 0.0))])
       (define width  (max (+ dx1 w1) (+ dx2 w2)))
       (define height (max (+ dy1 h1) (+ dy2 h2)))

       (glayer-group width height
                     (list (glayer geo1 dx1 dy1 w1 h1)
                           (glayer geo2 dx2 dy2 w2 h2))))]))

(define geo-pin-layers : (-> Geo (Listof Geo) Flonum Flonum Flonum Flonum (GLayer-Groupof Geo))
  (lambda [base siblings x1% y1% x2% y2%]
    (define-values (min-width min-height) (geo-size base))
    (let pin ([sreyal : (Listof (GLayerof Geo)) null]
              [dx : Flonum 0.0] [dy : Flonum 0.0] [w1 : Nonnegative-Flonum min-width] [h1 : Nonnegative-Flonum min-height]
              [lx : Flonum 0.0] [ty : Flonum 0.0] [rx : Flonum min-width] [by : Flonum min-height] 
              [siblings : (Listof Geo) siblings])
      (cond [(pair? siblings)
             (let ([geo2 (car siblings)])
               (define-values (w2 h2) (geo-size geo2))
               (define nx : Flonum (+ dx (- (* w1 x1%) (* w2 x2%))))
               (define ny : Flonum (+ dy (- (* h1 y1%) (* h2 y2%))))
               
               (pin (cons (glayer geo2 nx ny w2 h2) sreyal)
                    nx ny w2 h2
                    (min lx nx) (min ty ny) (max rx (+ nx w2)) (max by (+ ny h2))
                    (cdr siblings)))]
            [(or (< lx 0.0) (< ty 0.0))
             (let ([xoff (if (< lx 0.0) (- lx) 0.0)]
                   [yoff (if (< ty 0.0) (- ty) 0.0)])
               (let translate ([sreyal : (Listof (GLayerof Geo)) sreyal]
                               [layers : (Listof (GLayerof Geo)) null])
                 (if (pair? sreyal)
                     (let ([layer (car sreyal)])
                       (translate (cdr sreyal)
                                  (cons (geo-layer-translate layer xoff yoff) layers)))
                     ((inst glayer-group Geo) (+ (max rx min-width) xoff) (+ (max by min-height) yoff)
                                              (cons (glayer base xoff yoff min-width min-height) layers)))))]
            [else ((inst glayer-group Geo) (max rx min-width) (max by min-height)
                                           (cons (glayer base 0.0 0.0 min-width min-height)
                                                 (reverse sreyal)))]))))

(define geo-append-layers : (-> Geo-Append-Align Geo (Listof Geo) Flonum (GLayer-Groupof Geo))
  (lambda [alignment base siblings gapsize]
    (define-values (min-width min-height) (geo-flsize base))
    (define-values (flwidth flheight xoff yoff sreyal)
      (let compose : (Values Nonnegative-Flonum Nonnegative-Flonum  Flonum Flonum (Listof (GLayerof Geo)))
        ([sreyal : (Listof (GLayerof Geo)) null]
         [lx : Flonum 0.0] [ty : Flonum 0.0] [rx : Flonum min-width] [by : Flonum min-height]
         [x : Flonum (+ min-width gapsize)] [y : Flonum (+ min-height gapsize)]
         [siblings : (Listof Geo) siblings])
        (cond [(pair? siblings)
               (let*-values ([(sibling rest) (values (car siblings) (cdr siblings))]
                             [(swidth sheight) (geo-flsize sibling)])
                 (case alignment
                   [(vl vc vr v?)
                    (let ([delta (+ sheight gapsize)])
                      (compose (cons (glayer sibling x y swidth sheight) sreyal)
                               lx (min ty y) (max rx swidth) (max by (+ y sheight))
                               x (+ y delta) rest))]
                   [(ht hc hb h?)
                    (let ([delta (+ swidth gapsize)])
                      (compose (cons (glayer sibling x y swidth sheight) sreyal)
                               (min lx x) ty (max rx (+ x swidth)) (max by sheight)
                               (+ x delta) y rest))]
                   [else #| deadcode |# (compose sreyal lx ty rx by x y rest)]))]
              [(or (< lx 0.0) (< ty 0.0))
               (values (max (- rx lx) 0.0) (max (- by ty) 0.0)
                       (if (< lx 0.0) (- lx) 0.0) (if (< ty 0.0) (- ty) 0.0)
                       sreyal)]
              [else (values (max (- rx lx) 0.0) (max (- by ty) 0.0) 0.0 0.0 sreyal)])))

    (glayer-group flwidth flheight
                  (let locate : (GLayer-Listof Geo) ([liat : (Listof (GLayerof Geo)) sreyal]
                                                     [tail : (Listof (GLayerof Geo)) null])
                    (if (pair? liat)
                        (locate (cdr liat)
                                (cons (geo-append-layer alignment flwidth flheight (car liat) xoff yoff) tail))
                        (cons (geo-append-layer alignment flwidth flheight base xoff yoff min-width min-height)
                              tail))))))

(define geo-superimpose-layers : (-> Geo-Pin-Anchor Geo (Listof Geo) (GLayer-Groupof Geo))
  (lambda [anchor base siblings]
    (define-values (min-width min-height) (geo-flsize base))
    (let compose ([width : Nonnegative-Flonum min-width]
                  [height : Nonnegative-Flonum min-height]
                  [siblings : (Listof Geo) siblings]
                  [sreyal : (Listof (GLayerof Geo)) null])
      (if (pair? siblings)
          (let*-values ([(self rest) (values (car siblings) (cdr siblings))]
                        [(w h) (geo-flsize self)])
            (compose (max width w) (max height h) rest
                     (cons (glayer self 0.0 0.0 w h) sreyal)))
          (glayer-group width height
                        (let locate : (GLayer-Listof Geo) ([liat : (Listof (GLayerof Geo)) sreyal]
                                                           [tail : (Listof (GLayerof Geo)) null])
                          (if (pair? liat)
                              (locate (cdr liat)
                                      (cons (geo-superimpose-layer anchor width height (car liat)) tail))
                              (cons (geo-superimpose-layer anchor width height base min-width min-height)
                                    tail))))))))
