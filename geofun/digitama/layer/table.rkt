#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(unsafe-provide (rename-out [geo-table-layers unsafe-table-layers]
                            [geo-table-column-widths unsafe-table-column-widths]
                            [geo-table-row-heights unsafe-table-row-heights]))

(require racket/list)

(require "type.rkt")
(require "position.rkt")
(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (G) geo-table-layers : (-> (Vectorof (GLayerof G)) Positive-Index Positive-Index
                                            (Vectorof Geo-Pin-Port) (Vectorof Geo-Pin-Port) (Vectorof Flonum) (Vectorof Flonum)
                                            (Vectorof Nonnegative-Flonum) (Vectorof Nonnegative-Flonum)
                                            (GLayer-Groupof G))
  (lambda [table ncols nrows pcols prows gcols grows cwidths rheights]
    (vector-set! gcols (- ncols 1) 0.0)
    (vector-set! grows (- nrows 1) 0.0)

    (let compose-row ([width : Nonnegative-Flonum 0.0]
                      [height : Nonnegative-Flonum 0.0]
                      [row : Nonnegative-Fixnum 0]
                      [layers : (Listof (Listof (GLayerof G))) null])
      (if (< row nrows)
          (let ([hrow (vector-ref rheights row)]
                [hgap (max (vector-ref grows row) 0.0)]
                [ridx (* row ncols)])
            (let compose-col ([xoff : Nonnegative-Flonum 0.0]
                              [col : Nonnegative-Fixnum 0]
                              [rlayers : (Listof (GLayerof G)) null])
              (if (< col ncols)
                  (let ([cell (vector-ref table (+ ridx col))]
                        [wcol (vector-ref cwidths col)]
                        [wgap (max (vector-ref gcols col) 0.0)]
                        [port (geo-port-merge (vector-ref prows row) (vector-ref pcols col))])
                    (compose-col (+ xoff wcol wgap) (+ col 1)
                                 (cons (geo-superimpose-layer (geo-port-merge (vector-ref prows row) (vector-ref pcols col))
                                                              wcol hrow cell xoff height 'placeholder)
                                       rlayers)))
                  (compose-row xoff (+ height hrow hgap) (+ row 1) (cons (reverse rlayers) layers)))))

          (vector-immutable width height (assert (apply append (reverse layers)) pair?))))))

(define #:forall (G) geo-table-column-widths : (-> (Vectorof (GLayerof G)) Positive-Index Positive-Index Index (Vectorof Nonnegative-Flonum))
  (lambda [table row col max-index]
    (for/vector : (Vectorof Nonnegative-Flonum) #:length col ([c (in-list (range col))])
      (let compose : Nonnegative-Flonum ([width : Nonnegative-Flonum 0.0]
                                         [idx : Nonnegative-Fixnum c])
        (if (< idx max-index)
            (compose (max width (vector-ref (vector-ref table idx) 3)) (+ idx col))
            width)))))

(define #:forall (G) geo-table-row-heights : (-> (Vectorof (GLayerof G)) Positive-Index Positive-Index Index (Vectorof Nonnegative-Flonum))
  (lambda [table row col max-index]
    (for/vector : (Vectorof Nonnegative-Flonum) #:length row ([r (in-list (range 0 max-index col))])
      (define next-r : Nonnegative-Fixnum (+ r col))
      (if (<= next-r max-index)
          (let compose : Nonnegative-Flonum ([height : Nonnegative-Flonum 0.0]
                                             [idx : Nonnegative-Fixnum r])
            (if (< idx next-r)
                (compose (max height (vector-ref (vector-ref table idx) 4)) (+ idx 1))
                height))
          #;#:deadcode 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo->layer : (-> Geo (GLayerof Geo))
  (lambda [self]
    (define-values (w h) (geo-flsize self))
    (vector-immutable self 0.0 0.0 w h)))
