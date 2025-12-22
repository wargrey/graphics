#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/matrix/self.rkt"))
(provide (all-from-out "digitama/matrix/style.rkt"))
(provide (all-from-out "digitama/matrix/interface.rkt"))

(require racket/list)
(require racket/vector)

(require geofun/font)
(require geofun/paint)
(require geofun/composite)

(require geofun/digitama/base)
(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require geofun/digitama/dc/text)
(require geofun/digitama/dc/rect)

(require "digitama/base.rkt")
(require "digitama/matrix/dc.rkt")
(require "digitama/matrix/self.rkt")
(require "digitama/matrix/style.rkt")
(require "digitama/matrix/interface.rkt")
(require "digitama/matrix/identifier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (M) dia-matrix*
  (lambda [#:id [id0 : (Option Symbol) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:desc [desc-value : (Option (Dia-Matrix-Entry M)) #false]
           #:corner-desc [maybe-corner-desc : (Option Dia-Matrix-Optional-Entry) #false]
           #:header-desc [maybe-header-desc : (Option Dia-Matrix-Headers) #false]
           #:row-header-desc [maybe-rheader-desc : (Option Dia-Matrix-Sub-Headers) #false]
           #:col-header-desc [maybe-cheader-desc : (Option Dia-Matrix-Sub-Headers) #false]
           #:col-header-rotate [col-head-angle : Real 0.0]
           #:gap [gap : Real 0.0] #:row-gap [row-gap : (Option Real) #false] #:col-gap [col-gap : (Option Real) #false]
           #:λblock [make-cell : (Option Dia-Matrix-Id->Block) #false]
           #:λentry [make-entry : (Option (Dia-Matrix-Block-Style-Make DiaMtx-Entry-Style)) (default-diamtx-entry-style-make)]
           #:mask? [mask? : (Option Dia-Matrix-Mask) #false]
           #:hole? [hole? : (Option (-> M Any)) #false]
           [mtx : (Dia-Matrixof M)] [cell-width : (Option Real) #false] [cell-height : (Option Real) cell-width]] : (U Geo:Table Geo:Blank)
    (parameterize ([default-dia-block-base-style make-diamtx-fallback-style]
                   [default-diamtx-entry-style-make make-entry])
      (define id (or id0 (gensym 'dia:mtx:)))
      (define row-desc (or maybe-rheader-desc maybe-header-desc ((inst list (Option DC-Markup-Text)))))
      (define col-desc (or maybe-cheader-desc maybe-header-desc ((inst list (Option DC-Markup-Text)))))
      
      (define nrows : Index (if (vector? mtx) (vector-length mtx) (length mtx)))
      (define ncols : Index
        (for/fold ([ncols : Index 0])
                  ([row (if (vector? mtx) (in-vector mtx) (in-list mtx))])
          (define rlen (if (list? row) (length row) (vector-length row)))
          (if (> rlen ncols) rlen ncols)))
      
      (define (diamtx-cell [header : Dia-Matrix-Optional-Entry] [type : Dia-Matrix-Block-Type-Abbr]
                           [r : Index] [c : Index]
                           [cheader? : Boolean #false]) : (Option Dia:Block)
        (define self (dia-block-cell-id id type r c))
        (define style (default-diamtx-block-identify self type r c))
        (define brief
          (cond [(or (not header) (geo? header)) header]
                [else (dia-block-text-brief self header style)]))
        
        (dia-matrix-block-make id style (cons r c) cell-width cell-height
                               make-cell brief default-diamtx-block-fallback-construct
                               (and cheader? col-head-angle (real->double-flonum col-head-angle))))
      
      (define row-headers : (Vectorof (Option Dia:Block))
        (cond [(procedure? row-desc) (build-vector nrows (λ [[idx : Index]] (diamtx-cell (row-desc idx 0) 'rh idx 0)))]
              [(string? row-desc) (build-vector nrows (λ [[idx : Index]] (diamtx-cell (format row-desc idx) 'rh idx 0)))]
              [else (for/vector : (Vectorof (Option Dia:Block)) ([bdy (if (list? row-desc) (in-list row-desc) (in-vector row-desc))]
                                                                 [idx (in-range nrows)] #:when (index? idx))
                      (diamtx-cell bdy 'rh idx 0))]))
      
      (define col-headers : (Listof (Option Dia:Block))
        (cond [(procedure? col-desc) (build-list ncols (λ [[idx : Index]] (diamtx-cell (col-desc 0 idx) 'ch 0 idx #true)))]
              [(string? col-desc) (build-list ncols (λ [[idx : Index]] (diamtx-cell (format col-desc idx) 'ch 0 idx #true)))]
              [else (for/list : (Listof (Option Dia:Block)) ([bdy (if (list? col-desc) (in-list col-desc) (in-vector col-desc))]
                                                             [idx (in-range ncols)]#:when (index? idx))
                      (diamtx-cell bdy 'ch 0 idx #true))]))

      (define rheader-count : Index (vector-length row-headers))
      (define cheader-count : Index (length col-headers))
      
      (define entries : (Listof (Listof (Option Dia:Block)))
        (for/list ([row (if (vector? mtx) (in-vector mtx) (in-list mtx))]
                   [r (in-range nrows)] #:when (index? r))
          (define row-entry : (Listof (Option Dia:Block))
            (for/list ([col (if (vector? row) (in-vector row) (in-list row))]
                       [c (in-range ncols)] #:when (index? c))
              (cond [(and mask? (mask? r c)) (diamtx-cell #false 'mask r c)]
                    [(void? col) (diamtx-cell #false 'hole r c)]
                    [(and hole? (hole? col)) (diamtx-cell #false 'hole r c)]
                    [(or desc-value) (diamtx-cell (desc-value r c col) 'entry r c)]
                    [(geo? col) (diamtx-cell col 'entry r c)]
                    [else (diamtx-cell (format "~a" col) 'entry r c)])))

          (cond [(= rheader-count 0) row-entry]
                [(< r rheader-count) (cons (vector-ref row-headers r) row-entry)]
                [else (cons #false row-entry)])))
      
      (geo-table* #:id id #:base-operator base-op #:operator sibs-op
                  (cond [(and (> rheader-count 0) (> cheader-count 0))
                         (let ([corner (diamtx-cell maybe-corner-desc 'cnr 0 0)])
                           (cons (cons corner col-headers) entries))]
                        [(> cheader-count 0) (cons col-headers entries)]
                        [else entries])
                  'rc 'cb (or col-gap gap) (or row-gap gap)))))

(define #:forall (M) dia-matrix
  (lambda [#:id [id0 : (Option Symbol) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:desc [desc-value : (Option (Dia-Matrix-Entry M)) #false]
           #:corner-desc [maybe-corner-desc : (Option Dia-Matrix-Optional-Entry) #false]
           #:header-desc [maybe-header-desc : (Option Dia-Matrix-Headers) #false]
           #:row-header-desc [maybe-rheader-desc : (Option Dia-Matrix-Sub-Headers) #false]
           #:col-header-desc [maybe-cheader-desc : (Option Dia-Matrix-Sub-Headers) #false]
           #:col-header-rotate [col-head-angle : Real 0.0]
           #:gap [gap : Real 0.0] #:row-gap [row-gap : (Option Real) #false] #:col-gap [col-gap : (Option Real) #false]
           #:λblock [make-cell : (Option Dia-Matrix-Id->Block) #false]
           #:λentry [make-entry : (Option (Dia-Matrix-Block-Style-Make DiaMtx-Entry-Style)) (default-diamtx-entry-style-make)]
           #:mask? [mask? : (Option Dia-Matrix-Mask) #false]
           #:hole? [hole? : (Option (-> M Any)) #false]
           [ncols : Integer] [array : (Dia-Arrayof M)] [cell-width : (Option Real) #false] [cell-height : (Option Real) cell-width]] : (U Geo:Table Geo:Blank)
    (define mtx : (Dia-Matrixof M)
      (if (list? array)
          (let make-matrix ([arr : (Listof (U M Void)) array]
                            [mtx : (Listof (Listof (U M Void))) null])
            (cond [(null? arr) (reverse mtx)]
                  [(<= (length arr) ncols) (reverse (cons arr mtx))]
                  [else (let-values ([(self rest) (split-at arr ncols)])
                          (make-matrix rest (cons self mtx)))]))
          (let make-matrix ([arr : (Vectorof (U M Void)) array]
                            [mtx : (Listof (Vectorof (U M Void))) null])
            (define count (vector-length arr))
            (cond [(= count 0) (reverse mtx)]
                  [(<= count ncols) (reverse (cons arr mtx))]
                  [else (let-values ([(self rest) (vector-split-at arr ncols)])
                          (make-matrix rest (cons self mtx)))]))))

    ((inst dia-matrix* M) #:id id0 #:base-operator base-op #:operator sibs-op
                          #:desc desc-value #:corner-desc maybe-corner-desc
                          #:header-desc maybe-header-desc #:row-header-desc maybe-rheader-desc
                          #:col-header-desc maybe-cheader-desc #:col-header-rotate col-head-angle
                          #:gap gap #:row-gap row-gap #:col-gap col-gap
                          #:λblock make-cell #:λentry make-entry #:mask? mask? #:hole? hole?
                          mtx cell-width cell-height)))
