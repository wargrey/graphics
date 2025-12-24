#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/matrix/self.rkt"))
(provide (all-from-out "digitama/matrix/style.rkt"))
(provide (all-from-out "digitama/matrix/interface.rkt"))

(require digimon/digitama/unsafe/ops)
(require digimon/metrics)

(require racket/case)
(require racket/list)
(require racket/vector)

(require geofun/paint)
(require geofun/composite)
(require geofun/digitama/convert)
(require geofun/digitama/dc/composite)

(require "digitama/base.rkt")
(require "digitama/matrix/dc.rkt")
(require "digitama/matrix/self.rkt")
(require "digitama/matrix/style.rkt")
(require "digitama/matrix/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (M) dia-matrix
  (lambda [#:id [id0 : (Option Symbol) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:desc [entry-desc : (Option (Mtx-Entry M)) #false]
           #:corner-desc [corner-desc : Mtx-Maybe-Desc #false]
           #:header-desc [header-desc : (Option Mtx-Headers) #false]
           #:row-desc [rheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-desc [cheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-header-rotate [col-header-angle : Real 0.0]
           #:col-header-top? [col-header-top? : Boolean #true]
           #:λblock [make-entry-block : (Option (Mtx-Entry->Block M)) #false]
           #:λheader-block [make-header-block : (Option Mtx-Header->Block) #false]
           #:λstyle [make-entry-style : (Option (Mtx-Style-Make Mtx-Entry-Style)) (default-mtx-entry-style-make)]
           #:λmask-style [make-mask-style : (Option (Mtx-Style-Make Mtx-Mask-Style)) (default-mtx-mask-style-make)]
           #:λhole-style [make-hole-style : (Option (Mtx-Style-Make Mtx-Hole-Style)) (default-mtx-hole-style-make)]
           #:header-gap [hgap : Complex 0.0] #:gap [egap : Complex 0.0]
           #:mask? [mask? : (U Boolean Mtx-Mask) #false]
           #:hole? [hole? : (-> M Any) void?]
           [mtx : (Dia-Matrixof M)]
           [cell-width : (Option Real) ((default-mtx-entry-block-width))]
           [cell-height : (Option Real+%) cell-width]] : Geo:Table
    (define flcwidth (and cell-width (> cell-width 0.0) (real->double-flonum cell-width)))
    (define flcheight (and cell-height
                           (cond [(real? cell-height) (and (> cell-height 0.0) (real->double-flonum cell-height))]
                                 [else (and flcwidth (~length cell-height flcwidth))])))
    
    (parameterize ([default-dia-block-base-style make-mtx-fallback-style]
                   [default-mtx-entry-style-make make-entry-style]
                   [default-mtx-mask-style-make make-mask-style]
                   [default-mtx-hole-style-make make-hole-style]
                   [default-mtx-entry-block-width flcwidth]
                   [default-mtx-entry-block-height flcheight])
      (define id (or id0 (gensym 'dia:mtx:)))
      
      (define nrows : Index (if (vector? mtx) (vector-length mtx) (length mtx)))
      (define ncols : Index
        (for/fold ([ncols : Index 0])
                  ([row (if (vector? mtx) (in-vector mtx) (in-list mtx))])
          (define rlen (if (list? row) (length row) (vector-length row)))
          (if (> rlen ncols) rlen ncols)))

      (define (mtx-header [desc : Mtx-Maybe-Desc] [type : Mtx-Block-Type] [idx : Index]) : (Option Dia:Block)
        (define-values (self-id indices)
          (case/eq type
            [(rhdr)     (values (dia-block-cell-id id type idx   0) (list idx   0 idx))]
            [(chdr)     (values (dia-block-cell-id id type   0 idx) (list 0   idx idx))]
            [else #;cnr (values (dia-block-cell-id id type idx idx) (list idx idx idx))]))
        (define style (default-mtx-block-identify (void) type indices))
        
        (dia-mtx-block-make self-id style indices
                            (cond [(or (not desc) (void? desc)) #false]
                                  [(geo? desc) desc]
                                  [else (dia-block-text-brief desc style #:id self-id)])
                            (and (eq? type 'chdr) (* (real->double-flonum col-header-angle)
                                                     (if (or col-header-top?) 1.0 -1.0)))
                            make-header-block default-mtx-header-fallback-construct))

      (define (mtx-body [datum : M] [desc : (U Mtx-Maybe-Desc (Boxof (Mtx-Entry M)))]
                        [type : Mtx-Block-Type] [r : Index] [c : Index] [idx : Index]) : (Option Dia:Block)
        (define-values (self-id indices) (values (dia-block-cell-id id type r c) (list r c idx)))
        (define style (default-mtx-block-identify datum type indices))
        
        ((inst dia-mtx-block-make (Pairof Symbol M))
         (cons self-id datum) style indices
         (let datum->geo ([desc : (U Mtx-Maybe-Desc (Boxof (Mtx-Entry M))) desc])
           (cond [(or (not desc) (void? desc)) #false]
                 [(geo? desc) desc]
                 [(box? desc) (datum->geo ((unbox desc) datum style indices))]
                 [else (dia-block-text-brief desc style #:id self-id)]))
         #false make-entry-block default-mtx-entry-fallback-construct))

      (define row-desc : (Option Mtx-Static-Headers) (let ([desc (or rheader-desc header-desc)]) (if (procedure? desc) #false desc)))
      (define col-desc : (Option Mtx-Static-Headers) (let ([desc (or cheader-desc header-desc)]) (if (procedure? desc) #false desc)))
      
      (define row-headers : (Vectorof (Option Dia:Block))
        (cond [(procedure? rheader-desc) (build-vector nrows (λ [[idx : Index]] (let ([r (unsafe-idx+ idx 1)]) (mtx-header (rheader-desc r) 'rhdr r))))]
              [(procedure? header-desc) (build-vector nrows (λ [[idx : Index]] (let ([r (unsafe-idx+ idx 1)]) (mtx-header (header-desc r 0) 'rhdr r))))]
              [(string? row-desc) (build-vector nrows (λ [[idx : Index]] (let ([r (unsafe-idx+ idx 1)]) (mtx-header (format row-desc r) 'rhdr r))))]
              [(and row-desc)
               (for/vector : (Vectorof (Option Dia:Block)) ([bdy (if (list? row-desc) (in-list row-desc) (in-vector row-desc))]
                                                            [idx (in-range 1 (add1 nrows))] #:when (index? idx))
                 (mtx-header bdy 'rhdr idx))]
              [else #()]))
      
      (define col-headers : (Listof (Option Dia:Block))
        (cond [(procedure? cheader-desc) (build-list ncols (λ [[idx : Index]] (let ([c (unsafe-idx+ idx 1)]) (mtx-header (cheader-desc c) 'chdr c))))]
              [(procedure? header-desc) (build-list ncols (λ [[idx : Index]] (let ([c (unsafe-idx+ idx 1)]) (mtx-header (header-desc 0 c) 'chdr c))))]
              [(string? col-desc) (build-list ncols (λ [[idx : Index]] (let ([c (unsafe-idx+ idx 1)]) (mtx-header (format col-desc c) 'chdr c))))]
              [(and col-desc)
               (for/list : (Listof (Option Dia:Block)) ([bdy (if (list? col-desc) (in-list col-desc) (in-vector col-desc))]
                                                             [idx (in-range 1 (add1 ncols))] #:when (index? idx))
                 (mtx-header bdy 'chdr idx))]
              [else null]))

      (define nrowhdrs : Index (vector-count values row-headers))
      (define ncolhdrs : Index (count values col-headers))

      (define entries : (Listof (Listof (Option Dia:Block)))
        (for/list ([row (if (vector? mtx) (in-vector mtx) (in-list mtx))]
                   [r (in-range nrows)] #:when (index? r))
          (define row-idx0 (unsafe-idx* r ncols))
          (define row-entries : (Listof (Option Dia:Block))
            (for/list ([dat (if (list? row) (in-list row) (in-vector row))]
                       [c (in-range ncols)] #:when (index? c))
              (define idx (unsafe-idx+ row-idx0 c))
              (cond [(if (boolean? mask?) mask? (mask? r c)) (mtx-body dat #false 'mask r c idx)]
                    [(hole? dat) (mtx-body dat #false 'hole r c idx)]
                    [(geo? dat) (mtx-body dat dat 'entry r c idx)]
                    [(or entry-desc) (mtx-body dat (box entry-desc) 'entry r c idx)]
                    [else (mtx-body dat (format "~a" dat) 'entry r c idx)])))

          (cond [(= nrowhdrs 0) row-entries]
                [(< r nrowhdrs) (cons (vector-ref row-headers r) row-entries)]
                [else (cons #false row-entries)])))

      (define-values (chgap rhgap) (if (real? hgap) (values hgap hgap) (values (real-part hgap) (imag-part hgap))))
      (define-values (cegap regap) (if (real? egap) (values egap egap) (values (real-part egap) (imag-part egap))))

      (if (and col-header-top?)
          (geo-table* #:id id #:base-operator base-op #:operator sibs-op
                      #:border bdr #:background bg #:margin margin #:padding padding
                      (cond [(and (> nrowhdrs 0) (> ncolhdrs 0))
                             (let ([corner (mtx-header corner-desc 'cnr 0)])
                               (cons (cons corner col-headers) entries))]
                            [(> ncolhdrs 0) (cons col-headers entries)]
                            [else entries])
                      '(rc) '(cb)
                      (if (> nrowhdrs 0) (list chgap cegap) (list cegap))
                      (if (> ncolhdrs 0) (list rhgap regap) (list regap)))
          (geo-table* #:id id #:base-operator base-op #:operator sibs-op
                      #:border bdr #:background bg #:margin margin #:padding padding
                      (cond [(and (> nrowhdrs 0) (> ncolhdrs 0))
                             (let ([corner (mtx-header corner-desc 'cnr 0)])
                               (append entries (list (cons corner col-headers))))]
                            [(> ncolhdrs 0) (append entries (list col-headers))]
                            [else entries])
                      '(rc) '(ct)
                      (if (> nrowhdrs 0) (list chgap cegap) (list cegap))
                      (cond [(= nrows 0) null]
                            [(= ncolhdrs 0) (list regap)]
                            [else (append (make-list (sub1 nrows) regap)
                                          (list rhgap))]))))))

(define #:forall (M) dia-array
  (lambda [#:id [id0 : (Option Symbol) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:desc [desc-value : (Option (Mtx-Entry M)) #false]
           #:corner-desc [maybe-corner-desc : (Option Mtx-Maybe-Desc) #false]
           #:header-desc [maybe-header-desc : (Option Mtx-Headers) #false]
           #:row-desc [maybe-rheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-desc [maybe-cheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-header-rotate [col-header-angle : Real 0.0]
           #:col-header-top? [col-header-top? : Boolean #true]
           #:λblock [make-entry-block : (Option (Mtx-Entry->Block M)) #false]
           #:λheader-block [make-header-block : (Option Mtx-Header->Block) #false]
           #:λstyle [make-entry-style : (Option (Mtx-Style-Make Mtx-Entry-Style)) (default-mtx-entry-style-make)]
           #:λmask-style [make-mask-style : (Option (Mtx-Style-Make Mtx-Mask-Style)) (default-mtx-mask-style-make)]
           #:λhole-style [make-hole-style : (Option (Mtx-Style-Make Mtx-Hole-Style)) (default-mtx-hole-style-make)]
           #:header-gap [hgap : Complex 0.0] #:gap [egap : Complex 0.0]
           #:mask? [mask? : (U Boolean Mtx-Mask) #false]
           #:hole? [hole? : (-> M Any) void?]
           #:ncols [ncols0 : Integer 0]
           [array : (Dia-Arrayof M)]
           [cell-width : (Option Real) ((default-mtx-entry-block-width))]
           [cell-height : (Option Real) cell-width]] : Geo:Table
    (define ncols : Index
      (cond [(and (index? ncols0) (> ncols0 0)) ncols0]
            [(list? array) (length array)]
            [else (vector-length array)]))
    
    (define mtx : (Dia-Matrixof M)
      (if (list? array)
          (let make-matrix ([arr : (Listof M) array]
                            [mtx : (Listof (Listof M)) null])
            (cond [(null? arr) (reverse mtx)]
                  [(<= (length arr) ncols) (reverse (cons arr mtx))]
                  [else (let-values ([(self rest) (split-at arr ncols)])
                          (make-matrix rest (cons self mtx)))]))
          (let make-matrix ([arr : (Vectorof M) array]
                            [mtx : (Listof (Vectorof M)) null])
            (define count (vector-length arr))
            (cond [(= count 0) (reverse mtx)]
                  [(<= count ncols) (reverse (cons arr mtx))]
                  [else (let-values ([(self rest) (vector-split-at arr ncols)])
                          (make-matrix rest (cons self mtx)))]))))

    ((inst dia-matrix M) #:id id0 #:base-operator base-op #:operator sibs-op
                         #:border bdr #:background bg #:margin margin #:padding padding
                         #:desc desc-value #:corner-desc maybe-corner-desc #:header-desc maybe-header-desc
                         #:row-desc maybe-rheader-desc #:col-desc maybe-cheader-desc
                         #:col-header-rotate col-header-angle #:col-header-top? col-header-top?
                         #:λblock make-entry-block #:λheader-block make-header-block
                         #:λstyle make-entry-style #:λmask-style make-mask-style #:λhole-style make-hole-style
                         #:header-gap hgap #:gap egap #:mask? mask? #:hole? hole?
                         mtx cell-width cell-height)))
