#lang typed/racket/base

(provide (all-defined-out))
(provide expr:matrix? Expr:Matrix)
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/matrix/types.rkt"))
(provide (all-from-out "digitama/matrix/style.rkt"))
(provide (all-from-out "digitama/matrix/interface.rkt"))

(require digimon/digitama/unsafe/ops)
(require digimon/metrics)

(require racket/case)
(require racket/list)
(require racket/vector)

(require "digitama/base.rkt")
(require "digitama/matrix/dc.rkt")
(require "digitama/matrix/self.rkt")
(require "digitama/matrix/types.rkt")
(require "digitama/matrix/style.rkt")
(require "digitama/matrix/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (M) $matrix
  (lambda [#:id [id0 : (Option Symbol) #false]
           #:header-gap [hgap : Complex 0.0] #:gap [egap : Complex 0.0]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Spacing) #false]
           #:padding [padding : (Option Geo-Spacing) #false]
           #:desc [entry-desc : (Option (Mtx-Entry M)) #false]
           #:slot-backstop [backstop : Mtx-Backstop-Style (make-mtx-backstop-style)]
           #:corner-desc [corner-desc : Geo-Maybe-Rich-Text #false]
           #:header-desc [header-desc : (Option Mtx-Headers) #false]
           #:row-desc [rheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-desc [cheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-header-rotate [col-header-angle : Real -pi/2]
           #:col-header-top? [col-header-top? : Boolean #true]
           #:λslot [make-entry-slot : (Option (Mtx-Entry->Slot M)) #false]
           #:λheader-slot [make-header-slot : (Option Mtx-Header->Slot) #false]
           #:λstyle [make-entry-style : (Option (Mtx-Style-Make Mtx-Entry-Style)) (default-mtx-entry-style-make)]
           #:λmask-style [make-mask-style : (Option (Mtx-Style-Make Mtx-Mask-Style)) (default-mtx-mask-style-make)]
           #:λhole-style [make-hole-style : (Option (Mtx-Style-Make Mtx-Hole-Style)) (default-mtx-hole-style-make)]
           #:mask? [mask? : (U Boolean Mtx-Mask) #false]
           #:hole? [hole? : (Option (-> M Any)) #false]
           [mtx : ($Matrixof M)]
           [cell-width : Real 32.0]
           [cell-height : Real+% cell-width]] : Expr:Matrix
    (parameterize ([default-mtx-entry-style-make make-entry-style]
                   [default-mtx-mask-style-make make-mask-style]
                   [default-mtx-hole-style-make make-hole-style])
      (define-values (flcwidth flcheight) (~extent cell-width cell-height))
      (define id (or id0 (gensym 'dia:mtx:)))
      (define nrows : Index (if (vector? mtx) (vector-length mtx) (length mtx)))
      (define ncols : Index
        (for/fold ([ncols : Index 0])
                  ([row (if (vector? mtx) (in-vector mtx) (in-list mtx))])
          (define rlen (if (list? row) (length row) (vector-length row)))
          (if (> rlen ncols) rlen ncols)))

      (define (mtx-header [desc : Geo-Maybe-Rich-Text] [type : Mtx-Block-Type] [idx : Index]) : (Option Expr:Slot)
        (define-values (self-id indices)
          (case/eq type
            [(rhdr)     (values (expr-slot-cell-id id type idx   0) (mtx-hdr idx   0 'rc))]
            [(chdr)     (values (expr-slot-cell-id id type   0 idx) (mtx-hdr 0   idx (if col-header-top? 'cb 'ct)))]
            [else #;cnr (values (expr-slot-cell-id id type idx idx) (mtx-hdr idx idx 'cc))]))
        (define style (cons (dia-mtx-header-style-make self-id type indices) backstop))
        
        (dia-mtx-slot-make self-id (void) style indices
                           (cond [(or (not desc) (void? desc)) #false]
                                 [(geo? desc) desc]
                                 [else (expr-slot-text-term desc style #:id self-id)])
                           flcwidth flcheight
                           (and (eq? type 'chdr)
                                (* (real->double-flonum col-header-angle)
                                   (if (or col-header-top?) 1.0 -1.0)))
                           make-header-slot default-mtx-header-fallback-construct))

      (define (mtx-body [raw : M] [type : Mtx-Block-Type] [r : Index] [c : Index] [idx : Index]) : (Option Expr:Slot)
        (define-values (self-id indices) (values (expr-slot-cell-id id type r c) (mtx-indices #:row r #:col c #:ordinal idx)))
        (define style (cons (dia-mtx-style-make self-id raw type indices) backstop))
        (define term : (Option Geo)
          (and (eq? type 'entry)
               (let datum->geo ([desc : (U Geo-Maybe-Rich-Text M) (if (and entry-desc) (entry-desc raw style indices) raw)])
                 (cond [(or (not desc) (geo? desc)) desc]
                       [(geo-rich-text? desc) (expr-slot-text-term desc style #:id self-id)]
                       [(void? desc) (if (void? raw) #false (datum->geo raw))]
                       [else (expr-slot-text-term (format "~a" desc) style #:id self-id)]))))
        
        ((inst dia-mtx-slot-make M Mtx-Indices) self-id raw style indices term flcwidth flcheight #false
                                                make-entry-slot default-mtx-entry-fallback-construct))

      (define row-desc : (Option Mtx-Static-Headers) (let ([desc (or rheader-desc header-desc)]) (if (procedure? desc) #false desc)))
      (define col-desc : (Option Mtx-Static-Headers) (let ([desc (or cheader-desc header-desc)]) (if (procedure? desc) #false desc)))
      
      (define row-headers : (Vectorof (Option Expr:Slot))
        (cond [(procedure? rheader-desc) (build-vector nrows (λ [[idx : Index]] (let ([r (unsafe-idx+ idx 1)]) (mtx-header (rheader-desc r) 'rhdr r))))]
              [(procedure? header-desc) (build-vector nrows (λ [[idx : Index]] (let ([r (unsafe-idx+ idx 1)]) (mtx-header (header-desc r 0) 'rhdr r))))]
              [(string? row-desc) (build-vector nrows (λ [[idx : Index]] (let ([r (unsafe-idx+ idx 1)]) (mtx-header (format row-desc r) 'rhdr r))))]
              [(and row-desc)
               (for/vector : (Vectorof (Option Expr:Slot)) ([bdy (if (list? row-desc) (in-list row-desc) (in-vector row-desc))]
                                                                 [idx (in-range 1 (add1 nrows))] #:when (index? idx))
                 (mtx-header bdy 'rhdr idx))]
              [else #()]))
      
      (define col-headers : (Listof (Option Expr:Slot))
        (cond [(procedure? cheader-desc) (build-list ncols (λ [[idx : Index]] (let ([c (unsafe-idx+ idx 1)]) (mtx-header (cheader-desc c) 'chdr c))))]
              [(procedure? header-desc) (build-list ncols (λ [[idx : Index]] (let ([c (unsafe-idx+ idx 1)]) (mtx-header (header-desc 0 c) 'chdr c))))]
              [(string? col-desc) (build-list ncols (λ [[idx : Index]] (let ([c (unsafe-idx+ idx 1)]) (mtx-header (format col-desc c) 'chdr c))))]
              [(and col-desc)
               (for/list : (Listof (Option Expr:Slot)) ([bdy (if (list? col-desc) (in-list col-desc) (in-vector col-desc))]
                                                        [idx (in-range 1 (add1 ncols))] #:when (index? idx))
                 (mtx-header bdy 'chdr idx))]
              [else null]))

      (define nrowhdrs : Index (vector-count values row-headers))
      (define ncolhdrs : Index (count values col-headers))

      (define entries : (Listof (Listof (Option Expr:Slot)))
        (for/list ([row (if (vector? mtx) (in-vector mtx) (in-list mtx))]
                   [r (in-range nrows)] #:when (index? r))
          (define row-idx0 (unsafe-idx* r ncols))
          (define row-entries : (Listof (Option Expr:Slot))
            (for/list ([dat (if (list? row) (in-list row) (in-vector row))]
                       [c (in-range ncols)] #:when (index? c))
              (define idx (unsafe-idx+ row-idx0 c))
              (cond [(if (boolean? mask?) mask? (mask? r c)) (mtx-body dat 'mask r c idx)]
                    [(and hole? (hole? dat)) (mtx-body dat 'hole r c idx)]
                    [(void? dat) (mtx-body dat 'hole r c idx)]
                    [else (mtx-body dat 'entry r c idx)])))

          (cond [(= nrowhdrs 0) row-entries]
                [(< r nrowhdrs) (cons (vector-ref row-headers r) row-entries)]
                [else (cons #false row-entries)])))

      (define-values (chgap rhgap) (if (real? hgap) (values hgap hgap) (values (real-part hgap) (imag-part hgap))))
      (define-values (cegap regap) (if (real? egap) (values egap egap) (values (real-part egap) (imag-part egap))))

      (if (and col-header-top?)
          (make-expr:matrix (cond [(and (> nrowhdrs 0) (> ncolhdrs 0))
                                   (let ([corner (mtx-header corner-desc 'cnr 0)])
                                     (cons (cons corner col-headers) entries))]
                                  [(> ncolhdrs 0) (cons col-headers entries)]
                                  [else entries])
                            id bdr bg margin padding
                            '(rc) '(cb)
                            (if (> nrowhdrs 0) (list chgap cegap) (list cegap))
                            (if (> ncolhdrs 0) (list rhgap regap) (list regap)))
          (make-expr:matrix (cond [(and (> nrowhdrs 0) (> ncolhdrs 0))
                                   (let ([corner (mtx-header corner-desc 'cnr 0)])
                                     (append entries (list (cons corner col-headers))))]
                                  [(> ncolhdrs 0) (append entries (list col-headers))]
                                  [else entries])
                            id bdr bg margin padding
                            '(rc) '(ct)
                            (if (> nrowhdrs 0) (list chgap cegap) (list cegap))
                            (cond [(= nrows 0) null]
                                  [(= ncolhdrs 0) (list regap)]
                                  [else (append (make-list (sub1 nrows) regap)
                                                (list rhgap))]))))))

(define #:forall (M) $array
  (lambda [#:id [id0 : (Option Symbol) #false]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:header-gap [hgap : Complex 0.0] #:gap [egap : Complex 0.0]
           #:desc [desc-value : (Option (Mtx-Entry M)) #false]
           #:corner-desc [corner-desc : (Option Geo-Maybe-Rich-Text) #false]
           #:header-desc [header-desc : (Option Mtx-Headers) #false]
           #:row-desc [rheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-desc [cheader-desc : (Option Mtx-Spec-Headers) #false]
           #:col-header-rotate [col-header-angle : Real -pi/2]
           #:col-header-top? [col-header-top? : Boolean #true]
           #:λslot [make-entry-slot : (Option (Mtx-Entry->Slot M)) #false]
           #:λheader-slot [make-header-slot : (Option Mtx-Header->Slot) #false]
           #:λstyle [make-entry-style : (Option (Mtx-Style-Make Mtx-Entry-Style)) (default-mtx-entry-style-make)]
           #:λmask-style [make-mask-style : (Option (Mtx-Style-Make Mtx-Mask-Style)) (default-mtx-mask-style-make)]
           #:λhole-style [make-hole-style : (Option (Mtx-Style-Make Mtx-Hole-Style)) (default-mtx-hole-style-make)]
           #:mask? [mask? : (U Boolean Mtx-Mask) #false]
           #:hole? [hole? : (Option (-> M Any)) #false]
           #:ncols [ncols0 : Integer 0]
           [array : ($Arrayof M)]
           [cell-width : Real]
           [cell-height : Real+% cell-width]] : Expr:Matrix
    (define max-ncols : Index (if (list? array) (length array) (vector-length array)))
    (define ncols : Index
      (cond [(> ncols0 0) (if (> ncols0 max-ncols) max-ncols ncols0)]
            [(pair? cheader-desc) (length cheader-desc)]
            [(and (vector? cheader-desc) (positive? (vector-length cheader-desc))) (vector-length cheader-desc)]
            [else #;'(we don't care row headers) max-ncols]))
    
    (define mtx : ($Matrixof M)
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

    ((inst $matrix M) #:id id0 #:border bdr #:background bg #:margin margin #:padding padding
                      #:desc desc-value #:corner-desc corner-desc #:header-desc header-desc
                      #:row-desc rheader-desc #:col-desc cheader-desc
                      #:col-header-rotate col-header-angle #:col-header-top? col-header-top?
                      #:λslot make-entry-slot #:λheader-slot make-header-slot
                      #:λstyle make-entry-style #:λmask-style make-mask-style #:λhole-style make-hole-style
                      #:header-gap hgap #:gap egap #:mask? mask? #:hole? hole?
                      mtx cell-width cell-height)))
