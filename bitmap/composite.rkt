#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator geo-pin-operators)
(provide (rename-out [bitmap-pin-over bitmap-pin]))
(provide (rename-out [bitmap-ct-superimpose bitmap-n-superimpose] [bitmap-ct-superimpose* bitmap-n-superimpose*]
                     [bitmap-cb-superimpose bitmap-s-superimpose] [bitmap-cb-superimpose* bitmap-s-superimpose*]
                     [bitmap-lc-superimpose bitmap-w-superimpose] [bitmap-lc-superimpose* bitmap-w-superimpose*]
                     [bitmap-rc-superimpose bitmap-e-superimpose] [bitmap-rc-superimpose* bitmap-e-superimpose*])
         (rename-out [bitmap-lt-superimpose bitmap-wn-superimpose] [bitmap-lt-superimpose* bitmap-wn-superimpose*]
                     [bitmap-rt-superimpose bitmap-en-superimpose] [bitmap-rt-superimpose* bitmap-en-superimpose*]
                     [bitmap-lb-superimpose bitmap-ws-superimpose] [bitmap-lb-superimpose* bitmap-ws-superimpose*]
                     [bitmap-rb-superimpose bitmap-es-superimpose] [bitmap-rb-superimpose* bitmap-es-superimpose*]))

(require "constructor.rkt")
(require "digitama/convert.rkt")
(require "digitama/unsafe/composite.rkt")

(require racket/math)
(require racket/list)

(require geofun/paint)
(require geofun/digitama/composite)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/table)
(require geofun/digitama/unsafe/visual/ctype)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-composite : (->* (Bitmap Real Real Bitmap) (Real Real #:operator Geo-Pin-Operator) Bitmap)
  (lambda [bmp1 x1 y1 bmp2 [x2 0.0] [y2 0.0] #:operator [op (default-pin-operator)]]
    (bitmap_composite (geo-select-operator op default-pin-operator)
                      (bitmap-surface bmp1) (bitmap-surface bmp2)
                      (- (real->double-flonum x1) (real->double-flonum x2))
                      (- (real->double-flonum y1) (real->double-flonum y2))
                      (bitmap-density bmp1))))

(define-pin bitmap-pin-over  #:-> Bitmap #:as bitmap-composite #:with 'over)
(define-pin bitmap-pin-under #:-> Bitmap #:as bitmap-composite #:with 'dest-over)

(define bitmap-pin* : (-> Real Real Real Real Bitmap [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (lambda [#:operator [op (default-pin-operator)] x1-frac y1-frac x2-frac y2-frac bmp0 . bmps]
    (cond [(null? bmps) bmp0]
          [(null? (cdr bmps))
           (bitmap_pin (geo-select-operator op default-pin-operator)
                       (real->double-flonum x1-frac) (real->double-flonum y1-frac)
                       (real->double-flonum x2-frac) (real->double-flonum y2-frac)
                       (bitmap-surface bmp0) (bitmap-surface (car bmps))
                       (bitmap-density bmp0))]
          [else
           (bitmap_pin* (geo-select-operator op default-pin-operator)
                        (real->double-flonum x1-frac) (real->double-flonum y1-frac)
                        (real->double-flonum x2-frac) (real->double-flonum y2-frac)
                        (bitmap-surface bmp0) (map bitmap-surface bmps)
                        (bitmap-density bmp0))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-table : (->* (Integer (Listof Bitmap))
                            ((Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                            Bitmap)
  (lambda [ncols bitmaps [col-ports null] [row-ports null] [col-gaps null] [row-gaps null]]
    (cond [(or (<= ncols 0) (null? bitmaps)) (bitmap-blank)]
          [else (let-values ([(maybe-nrows extra-ncols) (quotient/remainder (length bitmaps) ncols)])
                  (define nrows : Nonnegative-Fixnum (+ maybe-nrows (sgn extra-ncols)))
                  (bitmap_table (map bitmap-surface bitmaps) ncols nrows
                                (geo-config-expand col-ports ncols 'cc) (geo-config-expand row-ports nrows 'cc)
                                (geo-config-expand col-gaps ncols 0.0 real->double-flonum) (geo-config-expand row-gaps nrows 0.0 real->double-flonum)
                                (bitmap-density (car bitmaps))))])))

(define bitmap-table* : (->* ((Listof (Listof (Option Bitmap))))
                             ((Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                             Bitmap)
  (lambda [bitmaps [col-ports null] [row-ports null] [col-gaps null] [row-gaps null]]
    (define ncols : Index (apply max 0 ((inst map Index (Listof (Option Bitmap))) length bitmaps)))
    (define nrows : Index (length bitmaps))
    (define cont : Bitmap (bitmap-blank))
    (define sont : Bitmap-Surface (bitmap-surface cont))

    (define (bitmap-surface* [bmp : (Option Bitmap)]) : Bitmap-Surface
      (if (not bmp) sont (bitmap-surface bmp)))

    (define-values (surfaces density)
      (for/fold ([surfaces : (Listof Bitmap-Surface) null]
                 [density : (Option Positive-Flonum) #false])
                ([rows : (Listof (Option Bitmap)) (in-list bitmaps)])
        (define rsize : Index (length rows))
        (values (cond [(= ncols rsize) (append surfaces (map bitmap-surface* rows))]
                      [else (append surfaces (map bitmap-surface* rows) (make-list (- ncols rsize) sont))])
                (or density (let ([fst (findf bitmap? rows)])
                              (and fst (bitmap-density fst)))))))
    
    (cond [(not density) cont]
          [else (bitmap_table surfaces ncols nrows
                              (geo-config-expand col-ports ncols 'cc) (geo-config-expand row-ports nrows 'cc)
                              (geo-config-expand col-gaps ncols 0.0 real->double-flonum) (geo-config-expand row-gaps nrows 0.0 real->double-flonum)
                              density)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-pyramid : (->* ((Listof Bitmap))
                              (Nonnegative-Real (Option Nonnegative-Real) (Geo-Config-Argof Geo-Pin-Port))
                              Bitmap)
  (lambda [bitmaps [sub-gaps 0] [sibling-gaps #false] [aligns null]]
    (cond [(null? bitmaps) (bitmap-blank)]
          [(null? (cdr bitmaps)) (car bitmaps)]
          [else (bitmap_pyramid (map bitmap-surface bitmaps)
                                (real->double-flonum sub-gaps) (real->double-flonum (or sibling-gaps (* sub-gaps 1.618)))
                                (geo-config-expand aligns (length bitmaps) 'cc)
                                (bitmap-density (car bitmaps)))])))

; TODO: find a better ratio between subtree gapsize and the leaf one
(define bitmap-heap : (->* ((Listof Bitmap))
                            (#:ary Positive-Index Nonnegative-Real (Option Nonnegative-Real) (Geo-Config-Argof Geo-Pin-Port))
                            Bitmap)
  (lambda [bitmaps #:ary [ary 2] [sub-gaps 0] [sibling-gaps #false] [aligns null]]
    (cond [(null? bitmaps) (bitmap-blank)]
          [(null? (cdr bitmaps)) (car bitmaps)]
          [else (let ([sfcs (map bitmap-surface bitmaps)]
                      [sub-gapsize (real->double-flonum sub-gaps)]
                      [sibling-gapsize (real->double-flonum (or sibling-gaps (* sub-gaps 0.618)))]
                      [aligns (geo-config-expand aligns (length bitmaps) 'cc)]
                      [density (bitmap-density (car bitmaps))])
                  (if (= ary 1)
                      (bitmap_table sfcs 1 (length bitmaps) aligns aligns (vector sibling-gapsize) (vector sub-gapsize) density)
                      (bitmap_heap sfcs ary sub-gapsize sibling-gapsize aligns density)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-combiner "bitmap-~a-append*" #:-> (Bitmap [#:operator Geo-Pin-Operator] [#:gapsize Real])
  #:with alignment bitmaps op->integer [#:operator [op (default-pin-operator)] #:gapsize [delta 0.0]]
  #:empty (bitmap-blank)
  #:operator-expr (geo-select-operator op default-pin-operator)
  #:short-path #:for base bmp #:if (zero? delta)
  ([(vl) (bitmap_pin op->integer 0.0 1.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(vc) (bitmap_pin op->integer 0.5 1.0 0.5 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(vr) (bitmap_pin op->integer 1.0 1.0 1.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(ht) (bitmap_pin op->integer 1.0 0.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(hc) (bitmap_pin op->integer 1.0 0.5 0.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(hb) (bitmap_pin op->integer 1.0 1.0 0.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))])
  #:do (let ([base (car bitmaps)])
         (bitmap_append alignment op->integer
                        (bitmap-surface base) (map bitmap-surface (cdr bitmaps))
                        (real->double-flonum delta) (bitmap-density base))))

(define-combiner "bitmap-~a-superimpose*" #:-> (Bitmap [#:operator Geo-Pin-Operator])
  #:with alignment bitmaps op->integer [#:operator [op (default-pin-operator)]]
  #:empty (bitmap-blank)
  #:operator-expr (geo-select-operator op default-pin-operator)
  #:short-path #:for base bmp #:if #true
  ([(lt) (bitmap_pin op->integer 0.0 0.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(lc) (bitmap_pin op->integer 0.0 0.5 0.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(lb) (bitmap_pin op->integer 0.0 1.0 0.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(ct) (bitmap_pin op->integer 0.5 0.0 0.5 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(cc) (bitmap_pin op->integer 0.5 0.5 0.5 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(cb) (bitmap_pin op->integer 0.5 1.0 0.5 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(rt) (bitmap_pin op->integer 1.0 0.0 1.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(rc) (bitmap_pin op->integer 1.0 0.5 1.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(rb) (bitmap_pin op->integer 1.0 1.0 1.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))])
  #:do (bitmap_superimpose alignment op->integer (map bitmap-surface bitmaps) (bitmap-density (car bitmaps))))

(define bitmap-vl-append : (-> [#:operator Geo-Pin-Operator] [#:gapsize Real] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . bitmaps]
    (bitmap-vl-append* #:operator op #:gapsize delta bitmaps)))

(define bitmap-vc-append : (-> [#:operator Geo-Pin-Operator] [#:gapsize Real] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . bitmaps]
    (bitmap-vc-append* #:operator op #:gapsize delta bitmaps)))

(define bitmap-vr-append : (-> [#:operator Geo-Pin-Operator] [#:gapsize Real] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . bitmaps]
    (bitmap-vr-append* #:operator op #:gapsize delta bitmaps)))

(define bitmap-ht-append : (-> [#:operator Geo-Pin-Operator] [#:gapsize Real] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . bitmaps]
    (bitmap-ht-append* #:operator op #:gapsize delta bitmaps)))

(define bitmap-hc-append : (-> [#:operator Geo-Pin-Operator] [#:gapsize Real] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . bitmaps]
    (bitmap-hc-append* #:operator op #:gapsize delta bitmaps)))

(define bitmap-hb-append : (-> [#:operator Geo-Pin-Operator] [#:gapsize Real] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . bitmaps]
    (bitmap-hb-append* #:operator op #:gapsize delta bitmaps)))

(define bitmap-lt-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-lt-superimpose* #:operator op bitmaps)))

(define bitmap-ct-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-ct-superimpose* #:operator op bitmaps)))

(define bitmap-rt-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-rt-superimpose* #:operator op bitmaps)))

(define bitmap-lc-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-lc-superimpose* #:operator op bitmaps)))

(define bitmap-cc-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-cc-superimpose* #:operator op bitmaps)))

(define bitmap-rc-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-rc-superimpose* #:operator op bitmaps)))

(define bitmap-lb-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-lb-superimpose* #:operator op bitmaps)))

(define bitmap-cb-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-cb-superimpose* #:operator op bitmaps)))

(define bitmap-rb-superimpose : (-> [#:operator Geo-Pin-Operator] Bitmap * Bitmap)
  (λ [#:operator [op (default-pin-operator)] . bitmaps]
    (bitmap-rb-superimpose* #:operator op bitmaps)))
