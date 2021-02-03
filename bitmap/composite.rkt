#lang typed/racket

(provide (except-out (all-defined-out) make-pin make-append make-append* make-superimpose make-superimpose*))
(provide (rename-out [bitmap-pin-over bitmap-pin]))

(require "constructor.rkt")
(require "digitama/composite.rkt")
(require "digitama/unsafe/composite.rkt")
(require "digitama/unsafe/convert.rkt")

(require (for-syntax racket/base))

;;; TODO
; deal with compositions with negative parameters.
; `flomap` will recalculate the locations,
; `pict` simply drop parts outside boundary.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-pin-operator : (Parameterof Symbol) (make-parameter 'over))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-composite : (case-> [Symbol Bitmap Complex Bitmap -> Bitmap]
                                   [Symbol Bitmap Real Real Bitmap -> Bitmap]
                                   [Symbol Bitmap Complex Bitmap Complex -> Bitmap]
                                   [Symbol Bitmap Real Real Bitmap Real Real -> Bitmap])
  (case-lambda
    [(op bmp1 x/pt y/bmp2 bmp2/pt)
     (cond [(real? y/bmp2) (bitmap-composite op bmp1 x/pt y/bmp2 bmp2/pt 0.0 0.0)]
           [else (bitmap-composite op bmp1 (- x/pt bmp2/pt) y/bmp2)])]
    [(op bmp1 pt bmp2)
     (bitmap_composite (or (bitmap-operator->integer op) CAIRO_OPERATOR_OVER)
                       (bitmap-surface bmp1) (bitmap-surface bmp2)
                       (real->double-flonum (real-part pt)) (real->double-flonum (imag-part pt))
                       (bitmap-density bmp1))]
    [(op bmp1 x1 y1 bmp2 x2 y2)
     (bitmap_composite (or (bitmap-operator->integer op) CAIRO_OPERATOR_OVER)
                       (bitmap-surface bmp1) (bitmap-surface bmp2)
                       (- (real->double-flonum x1) (real->double-flonum x2))
                       (- (real->double-flonum y1) (real->double-flonum y2))
                       (bitmap-density bmp1))]))

(define bitmap-pin* : (-> Real Real Real Real Bitmap Bitmap * Bitmap)
  (lambda [x1-frac y1-frac x2-frac y2-frac bmp0 . bmps]
    (cond [(null? bmps) bmp0]
          [else (bitmap_pin* (or (bitmap-operator->integer (default-pin-operator)) CAIRO_OPERATOR_OVER)
                             (real->double-flonum x1-frac) (real->double-flonum y1-frac)
                             (real->double-flonum x2-frac) (real->double-flonum y2-frac)
                             (bitmap-surface bmp0) (map bitmap-surface bmps)
                             (bitmap-density bmp0))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-table : (->* (Integer (Listof Bitmap))
                            ((U (Listof Superimpose-Alignment) Superimpose-Alignment)
                             (U (Listof Superimpose-Alignment) Superimpose-Alignment)
                             (U (Listof Nonnegative-Real) Nonnegative-Real)
                             (U (Listof Nonnegative-Real) Nonnegative-Real))
                            Bitmap)
  (lambda [ncols bitmaps [col-aligns null] [row-aligns null] [col-gaps null] [row-gaps null]]
    (cond [(or (<= ncols 0) (null? bitmaps)) (bitmap-blank)]
          [else (let-values ([(maybe-nrows extra-ncols) (quotient/remainder (length bitmaps) ncols)])
                  (define nrows : Nonnegative-Fixnum (+ maybe-nrows (sgn extra-ncols)))
                  (bitmap_table (map bitmap-surface bitmaps) ncols nrows
                                (bitmap-expand-args col-aligns symbol? 'cc) (bitmap-expand-args row-aligns symbol? 'cc)
                                (bitmap-expand-args col-gaps real? 0.0 real->double-flonum) (bitmap-expand-args row-gaps real? 0.0 real->double-flonum)
                                (bitmap-density (car bitmaps))))])))

(define bitmap-table* : (->* ((Listof (Listof Bitmap)))
                            ((U (Listof Superimpose-Alignment) Superimpose-Alignment)
                             (U (Listof Superimpose-Alignment) Superimpose-Alignment)
                             (U (Listof Nonnegative-Real) Nonnegative-Real)
                             (U (Listof Nonnegative-Real) Nonnegative-Real))
                             Bitmap)
  (lambda [bitmaps [col-aligns null] [row-aligns null] [col-gaps null] [row-gaps null]]
    (define ncols : Index (apply max 0 ((inst map Index (Listof Bitmap)) length bitmaps)))
    (define nrows : Index (length bitmaps))
    (define cont : Bitmap (bitmap-blank))
    (define sont : Bitmap-Surface (bitmap-surface cont))

    (define-values (surfaces density)
      (for/fold ([surfaces : (Listof Bitmap-Surface) null]
                 [density : (Option Positive-Flonum) #false])
                ([rows : (Listof Bitmap) (in-list bitmaps)])
        (define rsize : Index (length rows))
        (values (cond [(= ncols rsize) (append surfaces (map bitmap-surface rows))]
                      [else (append surfaces (map bitmap-surface rows) (make-list (- ncols rsize) sont))])
                (or density (and (pair? rows) (bitmap-density (car rows)))))))
    
    (cond [(not density) cont]
          [else (bitmap_table surfaces ncols nrows
                              (bitmap-expand-args col-aligns symbol? 'cc) (bitmap-expand-args row-aligns symbol? 'cc)
                              (bitmap-expand-args col-gaps real? 0.0 real->double-flonum) (bitmap-expand-args row-gaps real? 0.0 real->double-flonum)
                              density)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-pyramid : (->* ((Listof Bitmap))
                              ((U (Listof Superimpose-Alignment) Superimpose-Alignment) Nonnegative-Real (Option Nonnegative-Real))
                              Bitmap)
  (lambda [bitmaps [aligns null] [sibling-gaps 0] [sub-gaps #false]]
    (cond [(null? bitmaps) (bitmap-blank)]
          [(null? (cdr bitmaps)) (car bitmaps)]
          [else (bitmap_pyramid (map bitmap-surface bitmaps)
                                (real->double-flonum sibling-gaps) (real->double-flonum (or sub-gaps sibling-gaps))
                                (bitmap-expand-args aligns symbol? 'cc)
                                (bitmap-density (car bitmaps)))])))

(define bitmap-heap : (->* ((Listof Bitmap))
                            (#:ary Positive-Index
                             Nonnegative-Real (Option Nonnegative-Real)
                             (U (Listof Superimpose-Alignment) Superimpose-Alignment))
                            Bitmap)
  (lambda [bitmaps #:ary [ary 2] [sibling-gaps 0] [sub-gaps #false] [aligns null]]
    (cond [(null? bitmaps) (bitmap-blank)]
          [(null? (cdr bitmaps)) (car bitmaps)]
          [else (let ([sfcs (map bitmap-surface bitmaps)]
                      [sibling-gapsize (real->double-flonum sibling-gaps)]
                      [sub-gapsize (real->double-flonum (or sub-gaps sibling-gaps))]
                      [aligns (bitmap-expand-args aligns symbol? 'cc)]
                      [density (bitmap-density (car bitmaps))])
                  (if (= ary 1)
                      (bitmap_table sfcs ary (length bitmaps) aligns aligns (list sibling-gapsize) (list sub-gapsize) density)
                      (bitmap_heap sfcs ary sibling-gapsize sub-gapsize aligns density)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-pin : (-> Bitmap-Composition-Operator Bitmap-Pin)
  (lambda [op]
    (case-lambda
      [(bmp1 pt bmp2) (bitmap-composite op bmp1 (real-part pt) (imag-part pt) bmp2)]
      [(bmp1 x/pt y/bmp2 bmp2/pt) (bitmap-composite op bmp1 x/pt y/bmp2 bmp2/pt)]
      [(bmp1 x1 y1 bmp2 x2 y2) (bitmap-composite op bmp1 x1 y1 bmp2 x2 y2)])))

(define make-append* : (-> Symbol (-> (Listof Bitmap) [#:gapsize Real] Bitmap))
  (lambda [alignment]
    (define blend-mode : Integer  CAIRO_OPERATOR_OVER)
    (λ [bitmaps #:gapsize [delta 0.0]]
      (cond [(null? bitmaps) (bitmap-blank)]
            [(null? (cdr bitmaps)) (car bitmaps)]
            [(and (zero? delta) (null? (cddr bitmaps)))
             (let-values ([(base bmp) (values (car bitmaps) (cadr bitmaps))])
               (case alignment
                 [(vl) (bitmap_pin blend-mode 0.0 1.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(vc) (bitmap_pin blend-mode 0.5 1.0 0.5 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(vr) (bitmap_pin blend-mode 1.0 1.0 1.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(ht) (bitmap_pin blend-mode 1.0 0.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(hc) (bitmap_pin blend-mode 1.0 0.5 0.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(hb) (bitmap_pin blend-mode 1.0 1.0 0.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [else base]))]
            [else (let-values ([(base children) (values (car bitmaps) (cdr bitmaps))])
                    (bitmap_append alignment blend-mode
                                   (bitmap-surface base) (map bitmap-surface children)
                                   (real->double-flonum delta) (bitmap-density base)))]))))

(define make-append : (-> Symbol (-> [#:gapsize Real] Bitmap * Bitmap))
  (lambda [alignment]
    (define append-apply : (-> (Listof Bitmap) [#:gapsize Real] Bitmap) (make-append* alignment))
    (λ [#:gapsize [delta 0.0] . bitmaps] (append-apply #:gapsize delta bitmaps))))

(define make-superimpose* : (-> Symbol (-> (Listof Bitmap) Bitmap))
  (lambda [alignment]
    (λ [bitmaps]
      (define blend-mode : Integer (or (bitmap-operator->integer (default-pin-operator)) CAIRO_OPERATOR_OVER))
      (cond [(null? bitmaps) (bitmap-blank)]
            [(null? (cdr bitmaps)) (car bitmaps)]
            [(null? (cddr bitmaps))
             (let-values ([(base bmp) (values (car bitmaps) (cadr bitmaps))])
               (case alignment
                 [(lt) (bitmap_pin blend-mode 0.0 0.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(lc) (bitmap_pin blend-mode 0.0 0.5 0.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(lb) (bitmap_pin blend-mode 0.0 1.0 0.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(ct) (bitmap_pin blend-mode 0.5 0.0 0.5 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(cc) (bitmap_pin blend-mode 0.5 0.5 0.5 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(cb) (bitmap_pin blend-mode 0.5 1.0 0.5 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(rt) (bitmap_pin blend-mode 1.0 0.0 1.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(rc) (bitmap_pin blend-mode 1.0 0.5 1.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [(rb) (bitmap_pin blend-mode 1.0 1.0 1.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
                 [else base]))]
            [else (bitmap_superimpose alignment blend-mode (map bitmap-surface bitmaps) (bitmap-density (car bitmaps)))]))))

(define make-superimpose : (-> Symbol (-> Bitmap * Bitmap))
  (lambda [alignment]
    (define superimpose-apply : (-> (Listof Bitmap) Bitmap) (make-superimpose* alignment))
    (λ bitmaps (superimpose-apply bitmaps))))

(define-combiner
  [make-append       "bitmap-~a-append"       (vl vc vr ht hc hb)]
  [make-append*      "bitmap-~a-append*"      (vl vc vr ht hc hb)]
  [make-superimpose  "bitmap-~a-superimpose"  (lt lc lb ct cc cb rt rc rb)]
  [make-superimpose* "bitmap-~a-superimpose*" (lt lc lb ct cc cb rt rc rb)])

(define bitmap-pin-over : Bitmap-Pin (make-pin 'over))
(define bitmap-pin-under : Bitmap-Pin (make-pin 'dest-over))
