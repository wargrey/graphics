#lang typed/racket

(provide (except-out (all-defined-out) make-pin make-append make-append* make-superimpose make-superimpose*))
(provide (rename-out [bitmap-pin-over bitmap-pin]))

(require "constructor.rkt")
(require "digitama/composite.rkt")
(require "digitama/unsafe/composite.rkt")
(require "digitama/unsafe/convert.rkt")

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
  ;;; TODO: what if one or more bmps are larger then the base one
  (lambda [x1-frac y1-frac x2-frac y2-frac bmp0 . bmps]
    (cond [(null? bmps) bmp0]
          [else (bitmap_pin* (or (bitmap-operator->integer (default-pin-operator)) CAIRO_OPERATOR_OVER)
                             (real->double-flonum x1-frac) (real->double-flonum y1-frac)
                             (real->double-flonum x2-frac) (real->double-flonum y2-frac)
                             (bitmap-surface bmp0) (map bitmap-surface bmps)
                             (bitmap-density bmp0))])))

(define bitmap-table : (->* (Integer (Listof Bitmap))
                            ((Listof Superimpose-Alignment)
                             (Listof Superimpose-Alignment)
                             (Listof Nonnegative-Real)
                             (Listof Nonnegative-Real))
                            Bitmap)
  (lambda [ncols bitmaps [col-aligns null] [row-aligns null] [col-gaps null] [row-gaps null]]
    (cond [(or (<= ncols 0) (null? bitmaps)) (bitmap-blank)]
          [else (let-values ([(maybe-nrows extra-ncols) (quotient/remainder (length bitmaps) ncols)])
                  (define nrows : Nonnegative-Fixnum (+ maybe-nrows (sgn extra-ncols)))
                  (bitmap_table (map bitmap-surface bitmaps) ncols nrows
                                (if (null? col-aligns) '(cc) col-aligns)
                                (if (null? row-aligns) '(cc) row-aligns)
                                (if (null? col-gaps) '(0.0) (map real->double-flonum col-gaps))
                                (if (null? row-gaps) '(0.0) (map real->double-flonum col-gaps))
                                (bitmap-density (car bitmaps))))])))

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
