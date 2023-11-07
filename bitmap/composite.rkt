#lang typed/racket

(provide (except-out (all-defined-out) make-pin))
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
                              (Nonnegative-Real (Option Nonnegative-Real) (U (Listof Superimpose-Alignment) Superimpose-Alignment))
                              Bitmap)
  (lambda [bitmaps [sub-gaps 0] [sibling-gaps #false] [aligns null]]
    (cond [(null? bitmaps) (bitmap-blank)]
          [(null? (cdr bitmaps)) (car bitmaps)]
          [else (bitmap_pyramid (map bitmap-surface bitmaps)
                                (real->double-flonum sub-gaps) (real->double-flonum (or sibling-gaps (* sub-gaps 1.618)))
                                (bitmap-expand-args aligns symbol? 'cc)
                                (bitmap-density (car bitmaps)))])))

; TODO: find a better ratio between subtree gapsize and the leaf one
(define bitmap-heap : (->* ((Listof Bitmap))
                            (#:ary Positive-Index
                             Nonnegative-Real (Option Nonnegative-Real)
                             (U (Listof Superimpose-Alignment) Superimpose-Alignment))
                            Bitmap)
  (lambda [bitmaps #:ary [ary 2] [sub-gaps 0] [sibling-gaps #false] [aligns null]]
    (cond [(null? bitmaps) (bitmap-blank)]
          [(null? (cdr bitmaps)) (car bitmaps)]
          [else (let ([sfcs (map bitmap-surface bitmaps)]
                      [sub-gapsize (real->double-flonum sub-gaps)]
                      [sibling-gapsize (real->double-flonum (or sibling-gaps (* sub-gaps 0.618)))]
                      [aligns (bitmap-expand-args aligns symbol? 'cc)]
                      [density (bitmap-density (car bitmaps))])
                  (if (= ary 1)
                      (bitmap_table sfcs 1 (length bitmaps) aligns aligns (list sibling-gapsize) (list sub-gapsize) density)
                      (bitmap_heap sfcs ary sub-gapsize sibling-gapsize aligns density)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-pin : (-> Symbol Bitmap-Composition-Operator Bitmap-Pin)
  (lambda [name op]
    (define λpin : Bitmap-Pin
      (case-lambda
        [(bmp1 pt bmp2) (bitmap-composite op bmp1 (real-part pt) (imag-part pt) bmp2)]
        [(bmp1 x/pt y/bmp2 bmp2/pt) (bitmap-composite op bmp1 x/pt y/bmp2 bmp2/pt)]
        [(bmp1 x1 y1 bmp2 x2 y2) (bitmap-composite op bmp1 x1 y1 bmp2 x2 y2)]))
    
    (procedure-rename λpin name)))

(define bitmap-pin-over  : Bitmap-Pin (make-pin 'bitmap-pin-over  'over))
(define bitmap-pin-under : Bitmap-Pin (make-pin 'bitmap-pin-under 'dest-over))

(define-combiner "bitmap-~a-append*" #:-> ([#:gapsize Real])
  #:with alignment bitmaps [#:gapsize [delta 0.0]]
  #:blend-mode [blend-mode CAIRO_OPERATOR_OVER]
  #:empty (bitmap-blank)
  #:short-path #:for base bmp #:if (zero? delta)
  ([(vl) (bitmap_pin blend-mode 0.0 1.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(vc) (bitmap_pin blend-mode 0.5 1.0 0.5 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(vr) (bitmap_pin blend-mode 1.0 1.0 1.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(ht) (bitmap_pin blend-mode 1.0 0.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(hc) (bitmap_pin blend-mode 1.0 0.5 0.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(hb) (bitmap_pin blend-mode 1.0 1.0 0.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))])
  #:do (let-values ([(base children) (values (car bitmaps) (cdr bitmaps))])
         (bitmap_append alignment blend-mode
                        (bitmap-surface base) (map bitmap-surface children)
                        (real->double-flonum delta) (bitmap-density base))))

(define-combiner "bitmap-~a-superimpose*" #:-> ()
  #:with alignment bitmaps []
  #:blend-mode [blend-mode (or (bitmap-operator->integer (default-pin-operator)) CAIRO_OPERATOR_OVER)]
  #:empty (bitmap-blank)
  #:short-path #:for base bmp #:if #true
  ([(lt) (bitmap_pin blend-mode 0.0 0.0 0.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(lc) (bitmap_pin blend-mode 0.0 0.5 0.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(lb) (bitmap_pin blend-mode 0.0 1.0 0.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(ct) (bitmap_pin blend-mode 0.5 0.0 0.5 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(cc) (bitmap_pin blend-mode 0.5 0.5 0.5 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(cb) (bitmap_pin blend-mode 0.5 1.0 0.5 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(rt) (bitmap_pin blend-mode 1.0 0.0 1.0 0.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(rc) (bitmap_pin blend-mode 1.0 0.5 1.0 0.5 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))]
   [(rb) (bitmap_pin blend-mode 1.0 1.0 1.0 1.0 (bitmap-surface base) (bitmap-surface bmp) (bitmap-density base))])
  #:do (bitmap_superimpose alignment blend-mode (map bitmap-surface bitmaps) (bitmap-density (car bitmaps))))

(define bitmap-vl-append : (-> [#:gapsize Real] Bitmap * Bitmap) (λ [#:gapsize [delta 0.0] . bitmaps] (bitmap-vl-append* #:gapsize delta bitmaps)))
(define bitmap-vc-append : (-> [#:gapsize Real] Bitmap * Bitmap) (λ [#:gapsize [delta 0.0] . bitmaps] (bitmap-vc-append* #:gapsize delta bitmaps)))
(define bitmap-vr-append : (-> [#:gapsize Real] Bitmap * Bitmap) (λ [#:gapsize [delta 0.0] . bitmaps] (bitmap-vr-append* #:gapsize delta bitmaps)))
(define bitmap-ht-append : (-> [#:gapsize Real] Bitmap * Bitmap) (λ [#:gapsize [delta 0.0] . bitmaps] (bitmap-ht-append* #:gapsize delta bitmaps)))
(define bitmap-hc-append : (-> [#:gapsize Real] Bitmap * Bitmap) (λ [#:gapsize [delta 0.0] . bitmaps] (bitmap-hc-append* #:gapsize delta bitmaps)))
(define bitmap-hb-append : (-> [#:gapsize Real] Bitmap * Bitmap) (λ [#:gapsize [delta 0.0] . bitmaps] (bitmap-hb-append* #:gapsize delta bitmaps)))

(define bitmap-lt-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-lt-superimpose* bitmaps)))
(define bitmap-ct-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-ct-superimpose* bitmaps)))
(define bitmap-rt-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-rt-superimpose* bitmaps)))
(define bitmap-lc-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-lc-superimpose* bitmaps)))
(define bitmap-cc-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-cc-superimpose* bitmaps)))
(define bitmap-rc-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-rc-superimpose* bitmaps)))
(define bitmap-lb-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-lb-superimpose* bitmaps)))
(define bitmap-cb-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-cb-superimpose* bitmaps)))
(define bitmap-rb-superimpose : (-> Bitmap * Bitmap) (λ bitmaps (bitmap-rb-superimpose* bitmaps)))
