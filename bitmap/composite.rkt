#lang typed/racket

(provide (except-out (all-defined-out) make-append make-append* make-superimpose))

(require "digitama/digicore.rkt")
(require "digitama/composite.rkt")
(require "digitama/unsafe/source.rkt")
(require "digitama/unsafe/composite.rkt")
(require "constructor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require (only-in typed/racket/draw bitmap-dc% Bitmap-DC%))

(define bitmap-composite : (->* (Bitmap Real Real Bitmap) (Real Real #:mode Symbol) Bitmap)
  (lambda [bmp1 x1 y1 bmp2 [x2 0.0] [y2 0.0] #:mode [op 'over]]
    (bitmap_composite (or (bitmap-operator->integer op) (bitmap-operator->integer 'over))
                      (bitmap->surface bmp1) (real->double-flonum x1) (real->double-flonum y1)
                      (bitmap->surface bmp2) (real->double-flonum x2) (real->double-flonum y2)
                      (real->double-flonum (send bmp1 get-backing-scale)))))

(define bitmap-pin-over : (->* (Bitmap Real Real Bitmap) (Real Real) Bitmap)
  (lambda [bmp1 x1 y1 bmp2 [x2 0.0] [y2 0.0]]
    (bitmap-composite bmp1 x1 y1 bmp2 x2 y2 #:mode 'over)))

(define bitmap-pin-under : (->* (Bitmap Real Real Bitmap) (Real Real) Bitmap)
  (lambda [bmp1 x1 y1 bmp2 [x2 0.0] [y2 0.0]]
    (bitmap-composite bmp1 x1 y1 bmp2 x2 y2 #:mode 'dest-over)))

(define make-append* : (-> Symbol (-> (Listof Bitmap) [#:gapsize Real] Bitmap))
  (lambda [alignment]
    (λ [bitmaps #:gapsize [delta 0.0]]
      (cond [(null? bitmaps) (bitmap-blank)]
            [(null? (cdr bitmaps)) (car bitmaps)]
            [else (let*-values ([(base others gap) (values (car bitmaps) (cdr bitmaps) (real->double-flonum delta))]
                                [(min-width min-height) (values (fx->fl (send base get-width)) (fx->fl (send base get-height)))])
                    (define-values (width0 height0 sllec)
                      (for/fold ([width : Flonum min-width]
                                 [height : Flonum min-height]
                                 [cells : (Listof Bitmap-Cell) (list (list base min-width min-height))])
                                ([child : Bitmap (in-list others)])
                        (define w : Flonum (fx->fl (send child get-width)))
                        (define h : Flonum (fx->fl (send child get-height)))
                        (define cells++ : (Listof Bitmap-Cell) (cons (list child w h) cells))
                        (case alignment
                          [(vl vc vr) (values (flmax width w) (fl+ height (fl+ h gap)) cells++)]
                          [(ht hc hb) (values (fl+ width (fl+ w gap)) (flmax height h) cells++)]
                          [else #|unreachable|# (values (flmax width w) (flmax height h) cells++)])))
                    
                    (define width : Flonum (flmax min-width width0))
                    (define height : Flonum (flmax min-height height0))
                    (define bmp : Bitmap (bitmap-blank width height #:density 2.0))
                    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
                    (send dc set-smoothing 'aligned)
                    
                    (let render : Void ([cells : (Listof Bitmap-Cell) (reverse sllec)]
                                        [xoff : Flonum (fl- 0.0 gap)]
                                        [yoff : Flonum (fl- 0.0 gap)])
                      (unless (null? cells)
                        (define w : Flonum (cadar cells))
                        (define h : Flonum (caddar cells))
                        (define this-x-if-use : Flonum (fl+ xoff gap))
                        (define this-y-if-use : Flonum (fl+ yoff gap))
                        (define-values (x y)
                          (case alignment
                            [(vl) (values 0.0                     this-y-if-use)]
                            [(vc) (values (fl/ (fl- width w) 2.0) this-y-if-use)]
                            [(vr) (values (fl- width w)           this-y-if-use)]
                            [(ht) (values this-x-if-use           0.0)]
                            [(hc) (values this-x-if-use           (fl/ (fl- height h) 2.0))]
                            [(hb) (values this-x-if-use           (fl- height h))]
                            [else #|unreachable|# (values this-x-if-use this-y-if-use)]))
                        (send dc draw-bitmap (caar cells) x y)
                        (render (cdr cells) (fl+ this-x-if-use w) (fl+ this-y-if-use h))))
                    bmp)]))))

(define make-append : (-> Symbol (-> [#:gapsize Real] Bitmap * Bitmap))
  (lambda [alignment]
    (define append-apply : (-> (Listof Bitmap) [#:gapsize Real] Bitmap) (make-append* alignment))
    (λ [#:gapsize [delta 0.0] . bitmaps] (append-apply #:gapsize delta bitmaps))))

(define make-superimpose : (-> Symbol (-> Bitmap * Bitmap))
  (lambda [alignment]
    (λ bitmaps
      (cond [(null? bitmaps) (bitmap-blank)]
            [(null? (cdr bitmaps)) (car bitmaps)]
            [(null? (cddr bitmaps))
             (let-values ([(base bmp) (values (car bitmaps) (cadr bitmaps))])
               (case alignment
                 [(lt) (bitmap-pin  0   0   0   0  base bmp)]
                 [(lc) (bitmap-pin  0  1/2  0  1/2 base bmp)]
                 [(lb) (bitmap-pin  0   1   0   1  base bmp)]
                 [(ct) (bitmap-pin 1/2  0  1/2  0  base bmp)]
                 [(cc) (bitmap-pin 1/2 1/2 1/2 1/2 base bmp)]
                 [(cb) (bitmap-pin 1/2  1  1/2  1  base bmp)]
                 [(rt) (bitmap-pin  1   0   1   0  base bmp)]
                 [(rc) (bitmap-pin  1  1/2  1  1/2 base bmp)]
                 [(rb) (bitmap-pin  1   1   1   1  base bmp)]
                 [else base]))]
            [else (let ([info : Pseudo-Bitmap (superimpose alignment bitmaps)])
                    (define-values (width height) (values (car info) (cadr info)))
                    (define bmp : Bitmap (bitmap-blank width height #:density 2.0))
                    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
                    (send dc set-smoothing 'aligned)
                    (for ([bmp+fxy (in-list (caddr info))])
                      (define-values (x y) ((cdr bmp+fxy) width height))
                      (send dc draw-bitmap (car bmp+fxy) x y))
                    bmp)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-combiner
  [make-append      "bitmap-~a-append"      (vl vc vr ht hc hb)]
  [make-append*     "bitmap-~a-append*"     (vl vc vr ht hc hb)]
  [make-superimpose "bitmap-~a-superimpose" (lt lc lb ct cc cb rt rc rb)])

(define bitmap-pin : (-> Real Real Real Real Bitmap Bitmap * Bitmap)
  (lambda [x1% y1% x2% y2% bmp0 . bmps]
    (define-values (bmp _who _cares)
      (for/fold ([bmp : Bitmap  bmp0]
                 [x : Exact-Rational 0]
                 [y : Exact-Rational 0])
                ([bmp1 (in-list (cons bmp0 bmps))]
                 [bmp2 (in-list bmps)])
        (define-values (w1 h1) (values (send bmp1 get-width) (send bmp1 get-height)))
        (define-values (w2 h2) (values (send bmp2 get-width) (send bmp2 get-height)))
        (define x1 (+ x (- (inexact->exact (* x1% w1)) (inexact->exact (* x2% w2)))))
        (define y1 (+ y (- (inexact->exact (* y1% h1)) (inexact->exact (* y2% h2)))))
        (values (bitmap-composite bmp x1 y1 bmp2) (max 0 x1) (max 0 y1))))
    bmp))

(define bitmap-table : (->* (Positive-Integer (Listof Bitmap))
                            ((Listof Superimpose-Alignment)
                             (Listof Superimpose-Alignment)
                             (Listof Nonnegative-Real)
                             (Listof Nonnegative-Real))
                            Bitmap)
  (lambda [ncols bitmaps [col-aligns '(cc)] [row-aligns '(cc)] [col-gaps '(0)] [row-gaps '(0)]]
    (define-values (maybe-nrows extra-ncols) (quotient/remainder (length bitmaps) ncols))
    (define nrows : Natural (fx+ maybe-nrows (sgn extra-ncols)))
    (define alcols : (Vectorof Symbol) (list->n:vector col-aligns ncols 'cc))
    (define alrows : (Vectorof Symbol) (list->n:vector row-aligns nrows 'cc))
    (define gcols : (Vectorof Flonum) (list->n:vector (map real->double-flonum col-gaps) ncols 0.0))
    (define grows : (Vectorof Flonum) (list->n:vector (map real->double-flonum row-gaps) nrows 0.0))

    (define table-ref : (-> Integer Integer Bitmap-Cell)
      (let ([table : Bitmap-Tables (list->table bitmaps nrows ncols)])
        ;;; TODO: why (unsafe-vector-ref) makes it slower?
        (λ [c r] (vector-ref table (fx+ (fx* r ncols) c)))))
    (define pbcols : (Vectorof Pseudo-Bitmap*)
      (for/vector : (Vectorof Pseudo-Bitmap*) ([c (in-range ncols)])
        (superimpose* (vector-ref alcols c) (for/list ([r (in-range nrows)]) (table-ref c r)))))
    (define pbrows : (Vectorof Pseudo-Bitmap*)
      (for/vector : (Vectorof Pseudo-Bitmap*) ([r (in-range nrows)])
        (superimpose* (vector-ref alrows r) (for/list ([c (in-range ncols)]) (table-ref c r)))))
    
    (unless (zero? nrows)
      (vector-set! gcols (sub1 ncols) 0.0)
      (vector-set! grows (sub1 nrows) 0.0))

    (define-values (width height cells)
      (for/fold ([width : Flonum 0.0] [height : Flonum 0.0] [pbmps : (Listof (List Bitmap Flonum Flonum)) null])
                ([row : Integer (in-range nrows)])
        (define pbrow : Pseudo-Bitmap* (vector-ref pbrows row))
        (define hrow : Flonum (fl+ (cadr pbrow) (vector-ref grows row)))
        (define-values (wcols cells)
          (for/fold ([xoff : Flonum 0.0] [pbmps : (Listof (List Bitmap Flonum Flonum)) pbmps])
                    ([col : Integer (in-range ncols)])
            (define cell : Bitmap-Cell (table-ref col row))
            (define pbcol : Pseudo-Bitmap* (vector-ref pbcols col))
            (define wcol : Flonum (fl+ (car pbcol) (vector-ref gcols col)))
            (define-values (x _y) (find-xy cell pbcol))
            (define-values (_x y) (find-xy cell pbrow))
            (values (fl+ xoff wcol) (cons (list (car cell) (fl+ x xoff) (fl+ y height)) pbmps))))
        (values wcols (fl+ height hrow) cells)))

    (define bmp : Bitmap (bitmap-blank width height #:density 2.0))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    (for ([cell : Bitmap-Cell (in-list cells)])
      (send dc draw-bitmap (car cell) (cadr cell) (caddr cell)))
    bmp))
