#lang typed/racket

(provide (except-out (all-defined-out) make-pin make-append make-append* make-superimpose))

(require "digitama/digicore.rkt")
(require "digitama/combiner.rkt")
(require "constructor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-pin : (-> Symbol (->* (Bitmap Real Real Bitmap) (Real Real) Bitmap))
  (lambda [order]
    (λ [bmp1 x1 y1 bmp2 [x2 0] [y2 0]]
      (define dx : Integer (exact-round (- x1 x2)))
      (define dy : Integer (exact-round (- y1 y2)))
      (define dx1 : Natural (fxmax 0 (fx- 0 dx)))
      (define dy1 : Natural (fxmax 0 (fx- 0 dy)))
      (define dx2 : Natural (fxmax 0 dx))
      (define dy2 : Natural (fxmax 0 dy))
      (define dc : (Instance Bitmap-DC%)
        (make-object bitmap-dc%
          (bitmap-blank (fxmax (fx+ dx1 (send bmp1 get-width)) (fx+ dx2 (send bmp2 get-width)))
                        (fxmax (fx+ dy1 (send bmp1 get-height)) (fx+ dy2 (send bmp2 get-height))))))
      (send dc set-smoothing 'aligned)
      (case order
        [(over)  (send* dc (draw-bitmap bmp1 dx1 dy1) (draw-bitmap bmp2 dx2 dy2))]
        [(under) (send* dc (draw-bitmap bmp2 dx2 dy2) (draw-bitmap bmp1 dx1 dy1))])
      (or (send dc get-bitmap) (bitmap-blank)))))

(define make-append* : (-> Symbol (-> (Listof Bitmap) [#:gapsize Real] Bitmap))
  (lambda [alignment]
    (λ [bitmaps #:gapsize [delta 0.0]]
      (cond [(null? bitmaps) (bitmap-blank)]
            [(null? (cdr bitmaps)) (car bitmaps)]
            [else (let*-values ([(base others) (values (car bitmaps) (cdr bitmaps))]
                                [(min-width min-height) (values (send base get-width) (send base get-height))])
                    (define-values (width0 height0)
                      (for/fold ([width : Real min-width] [height : Real min-height])
                                ([child : Bitmap (in-list others)])
                        (define w : Positive-Integer (send child get-width))
                        (define h : Positive-Integer (send child get-height))
                        (case alignment
                          [(vl vc vr) (values (max width w) (+ height h delta))]
                          [(ht hc hb) (values (+ width w delta) (max height h))]
                          [else #|unreachable|# (values (max width w) (max height h))])))
                    
                    (define width : Positive-Real (max min-width width0))
                    (define height : Positive-Real (max min-height height0))
                    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
                    (send dc set-smoothing 'aligned)
                    
                    (let render : Void ([bmps : (Listof Bitmap) bitmaps]
                                               [xoff : Real (- delta)]
                                               [yoff : Real (- delta)])
                      (unless (null? bmps)
                        (define bmp : Bitmap (car bmps))
                        (define-values (w h) (values (send bmp get-width) (send bmp get-height)))
                        (define this-x-if-use : Real (+ xoff delta))
                        (define this-y-if-use : Real (+ yoff delta))
                        (define-values (x y)
                          (case alignment
                            [(vl) (values 0                 this-y-if-use)]
                            [(vc) (values (/ (- width w) 2) this-y-if-use)]
                            [(vr) (values (- width w)       this-y-if-use)]
                            [(ht) (values this-x-if-use     0)]
                            [(hc) (values this-x-if-use     (/ (- height h) 2))]
                            [(hb) (values this-x-if-use     (- height h))]
                            [else #|unreachable|# (values this-x-if-use this-y-if-use)]))
                        (send dc draw-bitmap bmp x y)
                        (render (cdr bmps) (+ this-x-if-use w) (+ this-y-if-use h))))
                    (or (send dc get-bitmap) (bitmap-blank)))]))))

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
                    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
                    (send dc set-smoothing 'aligned)
                    (for ([bmp+fxy (in-list (caddr info))])
                      (define-values (x y) ((cdr bmp+fxy) width height))
                      (send dc draw-bitmap (car bmp+fxy) x y))
                    (or (send dc get-bitmap) (bitmap-blank)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-combiner
  [make-pin         "bitmap-pin-~a"         (over under)]
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
        (values (bitmap-pin-over bmp x1 y1 bmp2) (max 0 x1) (max 0 y1))))
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
    (define gcols : (Vectorof Real) (list->n:vector col-gaps ncols 0.0))
    (define grows : (Vectorof Real) (list->n:vector row-gaps nrows 0.0))
    (define table-ref : (-> Integer Integer Bitmap-Cell)
      (let ([table : Bitmap-Tables (list->table bitmaps nrows ncols)])
        (λ [c r] (vector-ref (vector-ref table r) c))))
    (define pbcols : (Vectorof Pseudo-Bitmap*)
      (list->vector (nmap ncols (λ [[c : Integer]]
                                  (superimpose* (vector-ref alcols c)
                                                (nmap nrows (λ [[r : Integer]] (table-ref c r))))))))
    (define pbrows : (Vectorof Pseudo-Bitmap*)
      (list->vector (nmap nrows (λ [[r : Integer]]
                                  (superimpose* (vector-ref alrows r)
                                                (nmap ncols (λ [[c : Integer]] (table-ref c r))))))))
    (vector-set! gcols (sub1 ncols) 0.0)
    (unless (zero? nrows) (vector-set! grows (sub1 nrows) 0.0))
    (bitmap-vl-append*
     (nmap nrows
           (λ [[r : Integer]]
             (bitmap-ht-append*
              (nmap ncols
                    (λ [[c : Integer]]
                      (let* ([cell (table-ref c r)]
                             [pbc (vector-ref pbcols c)]
                             [pbr (vector-ref pbrows r)]
                             [w (+ (car pbc) (vector-ref gcols c))]
                             [h (+ (cadr pbr) (vector-ref grows r))])
                        (define-values (x _y) (find-xy cell pbc))
                        (define-values (_x y) (find-xy cell pbr))
                        (define cell.bmp (bitmap-blank w h))
                        (send* (send cell.bmp make-dc)
                          (set-smoothing 'aligned)
                          (draw-bitmap (car cell) x y))
                        cell.bmp)))))))))
